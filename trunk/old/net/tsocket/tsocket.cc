#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include "tsocket.h"

#ifdef DEVEL
extern char * iendl;
#endif

using namespace std;

// #define VERBOSE_DEVEL

extern char id_tsocket[];
char id_tsocket[] = "$Id: tsocket.cc,v 1.3 2005/02/02 23:44:54 tom7 Exp $";

string tsocket::hostname() {
  if (!sock) return "(disconnected)";

  return _hostname;
}

int tsocket::valid() {
  return !!sock;
}

int tsocket::hello() {
  if (!sock) return 0;
  char silly;
  int smut = recv(sock,&silly,1,MSG_PEEK);
  if (smut < 0) {
    switch (errno) {
    case EWOULDBLOCK: // means no string.
      return 1;
    default:
      disconnect();
    }
  } else {
    /*    printf("smut was %d for socket %d\n", smut, sock); */
  }
  return 1;
}

int tsocket::validate() {
  assert ((!QTAIL) || (!QTAIL->next));

  printf("***  Verifying socket validity:\n");

  printf("     sock : %d\n",sock);
  printf("     QUEUE:\n");
  for(qitem * s = QHEAD;s; s = s -> next) {
    printf("        -> %p (%s)\n", s, s->data.c_str());
  }
  
  printf ("     QHEAD : %p\n     QTAIL : %p\n", QHEAD, QTAIL);
  
  return !!sock;
}


string tsocket::getpacket() {
  if (!sock) { return ""; }
  static char inbuf[1024];
  int smut;

  if (QHEAD)
    return getfromreturnqueue();

  smut = recv(sock,inbuf,1023,0);
  if (smut == -1) {
    switch (errno) {
    case EWOULDBLOCK: // means no string.
      return "";
      break;
    default:
      //      cout << "(!) Some reading error. Disconnecting.\n";
      disconnect();
    }
  }
  // got some data. null-terminate it.
  inbuf[smut]=0;
  for (int x=0; x < smut; x++) {
    if (inbuf[x] == '\n') {
      if (saveme[saveme.length()-1] == '\r') 
	addtoreturnqueue(saveme.substr(0,saveme.length()-1));
      else
	addtoreturnqueue(saveme);
      saveme = "";
    } else saveme += inbuf[x];
  }
  return getfromreturnqueue();
}
 
string tsocket::getfromreturnqueue() {
  if (!QHEAD) return "";
  assert((!QTAIL) || (QTAIL->next == 0));
  if (QHEAD == QTAIL) {
    // one element:
    string z = QHEAD->data;
    delete QHEAD;
    QHEAD=QTAIL=NULL;
    assert((!QTAIL) || (QTAIL->next == 0));
    return z;
  } else {
    qitem * z = QHEAD;
    string stuff = z->data;
    QHEAD = QHEAD->next;
    delete z;
    assert((!QTAIL) || (QTAIL->next == 0));
    return stuff;
  }
}

void tsocket::addtoreturnqueue(string addme) {
  if (!QTAIL) {
    QTAIL = QHEAD = new qitem(0,addme); 
  } else {
    QTAIL->next = new qitem(0,addme);
    QTAIL = QTAIL->next;
  }
  assert((!QTAIL) || (QTAIL->next == 0));
}
void tsocket::sendmsg(string snot) {
#ifdef VERBOSE_DEVEL
  printf("WRITING[%s]\n", snot.c_str());
#endif
  if (! writepacket(snot.c_str(),snot.length()) ) {
    //    cout << "(!) Couldn't write packet. Disconnecting.\n";
    disconnect();
  }
}

int tsocket::writepacket(const char *b, int len) {
#ifdef DEVELIO
  cout << "<< " << cesc(b,len) << iendl;
#endif

  if (!sock)
    return 0;
//  info ("About to send()");
   
  if ( (-1 == send(sock,b,len,0))) {
#ifdef DEVEL    
    switch (errno) {
    case EWOULDBLOCK: cout << "EWOULDBLOCK\n"; break;
    case EBADF: cout << "EBADF\n"; break;
    case ENOBUFS: cout << "ENOBUFS\n"; break;
    case EFAULT: cout << "EFAULT\n"; break;
    case ENOTSOCK: cout << "ENOTSOCK\n"; break;
    case EPIPE: cout << "EPIPE\n"; break;
    default: cout << "EIDON'TKNOW\n"; 
    }
    info ("writepacket failed");
#endif
    return 0;
  } else return 1;
}

tsocket::tsocket() {sock = 0; QHEAD=QTAIL=0; err=0;}
tsocket::tsocket(int snum) { 
  sock = snum; err=0;
  QHEAD=QTAIL=0;
  gonodelay();
}

tsocket * tsocket::getconn() {
  int tmplen, tmpsock;
  sockaddr_in tmpaddr;
  if (-1 != (tmpsock = 
	     accept(sock, (struct sockaddr *)&tmpaddr, (unsigned int*)&tmplen))) {
    
    string outadr;
    _hostname = "";
    hostent * pooper;
    if ((pooper = (hostent*)gethostbyaddr((char*)&tmpaddr.sin_addr,4,
					  AF_INET))) {

      _hostname = (string) pooper->h_name;

    } 
    if (_hostname == "") {
      int aa = ntohl(tmpaddr.sin_addr.s_addr);
      
      char addy[64];
      
      sprintf(addy,"%d.%d.%d.%d", aa&255, (aa>>=8)&255, (aa>>=8)&255,
	      (aa>>=8)&255);
      
      _hostname = (string)addy;
    }

    // add player:
    return new tsocket(tmpsock);
  }
  else return 0; /* no new socket */
}


tsocket::~tsocket() {
  if (sock) close(sock);
  while (QHEAD) {
    qitem * s = QHEAD;
    QHEAD = QHEAD->next;
    delete s;
  }
}

int tsocket::disconnect() {
  int t = (sock > 0) ? close(sock) : 0;
  sock = 0;
  _hostname = "";
  return t;
}

int tsocket::connectto(string where, int port) {
  sockaddr_in serverAddr /*, clientAddr*/;
 
 
  const char * addr = where.c_str();

  if (sock) disconnect();

  memset(&serverAddr, 0, sizeof (sockaddr_in));

  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = inet_addr(addr);
  serverAddr.sin_port = htons(port);
  //  cout << "Making Sock: " << addr << ":" << port << "\n";
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) 
    return sock;
  //  cout << "Connecting\n";
  if (connect(sock, (sockaddr *) &serverAddr, sizeof(serverAddr))<0)
    return -1;

  //  cout << "I appear to have connected.\n";
  gonodelay();
  return sock;
}
