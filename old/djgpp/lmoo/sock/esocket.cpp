#include <iostream.h>

#include "epoch.h"
//#include "tom.h"
#include "esocket.h"

#define EPIPE 104

#define DEVEL

#define DEVELIO

#define _WINDOWS

string esc_decode(const char * orig, int len);
string esc_encode(const char * data, int len);

int esocket::goNoDelay() {
#ifdef _WINDOWS
    long i=1;
    return ioctlsocket(sock, FIONBIO, i);
#else
    return fcntl(sock, F_SETFL, FNDELAY);
#endif
}

int esocket::getpacketlen() {
  return bytestoread;
}

string esocket::getpacket() {
  //int tmp = bytestoread;
  if (getpacketstatus() != ES_PACKET_READY)
    return (string) "";
  string returnme="";
//**************** ATTN: This is probably slow. ******************
  //for (int x=0;x<bytesread;x++) returnme+=buf[x];
  // could do = new string(buf,bytesread);
//****************************************************************
  returnme = esc_decode(buf,bytesread);
  delete[] buf;
  buf = NULL;
  bytesread = bytestoread = 0;
  status = ES_WAITPACKET;
  return returnme;
}
/*
string esocket::getpacket() {
  return esc_decode(getrawpacket());
}
*/
string binword(int num) {
  string s = "xx";
  s[0] = (char) (( num / 256 ) % 256);
  s[1] = (char) ( num % 256 );
  return s;
}

short wordbin(const unsigned char s1, const unsigned char s2) {
  return (short)((s1 * 256) + s2);
}

int hackbin(unsigned char c1, unsigned char c2) {
  return (int)(
       (((c1==255)?0:c1) * 255)
     + ( (c2==255)?0:c2));
}

int esocket::getpacketstatus() {

  int tmp;
  char sizey[2];
  if (sock==0)
    return ES_DISCONNECTED;

  if (status == ES_DISCONNECTED) return ES_DISCONNECTED;
  if (status == ES_PACKET_READY) return ES_PACKET_READY;

 // cout << "status: " << status << "/tBytes to read: " << bytesToRead << endl;

 // cin >> tmp;

  if (status == ES_WAITPACKET) {  //new packet
    int wpoop;
    wpoop = recv(sock,sizey,2,MSG_PEEK);
    if (wpoop == 2) {
    recv(sock, sizey, 2, 0);
      bytestoread = hackbin(sizey[0], sizey[1]);
//      cout << "(<<) Bytes to read: " << bytestoread << endl;
      if (bytestoread > 16384 || bytestoread <= 0) return ES_VERYBAD;
      if (buf) info ("Weird: buf was already alloc'd for new packet");
      buf = new char[bytestoread + 1];

      memset(buf,'*',bytestoread);

      status = ES_WAITDATA;
    } else if (wpoop == 1); // packet length not full yet. Good Thing We're Smart.
    else if (errno == EWOULDBLOCK)
      return ES_WAITPACKET;
    else if (errno == EPIPE || errno == 104)
      return status = ES_DISCONNECTED; // disconnected
    else{
    #ifdef DEVELIO
   	cout << "Unknown error code: "<< errno<<endl;
	perror("send");
        fatal("note me.");
    #endif
      return ES_DUNNO;
    }
  }

  if ( 0 != bytestoread && bytesread == bytestoread)
    return (status = ES_PACKET_READY);

  if (0 < (tmp=recv(sock, buf + (bytesread), bytestoread - bytesread, 0))) {
    //buf[bytesread += tmp] = 0;  // works better
    bytesread +=tmp;
  }
  else if (errno == EWOULDBLOCK)
    return ES_WAITDATA;
  else if (errno == EPIPE || errno == 104)
    return status = ES_DISCONNECTED;
  else {
#ifdef DEVELIO
    cout << "Unknown at recv: " << errno << endl;
    perror("recv: ");
    fatal("note me.");
#endif
    return ES_DUNNO;

   }
  return (bytestoread - bytesread) ? (status = ES_PACKET_READY) : ES_WAITDATA;
}

int esocket::sendmsg(const char *b) {
  if (status == ES_NOTCONNECTED) return 0;
  if (writePacket(b, strlen(b))) return 1;
  else return 0;
}

int esocket::sendmsg(string b) {
  if (status == ES_NOTCONNECTED) return 0;
  if (writePacket(b.c_str(), b.length())) return 1;
  else return 0;
}

int esocket::writePacket(const char *b, int len) {
#ifdef DEVELIO
  cout << "<< " << cesc(b,len) << "\r\n";
#endif
  if (!sock)
    return 0;
//  info ("About to send()");
  string z = esc_encode(b,len);
//  if ((-1 == send(sock,hackbinword(z.length()).c_str(), 2, 0))
//  || (-1 == send(sock,z.c_str(),z.length(),0)))
    if (-1 ==
send(sock,(hackbinword(z.length())+z).c_str(),2+z.length(),0))
{
#ifdef DEVEL

    switch (errno) {
    case EWOULDBLOCK: cout << "EWOULDBLOCK\n"; break;
    case EBADF: cout << "EBADF\n"; break;
#ifndef _WINDOWS
    case ENOBUFS: cout << "ENOBUFS\n"; break;
    case ENOTSOCK: cout << "ENOTSOCK\n"; break;
#endif
    case EFAULT: cout << "EFAULT\n"; break;
    case EPIPE: cout << "EPIPE\n"; break;
    default: cout << "EIDON'TKNOW\n";
    }
    info ("writepacket failed");
#endif
    return 0;
  } else
//  info ("just sendsend() not -1");
  return 1;
 // return (send(sock, b, len, 0) == -1)?info("writepacket2 failed"),1:1;
}

string hackbinword(int num) {
  char cee[3];
  cee[2] = 0;
  cee[0] = num/255;
  cee[1] = num%255;
  cee[0] = (cee[0])?cee[0]:255;
  cee[1] = (cee[1])?cee[1]:255;
  return cee;
}

esocket::esocket() {
  sock = 0;
  bytesread = bytestoread = 0;
  status = ES_NOTCONNECTED;
  buf = NULL;
}

esocket::esocket(int s) {
  bytesread = bytestoread = 0;
  buf = NULL;
  status = ES_WAITPACKET;
  sock = s;
  goNoDelay();
}

esocket::esocket(char *addr, int port) {
  bytesread = bytestoread = 0;
  buf = NULL;
  status = ES_NOTCONNECTED;
  connectTo(addr, port);
}

esocket::~esocket() {
  if ( sock > 0 )
#ifdef _WINDOWS
    _close(sock);
#else
    close(sock);
#endif
  if ( buf != NULL )
    delete[] buf;
}

int esocket::closesock() {
  int t = sock > 0 ? close(sock) : 0;
  sock = 0;
  status = ES_NOTCONNECTED;
  return t;
}

int esocket::connectTo(char *addr, int port) {
  sockaddr_in serverAddr, clientAddr;

  ZERO(serverAddr);
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = inet_addr(addr);
  serverAddr.sin_port = htons(port);
  //cout << "Making Sock: " << addr << ":" << port << "\n";
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    return sock;
  //cout << "Connecting\n";
  if (connect(sock, (sockaddr *) &serverAddr, sizeof(serverAddr))<0)
    return -1;
  goNoDelay();
  status = ES_WAITPACKET;
  return sock;
}

string esc_decode(const char * orig, int len) {

int x;
string poopy;

	for (x=0;x<len;x++) {
		if (orig[x] == '\xFE'){
		  if (x==(len-1)) return ""; // which triggers VERYBAD?
            poopy += orig[++x];
        } else
            poopy += (orig[x]=='\xFF')?'\0':orig[x];
    }
return poopy;
}

string esc_encode(const char * data, int len) {
unsigned char a;
int x;
string poopy;
  for(x=0;x<len;x++)
     if (a=data[x]) {
        if ((a | 1) == '\xFF')   // either 255 or 254
           poopy += '\xFE';      // escape it
	poopy += a;              // add character
      } else poopy += '\xFF'; // 0 -> 255 always.

return poopy;
}
