
/* the generic proxy allows you to set up a listen server
   which connects to a specified machine/port when it
   receives connections on a particular port. Messages
   are then forwarded between the two machines without
   tampering. */

#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string>
#include "tsocket.h"


char * SERVER_IP   = 0;
int    SERVER_PORT = 0;

int    LISTEN_PORT = 0;

#define isprinting(c) (((c) > 32) && ((c) < 255))

int serversock = 0;

void clean(int);
void printbin(FILE *,const char *, const unsigned char * in, int len);
void hangup(int sig);
void ignoresigpipe(int bleh);
int  init();
void acceptsockets();

struct sockeh {
  sockeh(int a,sockeh*b) {
    sock = new tsocket(a);
    next=b;
    outsock = new tsocket();
    outsock->connectto(SERVER_IP, SERVER_PORT);
    packets = 0;
  }
  /*  sockeh(tsocket * s, sockeh*b) : sock(s), next(b) {} */
  long int packets;
 ~sockeh() { delete sock; }
  tsocket * sock;
  tsocket * outsock;
  sockeh  * next;
};

sockeh * PHEAD = NULL;

int init () {
  sockaddr_in serverAddr;

  signal(SIGHUP,  hangup);
  signal(SIGPIPE, ignoresigpipe);
 
  if ( (serversock = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
    printf("Couldn't open socket.");
    exit( -1 );
  }

  // set up socket
  memset(&serverAddr,sizeof(serverAddr),0);
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serverAddr.sin_port = htons(LISTEN_PORT);
  
  //No delay!
  fcntl(serversock, F_SETFL, FNDELAY);
  int timeout = 42;
  while (bind(serversock, (struct sockaddr*)&serverAddr, 
	      sizeof(serverAddr)) < 0) {
    printf(".");
    fflush(stdout);
    if (!timeout--) {
      printf("Unable to bind to %d...", LISTEN_PORT);
      exit(-1);
    }
    sleep(1);
  }
  printf("\n");

  if (listen(serversock, 15) < 0) {
    printf ("Unable to start listening. Is another process using this port?");
    exit(-1);
  }
  printf ("*** gproxy started (port %d) ***\n", LISTEN_PORT);
  return 0;
}

void acceptsockets() {
  int tmplen, tmpsock;
  sockaddr_in tmpaddr;
  if (-1 != (tmpsock = 
     accept(serversock, (struct sockaddr *)&tmpaddr,
	    (unsigned int*)&tmplen))) {
    
    // add player:
    PHEAD = new sockeh(tmpsock,PHEAD);

    printf(">> Connection from (%s) at %p\n",
	   PHEAD->sock->hostname().c_str(),
	   PHEAD);
    
  }
}

int main (int argc, char ** argv) {
  if (argc < 4) {
    printf("usage: %s remotehost(ip) remoteport localport\n",*argv);
    exit(-1);
  }
  
  SERVER_IP = argv[1];
  SERVER_PORT = atoi(argv[2]);
  LISTEN_PORT = atoi(argv[3]);

  init();
  
  for (;;) {
    usleep(100000); // We're not THAT important.
    acceptsockets();
    string a;
    for(sockeh ** tmp = &PHEAD;(*tmp);tmp=&(*tmp)->next) {
      if (!((*tmp)->sock->hello())) {
	/* this is invalid, delete. */
	printf(">> Socket %p has error code %d\n", (*tmp), (*tmp)->sock->err);
	printf("-- Disconnecting (%s) socket at %p\n",
	       (*tmp)->sock->hostname().c_str(), (*tmp));
	printf("   * packets sent: %ld\n", (*tmp)->packets);
	sockeh * old = *tmp;
	(*tmp) = (*tmp)->next;
	delete old;
	if (!*tmp) break;
      }

    }
    for(sockeh * tmp = PHEAD;tmp;tmp=tmp->next) {
      int smut;
      unsigned char inbuf[8192];
      smut = recv(tmp->sock->sock,inbuf,8191,0);      
      if (smut == -1) {
	switch (errno) {
	case EWOULDBLOCK: // means no string.
	  break;
	default:
	  tmp->sock->disconnect();
	  //	  tmp->sock->sock = 0;
	}
      } else if (smut) {

	/* write it to the server */
	tmp->outsock->godelay();
	tmp->outsock->writepacket((char*)&inbuf[0], smut);
	tmp->packets ++;
	tmp->outsock->gonodelay();
    
      }

      /* read from client, try reading from server: */
    
      smut = recv(tmp->outsock->sock,inbuf,8191,0);
      if (smut == -1) {
	switch (errno) {
	case EWOULDBLOCK: // means no string.
	  continue;
	default:
	  tmp->outsock->disconnect();
	  tmp->sock->disconnect();
	}
      } else if (smut) {

	/* write it to the server */
	tmp->sock->godelay();
	tmp->sock->writepacket((char*)&inbuf[0], smut);
	tmp->sock->gonodelay();
      }
      
    }
  }
}

void clean(int sig) {
  printf("Cleanup... outa here!\n");
  for(sockeh*tmp;PHEAD;PHEAD=tmp) {
    tmp = PHEAD->next;
    delete PHEAD;
  }
  close(serversock);
  if (sig) exit(-1); else exit (0);
}

void hangup (int /*sig*/) {

}

void ignoresigpipe(int ) {

  signal(SIGPIPE, ignoresigpipe);
}

void printbin(FILE * f, const char * name, const unsigned char * in, int len) {
  /* ML causes brain damage */

  fprintf(f,"%s:\n", name);
  
  for (;;) {
    if (len < 16) {
      
      for (int i=0;i < len; i++) {
	if (i==8)fprintf(f," ");
	fprintf(f,"%02X ", in[i]);
      }
      
      for(int i=0; i < (16-len); i++) {
	fprintf(f,"   ");
      }

      fprintf(f,"   %s", (len <= 8)?" ":"");
      for (int i=0; i < len; i++) {
	if (i==8)fprintf(f," ");
	if (isprinting(in[i])) fprintf(f,"%c",in[i]);
	else fprintf(f,".");
      }
      fprintf(f,"\n");
      return;
    } else {

      for(int i=0; i < 16; i++) {
	if (i==8)fprintf(f," ");
	fprintf(f,"%02X ", in[i]);
      }
      fprintf(f,"   ");
      for(int i=0; i < 16; i++) {
	if (i==8)fprintf(f," ");
	if (isprinting(in[i])) fprintf(f,"%c",in[i]);
	else fprintf(f,".");
      }
      
      fprintf(f,"\n");
      in += 16;
      len -= 16;
    }

  }
}
