#include <iostream.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string>

#include "tsocket.h"
#include "tom.h"

#define DEFAULT_PORT 0x4242
/* #define HOST "128.2.74.129" */

//************************//
#define DEVEL
//************************//

char * iendl = "\n";

void hangup(int sig);
void ignoresigpipe(int bleh);
int  init(int);
void acceptsockets();

int serverSock;

struct sockeh {
  sockeh(int a,sockeh*b) {sock = new tsocket(a);next=b;}
  sockeh(tsocket * s, sockeh*b) : sock(s), next(b) {}
 ~sockeh() { delete sock; }
  tsocket * sock;
  sockeh *   next;
};

sockeh * PHEAD = NULL;

int init (int pp) {
  sockaddr_in serverAddr;

  signal(SIGINT,  clean);
  signal(SIGHUP,  hangup);
  signal(SIGQUIT, clean);
  signal(SIGPIPE, ignoresigpipe);
 
  if ( (serverSock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    fatal("Couldn't open socket."); 

  // set up socket
  memset(&serverAddr,sizeof(serverAddr),0);
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serverAddr.sin_port = htons(pp);
  
  //No delay!
  fcntl(serverSock, F_SETFL, FNDELAY);
  int timeout = 42;
  while (bind(serverSock, (struct sockaddr*)&serverAddr, sizeof(serverAddr)) < 0) {
    cout << '.' << flush;
    if (!timeout--) fatal("Unable to bind to address...");
    sleep(1);
  }
  cout << endl;

  if (listen(serverSock, 15) < 0)
    fatal ("Unable to start listening. Check process listing?");
  printf ("*** echod started (port %d) ***\n", pp);
  return 0;
}

void acceptsockets() {
  int tmplen, tmpsock;
  sockaddr_in tmpaddr;
  if (-1 != (tmpsock = 
     accept(serverSock, (struct sockaddr *)&tmpaddr,
	    (unsigned int*)&tmplen))) {
    
    // add player:
    PHEAD = new sockeh(tmpsock,PHEAD);

    printf(">> Connection from (%s) at %p\n",
	   PHEAD->sock->hostname().c_str(),
	   PHEAD);
    
  }
}

int main (int argc, char**argv) {
  int pt = DEFAULT_PORT;

  if (argc == 2) {
    pt = atoi(argv[1]);
  } else if (argc > 2) {
    printf("Only one argument, the port to listen on...\n");
    exit(-1);
  }

  init(pt);
  
  for (;;) {
    usleep(1000); 
    /*    sleep(1);*/
    acceptsockets();
    string a;
    for(sockeh ** tmp = &PHEAD;(*tmp);tmp=&(*tmp)->next) {
      if (!(*tmp)->sock->valid()) {
	/* this is invalid, delete. */

	printf(">> Socket %p has error code %d\n", (*tmp), (*tmp)->sock->err);
	printf("-- Disconnecting (%s) socket at %p\n",(*tmp)->sock->hostname().c_str(), (*tmp));
	//	printf("-- (*tmp)->next = %p\n", (*tmp)->next);
	sockeh * old = *tmp;
	(*tmp) = (*tmp)->next;
	delete old;
	if (!*tmp) break;
      }
    }
    for(sockeh * tmp = PHEAD;tmp;tmp=tmp->next) {
      // read...
      if ("" != (a = tmp->sock->getpacket())) {
	// this person sent something:
	//	printf("[%p]: %s\n", tmp, a.c_str());
	a += '\n';
	for (sockeh * memp = PHEAD;memp;memp=memp->next) {
	  if (tmp != memp && memp->sock->valid()) {
	    // don't echo to the sender.
	    memp->sock->sendmsg(a);
	  }
	}
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
  close(serverSock);
  if (sig) exit(-1); else exit (0);
}

void hangup (int /*sig*/) {
  info ("Got SIGHUP. Ignoring it...\n");
}

void ignoresigpipe(int ) {
  info ("Got SIGPIPE, but ignoring it...\n");
  signal(SIGPIPE, ignoresigpipe);

}
