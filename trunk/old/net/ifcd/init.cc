/* Copyright (c) 1999 Tom 7 - See COPYING for license */

#include "ifc.h"

extern char id_init[];
char id_init[] = "$Id: init.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

void hangup(int sig);
void ignoresigpipe(int bleh);

int init () {

  srandom(time(0));

  sockaddr_in serverAddr;

  USERS = new list<user> * [ HASHSIZE ];
  CHANNELS = new list<channel> * [ HASHSIZE ];

  PENDING = 0;

  for (int x=0;x<HASHSIZE;x++) {
    USERS[x] = 0;
    CHANNELS[x] = 0;
  }

  signal(SIGINT,  clean);
  signal(SIGHUP,  hangup);
  signal(SIGQUIT, clean);
  signal(SIGPIPE, ignoresigpipe);
 
  if ( (serversock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    fatal("Couldn't open socket."); 

  // set up socket
  memset(&serverAddr,sizeof(serverAddr),0);
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serverAddr.sin_port = htons(DEFAULT_PORT);
  
  //No delay!
  fcntl(serversock, F_SETFL, FNDELAY);
  int timeout = 256;
  while (bind(serversock, (struct sockaddr*)&serverAddr, sizeof(serverAddr)) < 0) {
    cout << '.' << flush;
    if (!timeout--) { 
      cout << endl;
      fatal("Unable to bind to address...");
    }
    sleep(2);
  }
  cout << endl;

  if (listen(serversock, 15) < 0)
    fatal ("Unable to start listening. Check process listing?");
  printf ("*** ifcd started ***\n");


  setuid(1000);

  return 0;
}

void hangup (int /*sig*/) {
  info ("Got SIGHUP. Ignoring it...\n");
  signal(SIGHUP, hangup);
}

void ignoresigpipe(int ) {
  info ("Got SIGPIPE, but ignoring it...\n");
  signal(SIGPIPE, ignoresigpipe);

}
