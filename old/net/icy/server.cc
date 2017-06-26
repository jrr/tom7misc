#include <iostream.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string>

#define VERBOSE_DEVEL

#include "linkbank.h"

#include "tsocket.h"
#include "tom.h"
#include "icy.h"

void ignoresigpipe(int bleh);

tsocket * makeserversock(int port) {
  sockaddr_in serverAddr;
  int serverSock;
  
  if ( (serverSock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    return 0;

  // set up socket
  memset(&serverAddr,sizeof(serverAddr),0);
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serverAddr.sin_port = htons(port);
  
  //No delay!
  fcntl(serverSock, F_SETFL, FNDELAY);
  int timeout = 42;
  while (bind(serverSock, (struct sockaddr*)&serverAddr, 
	      sizeof(serverAddr)) < 0) {
    cout << '.' << flush;
    if (!timeout--) return 0;
    sleep(1);
  }
  cout << iendl;
  if (listen(serverSock, 15) < 0)
    return 0;

  return new tsocket(serverSock);
}

extern linkbank * froodsays;
extern string current_topic;
extern string infostring;
extern string lastsaid;

void do_serverstuff(tsocket * svr) {
  tsocket * poop;
  if (infostring == "") infostring = froodsays->info(15);
  while ((poop = svr->getconn())) {
    poop->sendmsg((string)"<p><b>&lt;Frood&gt;</b> "+froodsays->construct());
    poop->sendmsg("\n<p><p>Here's info on the database:<p><font size=-1>");
    poop->sendmsg(infostring + (string)"</font><p>\n");
    /* Commented out because they're always so dirty these days! */
    /*
    poop->sendmsg((string)"The current topic on #42 is: <b>" 
		  +current_topic + string("</b>\n")); 
    */
    poop->sendmsg((string)"<p>The last thing I really said is: "
		  "<b>&lt;Frood&gt;</b> " + lastsaid + (string)"<p>\n");
    poop->sendmsg("[END]\n");
    info("Server serviced request");
    delete poop;
  }
}

void ignoresigpipe(int ) {
  //  info ("Got SIGPIPE, but ignoring it...\n");
  signal(SIGPIPE, ignoresigpipe);
}
