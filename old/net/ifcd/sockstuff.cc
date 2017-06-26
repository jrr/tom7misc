
#include "ifc.h"
#include <netdb.h>

extern char id_sockstuff[];
char id_sockstuff[] = 
    "$Id: sockstuff.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

void acceptsockets() {
  int tmplen=sizeof(sockaddr_in), tmpsock;
  sockaddr_in tmpaddr;
  if (-1 != (tmpsock = 
	     accept(serversock, 
		    (struct sockaddr *)&tmpaddr,
		    (unsigned int*)&tmplen))) {
    
    string outadr;

    hostent * pooper;
    if ((pooper = (hostent*)gethostbyaddr((char*)&tmpaddr.sin_addr,4,AF_INET)) 
	&& strchr(pooper->h_name,'.')) {
      
      /* if we could resolve the hostname *and* it has a period in it
	 (for unforgiving irc clients), use this name */
      outadr = lowercase((string) pooper->h_name);
      
    } else {
      /* otherwise use the IP address */

      int aa = ntohl(tmpaddr.sin_addr.s_addr);
      
      char addy[64];
      
      sprintf(addy,"%d.%d.%d.%d", (aa>>24)&255, (aa>>16)&255, (aa>>8)&255,
	      aa&255);
      
      outadr = (string)addy;

    }

    printf(" <-- New connection (%s)\n", outadr.c_str());

    addpending(tmpsock, outadr);
    
    /*    printf(" Survived that add..\nNow PENDING is: %p", PENDING); */
    
  }
}

char buff[512];

void serverloop() {
  static int usrping=0;
  static int pendingcycle=5000;
  static int timecycle=10, nowtime=time(0);

  for (;;) {
    usleep(150000); // We're not THAT important.
    acceptsockets();

    if (! (timecycle--)) { 
      nowtime = time(0); 
      timecycle = 10; 

      /*      printf ("*>         I'm a robot @ %d\n", nowtime); */
      for (int x=0; x < HASHSIZE; x++)
	for (list<channel> * c = CHANNELS[x] ; c ; c = c -> next)
	  if (c->data->modeflags & C_WANTTIMER)
	    c->data->timer(nowtime);
    }

    /* pings */

    for(list<user> * uh = USERS[usrping] ; uh ; uh = uh -> next) {

      /*
      printf("Nowtime: %d  .. Pingtime: %d\n", nowtime, 
	     uh->data-> pingtime);
      */
      
      if (uh -> data -> pingtime && uh -> data -> pingtime < nowtime) {
	/* cycled again without a pong in long enough. Disconnect. */

	printf("****** Ping timeout for %s!%s\n", uh->data->nick.c_str(),
	       uh->data->hostname.c_str());
	
	uh -> data -> sock -> disconnect();

      }
      uh -> data -> sendf ("PING :%s", SERVERNAME);

      uh -> data -> pingtime = nowtime + 120; /* 2 min timeout */

      /*
      printf("(now) Nowtime: %d  .. Pingtime: %d\n", nowtime, 
	     uh->data-> pingtime);
      */

    }
    

    if (++usrping >= HASHSIZE) usrping=0;

    if (--pendingcycle < 0) {
      
      for (list<pending> * ph = PENDING ; ph ; ph = ph -> next) {

	if (ph -> data -> pingtime && ph -> data -> pingtime < nowtime) {
	  /* cycled again without a pong in long enough. Disconnect. */

	  printf("****** Ping timeout for (pending) %s!%s\n", 
		 ph->data->nick.c_str(),
		 ph->data->hostname.c_str());

	  ph -> data -> sock -> disconnect();

	}


	int x = snprintf(buff, 1000, 
			 "PING :%s\r\n",
			 SERVERNAME);

	if (! ph->data->sock->writepacket(buff,x)) {
	  ph->data->sock -> disconnect ();
	  break;
	}
	
	ph -> data -> pingtime = nowtime + 30;

      }
      
      pendingcycle = 5000;
    }

    /* traverse hash table */

    string s;

    ///    printf("...");

    for(list<pending> * ptmp = PENDING; ptmp; ) {
      
      if (!ptmp->data->sock->valid()) {

	// remove sock

	// ?????????

	printf("Disconnecting pending connection.\n");
	list<pending> * pt = ptmp->next;
	delete ptmp->data->sock;
	ptmp->remove(PENDING);
	ptmp = pt;
	continue;
      }

      assert(ptmp && ptmp->data && ptmp->data->sock);

      if ("" != (s = ptmp->data->sock->getpacket())) {
	printf("(entering pendingparse)\n");
	list<pending> * o = ptmp;
	pendingparse(ptmp, s);
	printf("(leaving pendingparse with (%p,%p))\n", o,ptmp);
	if (o != ptmp) continue;
      }
      
      ptmp = ptmp->next; /* skip on 'continue' */

    }

    for(int x = 0; x < HASHSIZE; x ++) {
      
      for (list<user> * utmp = USERS[x]; utmp; ) {

	//printf("Checking user at %p\n", utmp);
	
	assert(utmp && utmp->data && utmp->data->sock);

	if (!utmp->data->sock->valid()) {
	  
	  // remove sock
	  
	  // ?????????
	  
	  printf("Disconnecting user connection %s!%s.\n",
		 utmp->data->nick.c_str(),
		 utmp->data->hostname.c_str());
	  /* remove from channels */

	  
	  list<user> * pt = utmp->next;

	  deluser(utmp,"[socket invalid]");

	  utmp = pt;
	  continue;
	} //else printf("(don't close)\n");
	
	if ("" != (s = utmp->data->sock->getpacket())) {
	  //	  printf("(entering userparse)\n");
	  list<user> * o = utmp;
	  userparse(utmp, s);
	  //	  printf("(leaving userparse with (%p,%p))\n", o,utmp);
	  if (o != utmp) continue;
	} //else printf("(don't read)\n");
      
	utmp = utmp -> next; 
      }

    }

  }
}
