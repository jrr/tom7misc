// [EPoCH]=-------------------------------------------------------------------
//                   SERVER - The main() et al. for the Epoch Server
// [server.cpp]=--------------------------------------------------------------

#include <iostream.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string>

#include "epoch.h"
#include "tom.h"
#include "parse.h"	
#include "server.h"
#include "buildnum.h"

#define DEFAULT_PORT 4242
#define SERVER_NAME "128.2.94.17"

#define PING_MODULUS 4
#define PING_SECONDS 9

//************************//
#define DEVEL
void dotests();
//************************//

void cleanup(int sig);
void hangup(int sig);
void init();
void pings(void);
void acceptsockets();
void startshutdown(int sig);
void checkshutdown();
void checkgamereq();
inline int playeridx();

eplayer   * player  [MAX_CLIENTS] = { 0 };
epochgame * game    [MAX_GAMES]   = { 0 };
gamerec   * grequest[MAX_GAMEREC] = { 0 };

bool shuttingdown = false;
double shutdowntime;
unsigned int shutdowntimeleft = 180;  //shutdown time in seconds
int serverSock;

void checkgamereq() {
  long n = time(NULL);
  for (int t=0;t<MAX_GAMEREC;t++)
      if(grequest[t]) {
        if (n >= grequest[t]->timeout) {
           delete grequest[t]->game;
           delete grequest[t];
           grequest[t]=0;
        }
      }
}

void dotests() {
/*
  player[1] = new eplayer;
  player[3] = new eplayer;
  player[15] = new eplayer;
  info ("Created players..");
  game[1] = new epochgame;
  info ("Created epochgame..");
  game[1]->addspectator(3);
  game[1]->addspectator(15);
  game[1]->addspectator(1);
  info("Added spectators..");
  game[1]->dropspectator(0); // not in list
  game[1]->dropspectator(3); // in list
  info("Done testin..");
*/
}

void pings () {
//return;
  static int lastmod =0;
  static double lasttime = 0;
  if (time(0) > lasttime) {
     lasttime = time(0) + PING_SECONDS; // every 10 seconds
     lastmod = (lastmod + 1) % PING_MODULUS;
     for (int idx=1;idx<MAX_CLIENTS;idx++)
        if ( ( (idx%PING_MODULUS) == lastmod) 
            && player[idx]) sendcommand(idx,"-");
     } 
}

void checkshutdown() {
  if (!shuttingdown) return;
   if (time(0) > shutdowntime) cleanup(0);
}

void init () {
  for (int i=24;i--;) cout << endl;
  info("Initializing Epoch Server");

cout << "[[[[[[[[[[[[ Epoch Server "
     <<MAJORVERSION<<"."
     <<MINORVERSION<<"."
     <<REVISION    <<" build "
     <<_WM_BUILDNUM<<" ]]]]]]]]]]]]\n\n\n";

  sockaddr_in serverAddr;

  signal(SIGINT,  cleanup);
  signal(SIGHUP,  hangup);
  signal(SIGQUIT, startshutdown);
 
  if ( (serverSock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    fatal("Couldn't open socket."); 

  // set up socket
  ZERO(serverAddr);
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serverAddr.sin_port = htons(DEFAULT_PORT);
  
  //No delay!
  fcntl(serverSock, F_SETFL, FNDELAY);
  printf ("Binding address...");
  int timeout = 42;
  while (bind(serverSock, (struct sockaddr*)&serverAddr, sizeof(serverAddr)) < 0) {
    cout << '.' << flush;
    if (!timeout--) {cout <<endl; fatal("Unable to bind to address...");}
    sleep(1);
  }
  cout << endl;

  if (listen(serverSock, 15) < 0)
    fatal ("Unable to start listening. Check process listing?");

  info ("Listening.");

#ifdef DEVEL

#endif

}

void acceptsockets() {
  int tmp=0, tmpLen, tmpSock;
  sockaddr_in tmpAddr;
   if (-1 != (tmpSock = accept(serverSock, (struct sockaddr *)&tmpAddr, &tmpLen) ) ) {
      if (shuttingdown) {
        info ("Socket connection refused: server shutting down.");
	send(tmpSock, "NL\x06", 3, 0);
	close(tmpSock);
      }
      else if (tmp=playeridx()) {  
	// add their asses.
	info ("Socket connection.");
	player[tmp] = new eplayer(tmp, tmp);
	player[tmp]->sock = new esocket(tmpSock);
      }
      else {
	info ("Socket connection refused: server full (!)");
	send(tmpSock, "NL\x03", 3, 0);
	close(tmpSock);
      }
    }
}

inline int playeridx() {
  int i;
  for (i=1;i<MAX_CLIENTS;i++)
     if (!player[i]) return i;
  return 0;
}

int main () {
   int i, j, tmp;
   char *buf = NULL;
   int buflen;
   long oldtime = 0, newtime;
   init();
 
 // game[0] = new game0server(player);

  while (1) {
    usleep(10); // We're not THAT important.
    if (oldtime <= (newtime = time(NULL))) {
	    checkshutdown();
	    pings();
	    checkgamereq(); 
            oldtime = newtime + 1;
    }
    acceptsockets();
    for (i=0; i<MAX_CLIENTS; i++) {

      if (player[i] && player[i]->sock) {
	switch (tmp = player[i]->sock->getpacketstatus())  {
	case ES_VERYBAD:
     	   cout << "[!!] Player " << i << " sent verybad packet.\n";
	/*FALLTHRU*/
	case ES_DISCONNECTED:  
	   disconnect(i);
	   break;
	case ES_PACKET_READY:  // Packet ready!
           parsecommand(player[i]->sock->getpacket(), i);
	   break;
	case ES_WAITDATA:
        case ES_WAITPACKET:
	break;
	default:
           cout << "Socket returned unknown code " << tmp << ".\n";
	   disconnect(i);
	   break;
	}
      }
      // don't assume we're still connected!
    }
  }
// ------------------

}

void cleanup(int sig) {

  info ("Cleaning up...");

  for (int i=0; i<MAX_CLIENTS; i++)
    if (player[i] && player[i]->sock) {
       cout << "Close #" << i << "... \n";
       player[i]->sock->sendmsg("X"); // ignore errors.
}
//  sleep(1);
  close(serverSock);
  if (sig) exit(-1); else exit (0);
}

void startshutdown(int sig) {
  char msg[256];
  sprintf(msg, "xServer going down in %d seconds...  Sorry!", shutdowntimeleft);
  
  shuttingdown = true;

  for (int i=0; i<MAX_CLIENTS; i++)
    if (player[i] != NULL && player[i]->sock != NULL) {
      cout << "Sending imminent shutdown to " << i << endl;
      sendcommand(i,msg);
    }
  shutdowntime = time(0) +shutdowntimeleft;
}

void hangup (int sig) {
     info ("I thought I was nohup... Not exiting!\n");
//   fatal ("Got SIGHUP -- normally would start shutdown.");
}

void disconnect(int idx) {
   player[idx]->sock->closesock();
/* REMOVE THEM FROM GAMES */ /* OR NOT? */   
   delete player[idx];
   player[idx] = NULL;
   cout << "[!!] Just disconnected " << idx << ".\n";
}

void sendcommand(int idx, string msg) {
    if (!player[idx]->sock->sendmsg(msg)) disconnect(idx);
}

void findbroadcast(string buf)  { // to Game0
  string hedder = "Gxx";
  hedder[1] = hedder[2] = '\0';
  hedder += buf;
  for (int i=0; i < MAX_CLIENTS; i++)
    if (player[i] && player[i]->sock && (player[i]->flags & FL_INGAME0))
      sendcommand(i, hedder);
}
