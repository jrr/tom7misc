#include <iostream.h>
#include <string>
#include <stdio.h>
#include <stdlib.h>

#include "tom.h"
#include "tsocket.h"
#include "aux.h"
#include "parse.h"

#include "icy.h"

#include "server.h"

#include <errno.h>
#include <sys/socket.h>

#include <signal.h>

string DEFAULT_SERVER = "165.166.177.26"; // bestweb/charlotte
#define DEFAULT_PORT 6667
#define LISTEN_PORT 16385
string  DEFNICK  = "Propellor";
string  REALNAME = "I left my heart in syntax error";
string  USERNAME = "KILL";
string  DEFCHANNEL = "#42";
string  NICKPASS = "nopass";
char * iendl = "\n";

int PINGTIME = (60*5*3);   /* 5 minutes */

string longtime(int c); /* DELETEME */

void clean(int something);

void printout(string);
tsocket *sock=NULL, *server=0;

extern int retardo;
int dospeak = 1;

int main(int argc, char ** argv) {
  string inward, outward;

  int wtime = 2;
  int mod=0, val=0;
  errno = 0;
  int kbio=1;
  int pingleft = PINGTIME;
  //  long otime=0;
  signal(SIGINT,  clean);
  signal(SIGTERM, clean);
  signal(SIGHUP,  clean);
  signal(SIGPIPE, ignoresigpipe);

  system("stty raw -echo");
  iendl = "\r\n";

  for (int a=1;a<(argc-1);a++) {
    if (string(argv[a]) == "-n")
      DEFNICK = argv[++a];
    else if (string(argv[a]) == "-u")
      USERNAME = argv[++a];
    else if (string(argv[a]) == "-c")
      DEFCHANNEL = argv[++a];
    else if (string(argv[a]) == "-s")
      DEFAULT_SERVER = argv[++a];
    else if (string(argv[a]) == "-p") {
      PINGTIME = atoi(argv[++a]);
      pingleft = PINGTIME;
    } else if (string(argv[a]) == "-np") {
      NICKPASS = argv[++a];
    } else if (string(argv[a]) == "-b") {
      /* just bot; no text analysis or speak server */
      dospeak = 0;
    }
  }

  load_action_list(ACTION_LIST);

  if (dospeak) {
    server = makeserversock(LISTEN_PORT);

    froodsays = new linkbank("icy-back.txt");
  }

  sock = new tsocket;

    
 reconnecto:
  sock->connectto(DEFAULT_SERVER.c_str(), DEFAULT_PORT);

  say((string)"USER " + USERNAME +(string)" dino saur :"+ REALNAME);
  say((string)"NICK " + DEFNICK);

  while (1) {
    /* don't eat cycles if nobody is pressing keys */
    if (wtime > 600000) usleep(600000); 
    else usleep(wtime = (int)(wtime * 1.5));

    if (--pingleft < 0) {
      sock->sendmsg("PING pikachu!\r\n");
      pingleft = PINGTIME;
    }

    if (sock->getstatus() != STAT_OK) { sleep(1); goto reconnecto; }
    if (kbio && kbhit()) {
      wtime = 2; /* get faster response if typing */
      int c = (unsigned char)getchar();
      cout << (char) c << flush;
      switch (c) {
      case 3: clean(0); break;
      case 8:
	cout << flush << ' ' << char(8) << flush << flush;
	outward = outward.substr(0,outward.length()-1);
	break;
      case 26:
	kbio=0;
	system("stty cooked echo");
	iendl="\n";
	cout << "Forking...\n";
	if(fork()) exit(0); /* fork as daemon */
	break;
      case '\r':
	printout(outward);
	sock->sendmsg(outward+(string)"\n");
	outward = ""; mod =0; val=0;
      default:
	if (mod) {
	  if (c>='0' && c<='9')
	    val += (c-'0')<<(--mod*4);
	  else 
	    val += (((c|32)-'a')+10)<<(--mod*4);
	  if (!mod) { cout << "["<<val<<"]";outward += (char)val;}
	} else if (c=='\\') {
	  mod=2; val=0;
	} else outward += c;    
      }
    } 
    if ("" != (inward = sock->getpacket())) {
      parse (inward);
    }

    if (dospeak) {
      if (!server->valid()) { delete server; server=0; }
      if (!server) server = makeserversock(LISTEN_PORT);
      else do_serverstuff(server);
    }
    if (!sock->valid()) { sleep(3); goto reconnecto; }
   
  }
  clean(0);
}

void printout(string ooo) {
  if (ooo[ooo.length()-1]) 
    cout <<"<< "<< ooo.substr(0,ooo.length()-1) << iendl;
  else cout <<"<< " << ooo << iendl;
}

void big_error(const char * s) {
  cout << "FATAL: " << s << iendl;
  clean(1);
}

void clean(int s) {
  system("stty cooked echo");
  iendl = "\n";
  cout << "\nCleanup...\n";

  if (sock != NULL && sock->getstatus() == STAT_OK) {
    say("QUIT :Abandon ship!!!!");
    sock->disconnect();
    delete sock;
    sock = NULL;
  }
  if (dospeak)
    froodsays->savetofile("icy-back.txt");
  if (s) exit (-1);
  else exit (0);
}

void say(string s) {
  sock->sendmsg(s + '\n');
}
