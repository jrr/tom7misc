#include <iostream.h>
#include <string>
#include <stdio.h>
#include <stdlib.h>

#include "../epoch.h"
#include "../tom.h"

#include <errno.h>
#include <sys/socket.h>

#include <signal.h>

#define SERVER_NAME "what.was.this.for.org"
#define DEFAULT_SERVER "127.0.0.1"
#define DEFAULT_PORT 4242

void clean(int something);
void parse(string msg);
esocket *sock=NULL;

int main() {
  string inward, outward;
  int len, mod=0, val=0;
  errno = 0;
  
  signal(SIGINT,  clean);
  signal(SIGTERM, clean);
  signal(SIGHUP,  clean);

  sock = new esocket(DEFAULT_SERVER, DEFAULT_PORT);
  if (errno) {
  cout << "client: " << flush;
  switch (errno) {
    case EBADF:        cout << "Bad FD"; break;
    case EFAULT:       cout << "EFAULT"; break;
    case ENOTSOCK:     cout << "NOTSOCK"; break;
    case EISCONN:      cout << "ISCONN"; break;
    case ECONNREFUSED: cout << "connection refused"; break;
    case EADDRINUSE:   cout << "addr in use"; break;
    case ENETUNREACH:  cout << "netunreach"; break;
    default: cout << errno; break;
  } exit (-1);
  }
  cout << "\nConnected......\n";
  sock->sendmsg("v0666");
  sock->sendmsg("LGuest\0");
  sock->sendmsg("Ppass\0");
  sock->sendmsg("]");
//  sock->sendmsg("& PING 81348734");
  system("stty raw -echo");  
  while (1) {
    usleep(10); // be nice
    if (kbhit()) {
        int c = (unsigned char)getchar();
        if (c == 3) clean(0);
        cout << (char) c << flush;
        if (c == '\r') {
             cout << "\r\n";
             sock->sendmsg(outward);
             outward = ""; mod =0; val=0;
        } else {
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
    if (ES_PACKET_READY == sock->getpacketstatus()) {
      inward = sock->getpacket();
      cout << "[<<] Got packet : " << esc(inward) << "\r\n";
      parse (inward);
    }
  }
  clean(0);
}

void clean(int s) {
  system("stty cooked echo");
  cout << "Cleanup...\n";
  if (sock != NULL) {
    sock->writePacket("x", 1);
    sock->closesock();
    delete sock;
    sock = NULL;
  }
  if (s) exit (-1);
  else exit (0);
}

void parse(string msg) {
   switch(msg[0]) {
   case 'X':
      cout << "[!!] Disconnected...\r\n";
      clean(0);
      break;
   case 'x':
      cout << "[SD] " << (char *)(msg.c_str() + 1);
      break;
   default:
   break;
   } 

}

