#ifndef _TM7_TSOCKET_H_
#define _TM7_TSOCKET_H_

#include <string>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <iostream.h>
#include <netdb.h>


#include <stdlib.h>

using namespace std;

enum { STAT_OK, STAT_NOTCONNECTED, };

struct qitem {
  qitem * next;
  string data;
  qitem(qitem * n, string d) : next(n), data(d) {}
};

class tsocket {
  /* should be private */
 public: 
  int sock;
  void gonodelay() { fcntl(sock, F_SETFL, O_NDELAY); }
  void godelay() {   fcntl(sock, F_SETFL, 0); }
  string saveme;
  qitem * QHEAD;
  qitem * QTAIL;
  string _hostname;
 public:
  int err;
  int valid();
  int hello(); /* a little smarter/slower valid call */
  int validate(); /* prints debugging info */
  tsocket * getconn(); /* only for server socks! */
  string getfromreturnqueue();
  void addtoreturnqueue(string);
  string getpacket();
  string hostname();
  int writepacket(const char *b, int len);
  int getstatus() { if (sock) return STAT_OK;
                          else return STAT_NOTCONNECTED; }
  tsocket();
  tsocket(int);
 ~tsocket();
  int disconnect();
  int connectto(string, int port);
  void sendmsg(string);
};


#endif



