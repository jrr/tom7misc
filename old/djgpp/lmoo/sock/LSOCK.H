#include <string>
#include "winsock.h"

class lsock {
public:
      InetAddress Connect;
      Socket s;

      connect(long addy, int port);
      send(char *, int);
      send(string);
      lsock();
     ~lsock();
};
