#include <stdio.h>
#include <string>

#include "winsock.h"
#include "lsock.h"

int main (void) {
  SocketInit ();

  lsock internet;

  internet.connect(dotaddr(127,0,0,1), 80);
  internet.send("Hey, word up!");

}

