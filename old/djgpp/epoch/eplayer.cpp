#include <iostream.h>
#include "epoch.h"
#include <stdio.h>

//hmmm....
const char *binword(unsigned short num);

eplayer::eplayer() {
  //cout << "Constructing Player!\n";
  init();
}

const char *binword(unsigned short num) {
  char s[2];
  s[0] = (char) ( num / 256 );
  s[1] = (char) ( num % 256 );
  return s;
}

void eplayer::init() {
  cout << "Constructing player!\n";
  num = id = -1;
  flags = gameflags = gameprefs = rank = 0;
  realname = email = "";
  loginstage = 0;
  name[0] = 0;
  sock = NULL;
}

eplayer::eplayer(int playernumber, int playerid) {
  init();
  num = playernumber;
  id = playerid;
}

eplayer::~eplayer() {
     if (sock) delete sock;
}
