#ifndef __WM_EPLAYER_H_
#define __WM_EPLAYER_H_

#include <string>
#include "epoch.h"

enum { LS_NOTHING,
       LS_VERSION,
       LS_VERIFY,
       LS_USER,
       LS_PASS,
       LS_LOGGEDIN, };

#define FL_ACCEPT   (1)    // accepting any games
#define FL_ADMIN    (1<<2)
#define FL_GUEST    (1<<3)
#define FL_ACCEPTG  (1<<4) // accepting from Guests
#define FL_INGAME0  (1<<5) // In 'Find Mode'
#define FL_LOGGEDIN (1<<6) // logged in all the way

class eplayer {
 void init();

public:

  int id, num;

  short loginstage;

  long flags; //   = 0;
  long gameflags; //   = 0;
  long gameprefs; //   = 0;

  short rank;

  char   name[11];
  string email;
  string realname;

  esocket *sock;

  //void print();

  eplayer();
  eplayer(int, int);
  ~eplayer();

};

#endif
