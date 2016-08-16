
#ifndef __PLAYERDB_H
#define __PLAYERDB_H

#include "player.h"
#include <string>
#include "selector.h"


using namespace std;

/* abstract interface */
struct PlayerDB {

  /* make db by searching cwd for player files.
     if there are none, create a default. */
  static PlayerDB * create();

  virtual Player *chooseplayer() = 0;

  /* was this the first launch? (no default player found) */
  virtual bool firsttime() = 0;

  virtual ~PlayerDB() {}
};

#endif

