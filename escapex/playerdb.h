
#ifndef _ESCAPE_PLAYERDB_H
#define _ESCAPE_PLAYERDB_H

#include "player.h"
#include "selector.h"

/* abstract interface */
struct PlayerDB {
  /* make db by searching cwd for player files.
     if there are none, create a default. */
  static PlayerDB *create();

  virtual Player *chooseplayer() = 0;

  /* was this the first launch? (no default player found) */
  virtual bool firsttime() = 0;

  virtual ~PlayerDB() {}
};

#endif

