
#ifndef __UPDATE_H
#define __UPDATE_H

#include "escapex.h"
#include "level.h"
#include "player.h"
#include "http.h"
#include "draw.h"

/* should be in a config somewhere */
#define COLLECTIONSURL "/COLLECTIONS"

enum UpdateResult {
  UD_SUCCESS, UD_FAIL,
};

/* update */
struct Updater : public Drawable {
  static Updater *Create(Player *p);
  virtual UpdateResult update(string &msg) = 0;
  virtual ~Updater();

  void draw() override = 0;
  void screenresize() override = 0;
};

#endif
