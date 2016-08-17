
#ifndef __UPDATE_H
#define __UPDATE_H

#include "escapex.h"
#include "level.h"
#include "player.h"
#include "http.h"
#include "draw.h"

/* should be in a config somewhere */
#define COLLECTIONSURL "/COLLECTIONS"

enum updateresult {
  UD_SUCCESS, UD_FAIL,
};

/* update */
struct Updater : public Drawable {
  static Updater *create(Player *p);
  virtual updateresult update(string & msg) = 0;
  virtual void destroy() = 0;
  virtual ~Updater() {};

  void draw() override = 0;
  void screenresize() override = 0;
};

#endif
