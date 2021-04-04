
#ifndef _ESCAPE_LOADLEVEL_H
#define _ESCAPE_LOADLEVEL_H

#include "escapex.h"
#include "selector.h"
#include "player.h"

/* abstract interface */
struct LoadLevel : public Drawable {
  virtual ~LoadLevel();

  virtual string SelectLevel() = 0;
  static LoadLevel *Create(Player *, string dir,
                           bool inexact,
                           bool allow_corrupted = false);

  virtual bool FirstUnsolved(string &file, string &title) = 0;

  /* Drawable */
  void Draw() override = 0;
  void ScreenResize() override = 0;
};

#endif
