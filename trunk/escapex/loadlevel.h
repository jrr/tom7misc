
#ifndef __LOADLEVEL_H
#define __LOADLEVEL_H

#include "escapex.h"
#include "font.h"
#include "selector.h"
#include "player.h"
#include "util.h"

/* abstract interface */
struct LoadLevel : public Drawable {
  virtual ~LoadLevel();

  virtual string selectlevel() = 0;
  static LoadLevel *Create(Player *, string dir, 
			   bool inexact,
			   bool allow_corrupted = false);

  virtual bool first_unsolved(string &file, string &title) = 0;
  
  /* Drawable */
  void draw() override = 0;
  void screenresize() override = 0;
};

#endif
