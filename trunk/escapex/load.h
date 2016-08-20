
#ifndef __LOAD_H
#define __LOAD_H

#include "escapex.h"
#include "font.h"
#include "selector.h"
#include "player.h"
#include "util.h"

/* abstract interface */
struct loadlevel : public Drawable {
  virtual ~loadlevel();
  virtual void destroy() = 0;

  virtual string selectlevel() = 0;
  static loadlevel *create(Player *, string dir, 
			    bool inexact,
			    bool allow_corrupted = false);

  virtual bool first_unsolved(string & file, string & title) = 0;
  
  /* Drawable */
  virtual void draw() = 0;
  virtual void screenresize() = 0;
};

#endif
