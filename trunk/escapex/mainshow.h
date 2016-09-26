
#ifndef __MAINSHOW_H
#define __MAINSHOW_H

#include <memory>

#include "draw.h"
#include "version.h"
#include "util.h"
#include "prefs.h"
#include "chars.h"
#include "client.h"
#include "loadlevel.h"
#include "message.h"
#include "play.h"
#include "generator.h"
#include "textscroll.h"

/* draws an ever-changing and poorly-played level */

struct MainShow {
  /* width and height given in tiles */
  static std::unique_ptr<MainShow> Create(int w, int h, int zf = 1);
  
  /* take a step. this can be about anything */
  virtual void step() = 0;

  /* draw to x,y on the supplied surface. if surface is
     0, then draw to the screen */
  virtual void draw(int x, int y, SDL_Surface *surf = nullptr) = 0;

  virtual ~MainShow();

  virtual int width() const = 0;
};

#endif
