
#ifndef __TEXTSCROLL_H
#define __TEXTSCROLL_H

#include "escapex.h"

struct TextScroll : public Drawable {
  int posx;
  int posy;

  int height;
  int width;

  /* pixels between lines */
  int vskip;

  /* at default size = entire screen */
  static TextScroll *Create(Font *);
  virtual void say(string s) = 0;
  virtual void unsay() = 0;

  /* for Drawable interface */
  void draw() override = 0;
  void screenresize() override = 0;
  
  virtual void drawto(SDL_Surface *surf = 0) = 0;

  virtual ~TextScroll();
};

#endif
