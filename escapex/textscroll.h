
#ifndef _ESCAPE_TEXTSCROLL_H
#define _ESCAPE_TEXTSCROLL_H

#include "escapex.h"

struct TextScroll : public Drawable {
  int posx = 0;
  int posy = 0;

  int height = 0;
  int width = 0;

  /* pixels between lines */
  int vskip = 0;

  /* at default size = entire screen */
  static TextScroll *Create(Font *);
  virtual void Say(string s) = 0;
  virtual void Unsay() = 0;

  /* for Drawable interface */
  void Draw() override = 0;
  void ScreenResize() override = 0;

  virtual void DrawTo(SDL_Surface *surf = 0) = 0;

  virtual ~TextScroll();
};

#endif
