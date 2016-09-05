
#include "textscroll.h"
#include "escapex.h"
#include "draw.h"

#define BACKLOG 256

namespace {
struct TextScroll_ : public TextScroll {
  static TextScroll_ *Create(Font *f);
  virtual void say(string s);
  virtual void unsay();

  /* Drawable */
  virtual void draw();
  virtual void screenresize();

  virtual void drawto(SDL_Surface *surf = 0);

  virtual ~TextScroll_();
  
  string log[BACKLOG];
  int pwrite;

  // Not owned.
  Font *ft;
};

TextScroll_::~TextScroll_() {}

void TextScroll_::screenresize() {}

TextScroll_ *TextScroll_::Create(Font *f) {
  TextScroll_ *ts = new TextScroll_();
  ts->ft = f;
  ts->posx = 0;
  ts->posy = 0;
  ts->vskip = 0;
  ts->height = screen->h;
  ts->width = screen->w;
  ts->pwrite = 0;
  return ts;
}

void TextScroll_::say(string s) {
  /* write new entry */
  log[pwrite] = s;
  pwrite++;
  pwrite %= BACKLOG;
}

/* weird results if unsay() with
   an empty buffer */
void TextScroll_::unsay() {
  if (pwrite) {
    pwrite--;
    log[pwrite] = "";
  } else {
    pwrite = BACKLOG - 1;
    log[pwrite] = "";
  }
}

/* XXX word wrap would be desirable.
   It's a little tricky because we're
   going backwards, but it's still easier
   than going forwards */
void TextScroll_::drawto(SDL_Surface *surf) {
  if (!surf) surf = screen;

  int y = (posy + height) - (ft->height + vskip);
  int l = pwrite?(pwrite-1):(BACKLOG-1);

  while (y > posy) {
    ft->drawto(surf, posx, y, log[l]);

    l--;
    if (l < 0) l = BACKLOG - 1;
    y -= (ft->height + vskip);
  }
}

void TextScroll_::draw() { 
  drawto(screen); 
}

}  // namespace

TextScroll::~TextScroll() {}
TextScroll *TextScroll::Create(Font *f) {
  return TextScroll_::Create(f);
}
