
#include "dirt.h"

#include "escapex.h"
#include "ptrlist.h"
#include "../cc-lib/sdl/sdlutil.h"

using rlist = PtrList<SDL_Rect>;

namespace {
struct Dirt_ : public Dirt {

  void mirror() override;

  /* PERF could offer mirror_region, which
     allows us to do more local dirtyrect stuff
     by predicting the regions that we will
     need to mirror */

  /* enqueue a rectangle to be drawn from the
     mirror. */
  void setdirty(int x, int y, int w, int h) override;

  /* draw all enqueued dirty rectangles. */
  void clean() override;

  ~Dirt_() override {
    if (surf) SDL_FreeSurface(surf);
    SDL_Rect *tmp;
    while (( tmp = rlist::pop(dirts) )) delete tmp;
  }

  Dirt_() {
    matchscreen();
  }

  void matchscreen() override {
    if (!surf || surf->w != screen->w || surf->h != screen->h) {
      if (surf) SDL_FreeSurface(surf);
      surf = sdlutil::makesurface(screen->w, screen->h, false);
    }
  }

private:
  SDL_Surface *surf = nullptr;
  rlist *dirts = nullptr;
};

/* assumes surf is the correct size by invariant */
void Dirt_::mirror() {
  SDL_BlitSurface(screen, 0, surf, 0);
}

/* XXX one of the following two could be a bit more
   clever. for instance, if there are duplicate or mostly
   overlapping rectangles, we can take their union. If
   there is more dirty area than the screen, we can just
   redraw the whole screen. */
void Dirt_::setdirty(int x, int y, int w, int h) {
  SDL_Rect *r = (SDL_Rect*)malloc(sizeof (SDL_Rect));
  r->x = x;
  r->y = y;
  r->w = w;
  r->h = h;
  rlist::push(dirts, r);
}

void Dirt_::clean() {
  SDL_Rect *r;
  while ( (r = rlist::pop(dirts)) ) {
    SDL_BlitSurface(surf, r, screen, r);
    /* debug version */
    //    SDL_FillRect(screen, r, 0x99AA9999);
    free(r);
  }
}
}

Dirt *Dirt::create() {
  return new Dirt_();
}

Dirt::~Dirt() {}
