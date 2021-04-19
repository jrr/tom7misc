
#include "dirt.h"

#include <vector>
#include <tuple>

#include "escapex.h"
#include "../cc-lib/sdl/sdlutil.h"

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
  vector<tuple<int, int, int, int>> dirts;
};
}  // namespace

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
  dirts.emplace_back(x, y, w, h);
}

void Dirt_::clean() {
  for (auto [x, y, w, h] : dirts) {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    r.w = w;
    r.h = h;
    SDL_BlitSurface(surf, &r, screen, &r);
    /* debug version */
    //    SDL_FillRect(screen, r, 0x99AA9999);
  }
}

Dirt *Dirt::create() {
  return new Dirt_();
}

Dirt::~Dirt() {}
