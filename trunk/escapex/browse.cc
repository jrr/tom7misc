
// XXX I started writing this like 10 years ago but didn't really get
// anywhere -- start over again?
// Things like client operations (upload/comment) need to be in
// separate files.
#include "escapex.h"
#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "browse.h"
#include "../cc-lib/crypt/md5.h"
#include "backgrounds.h"

#include <string.h>
#include <sys/stat.h>
#include <time.h>

#include "directories.h"

#include "dircache.h"
#include "chars.h"

#include "message.h"
#include "upload.h"
#include "prompt.h"

#include "leveldb.h"
#include "commenting.h"

#include "optimize.h"
#include "client.h"
#include "animation.h"

#include "menu.h"

#include "progress.h"

#ifdef LINUX
/* just used for debugging */
#  include <sys/time.h>
#endif

/* how frequently to make a move in solution playback */
#define LOADFRAME_TICKS 100
#define STEPS_BEFORE_SOL 5

namespace {

struct Browse_ : public Browse {
  Browse_() : background(0) {}

  void Draw() override {
    SDL_BlitSurface(background, 0, screen, 0);
  }

  void ScreenResize() override {
    makebackground();
  }

  ~Browse_() override {}

  string SelectLevel() override;

  void makebackground();
  SDL_Surface *background;

  void redraw() {
    Draw();
    SDL_Flip(screen);
  }

  /* From constructor argument. */
  bool allow_corrupted;

  /* save query we performed. */
  static lquery lastquery;
  /* and last MD5 we selected */
  static string lastmd5;
};

void Browse_::makebackground() {
  int w = screen->w;
  int h = screen->h;

  Backgrounds::GradientBlocks(background,
                              T_GREY,
                              T_RED,
                              Backgrounds::purpleish);
  if (!background) return;

  const Uint32 curveborder_color =
    SDL_MapRGBA(background->format, 163, 102, 102, 255);

  /* Border around the whole thing. */

  /* Curves are square. */
  const int curve_size = 12;
  const int curveborder_width = 3;
  const int border_x1 = 17, border_y1 = 13;
  const int border_x2 = w - 17, border_y2 = h - 13;

  /* Curves */
  sdlutil::blitall(Animation::curveborder_tan_tl, background,
                   border_x1, border_y1);
  sdlutil::blitall(Animation::curveborder_tan_tr, background,
                   border_x2 - curve_size, border_y1);
  sdlutil::blitall(Animation::curveborder_tan_br, background,
                   border_x2 - curve_size, border_y2 - curve_size);
  sdlutil::blitall(Animation::curveborder_tan_bl, background,
                   border_x1, border_y2 - curve_size);

  const int inborder_height = border_y2 - border_y1 - curve_size * 2;
  const int inborder_width = border_x2 - border_x1 - curve_size * 2;

  /* Thick border connecting curves */
  /* left */
  sdlutil::fillrect(background, curveborder_color,
                    border_x1, border_y1 + curve_size,
                    curveborder_width,
                    inborder_height);

  /* top */
  sdlutil::fillrect(background, curveborder_color,
                    border_x1 + curve_size,
                    border_y1,
                    inborder_width,
                    curveborder_width);

  /* right */
  sdlutil::fillrect(background, curveborder_color,
                    border_x2 - curveborder_width,
                    border_y1 + curve_size,
                    curveborder_width,
                    inborder_height);

  /* bottom */
  sdlutil::fillrect(background, curveborder_color,
                    border_x1 + curve_size,
                    border_y2 - curveborder_width,
                    inborder_width,
                    curveborder_width);

  /* Partially-translucent insides. */
  {
    SDL_Surface *vert = sdlutil::makealpharect(curve_size - curveborder_width,
                                                inborder_height,
                                                0, 0, 0, 0.8 * 255);
    SDL_Surface *horiz = sdlutil::makealpharect(inborder_width,
                                                 curve_size - curveborder_width,
                                                 0, 0, 0, 0.8 * 255);
    SDL_Surface *center = sdlutil::makealpharect(inborder_width,
                                                  inborder_height,
                                                  0, 0, 0, 0.8 * 255);

    /* left and right */
    sdlutil::blitall(vert, background,
                     border_x1 + curveborder_width,
                     border_y1 + curve_size);

    sdlutil::blitall(vert, background,
                     border_x2 - curve_size,
                     border_y1 + curve_size);

    /* top and bottom */
    sdlutil::blitall(horiz, background,
                     border_x1 + curve_size,
                     border_y1 + curveborder_width);

    sdlutil::blitall(horiz, background,
                     border_x1 + curve_size,
                     border_y2 - curve_size);

    /* center */
    sdlutil::blitall(center, background,
                     border_x1 + curve_size,
                     border_y1 + curve_size);

    SDL_FreeSurface(vert);
    SDL_FreeSurface(horiz);
    SDL_FreeSurface(center);
  }

  const int title_x = 39, title_y = 31;
  sdlutil::blitall(Animation::choose_a_level, background, title_x, title_y);

  /* bottom panel */
  const int bottom_height = 169;

  /* separator bars */
  const Uint32 separator_color =
    SDL_MapRGBA(background->format, 92, 59, 59, 255);
  const int separator_x = border_x1 + curveborder_width + 1;
  const int separator_width = border_x2 - border_x1 - curveborder_width * 2 - 2;
  const int separator_height = 2;
  const int topsep_y = 112;
  const int botsep_y = border_y2 - bottom_height - separator_height;

  sdlutil::fillrect(background, separator_color,
                    separator_x, topsep_y, separator_width, separator_height);

  sdlutil::fillrect(background, separator_color,
                    separator_x, botsep_y, separator_width, separator_height);
}

string Browse_::SelectLevel() {
  SDL_Event event;

  [[maybe_unused]]
  Uint32 nextframe = SDL_GetTicks() + LOADFRAME_TICKS;
  redraw();

  for (;;) {
    SDL_Delay(1);

    [[maybe_unused]]
    Uint32 now = SDL_GetTicks();

    while (SDL_PollEvent(&event)) {

      if (HandleVideoEvent(this, event)) continue;

      if (event.type == SDL_KEYDOWN) {
        int key = event.key.keysym.sym;
        /* breaking from here will allow the key to be
           treated as a search */

        switch (key) {
          // Handle special keys...
        default:;
        }

      }

      // XXX bogus. use Selector.
      switch (event.type) {
      case SDL_QUIT:
        return "";
      default:;
      }
    }

  } /* unreachable */

}

}   // namespace

Browse *Browse::Create(bool allow_corrupted) {
  Browse_ *b = new Browse_;
  b->allow_corrupted = allow_corrupted;
  b->makebackground();
  return b;
}

