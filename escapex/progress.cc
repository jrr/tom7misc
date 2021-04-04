#include "progress.h"

#include <string>

#include "escapex.h"
#include "draw.h"
#include "../cc-lib/sdl/font.h"


void Progress::drawbar(void *vepoch, int n, int tot,
                       const string &msg, int ticks) {
  Uint32 * epoch = (Uint32*) vepoch;
  if (tot > 50 && !(n % 5)) {
    Uint32 ti = SDL_GetTicks();
    if (*epoch < ti) {

      int w = screen->w - 4;
      int wd = (int)(((float)n / (float)tot) * (float)w);

      SDL_Rect rect;
      rect.y = 2;
      rect.h = 4;

      /* done part */
      rect.x = 2;
      rect.w = wd;
      SDL_FillRect(screen, &rect, PROGRESS_DONECOLOR);

      /* undone part */
      rect.x = wd + 2;
      rect.w = w - wd;
      SDL_FillRect(screen, &rect, PROGRESS_RESTCOLOR);

      rect.x = 2;
      rect.w = w;

      /* space for message */
      int lin = Font::lines(msg);
      rect.y = fonsmall->height * 2 - 2;
      rect.h = (fonsmall->height * lin) + 3;
      SDL_FillRect(screen, &rect, PROGRESS_BACKCOLOR);

      /* message */
      fonsmall->drawlines(6, fonsmall->height * 2, msg);

      SDL_Flip(screen);

      *epoch = ti + ticks;

    }
  }
}
