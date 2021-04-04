#ifndef _ESCAPE_BACKGROUNDS_H
#define _ESCAPE_BACKGROUNDS_H

#include "escapex.h"
#include "draw.h"
#include "drawable.h"

struct Backgrounds {

  /* Creates a checkerboard pattern of blocks with
     a random vertical gradient atop it. Modifies
     surf so that it points to an surface with the
     same size as the screen, if it doesn't already. */
  static void GradientBlocks(SDL_Surface *&surf,
                             int tile_white,
                             int tile_black,
                             float gradient_hue);

  /* Hues in degrees */
  static const float blueish;
  static const float purpleish;

 private:
  Backgrounds();
};

#endif
