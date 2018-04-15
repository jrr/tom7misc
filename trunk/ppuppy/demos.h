#ifndef __DEMOS_H
#define __DEMOS_H

#include "ppuppy.h"
#include "screen.h"

struct BouncingBalls {
  struct Ball {
    int bdx, bdy, bx, by, sqdia;
    void Update();
  };
  Ball ball1{1, 2, 64, 120, 100 * 100};
  Ball ball2{-3, 1, 120, 180, 74 * 74};

  // Update frame and draw to screen.
  void Draw();
  int frames;
  Screen screen;
};

#endif
