#ifndef __DEMOS_H
#define __DEMOS_H

#include "ppuppy.h"
#include "screen.h"
#include <vector>
#include <string>

struct BouncingBalls {
  struct Ball {
    int bdx, bdy, bx, by, sqdia;
    void Update();
  };
  Ball ball1{1, 2, 64, 120, 100 * 100};
  Ball ball2{-3, 1, 120, 180, 74 * 74};

  // Update frame and draw to screen.
  void Draw();
  int frames = 0;
  Screen screen;
};

struct Slideshow {
  Slideshow(const vector<string> &filenames);
  
  void Update();
  int frames = 0;
  Screen *GetScreen();
  std::vector<Screen> screens;
};

#endif
