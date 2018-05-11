#ifndef __DEMOS_H
#define __DEMOS_H

#include "ppuppy.h"
#include "screen.h"
#include <vector>
#include <string>
#include <thread>
#include <memory>
#include "arcfour.h"
#include "talk.h"

struct BouncingBalls {
  struct Ball {
    int bdx, bdy, bx, by, sqdia;
    void Update();
  };
  Ball ball1{1, 2, 64, 120, 100 * 100};
  Ball ball2{-3, 1, 120, 180, 74 * 74};

  // Update frame and draw to screen.
  // XXX make this conform to common interface
  void Draw();
  int frames = 0;
  Screen screen;
};

struct Slideshow {
  Slideshow(const string &meta_file,
	    const string &slide_data_file);
  
  void Update(uint8 joy1, uint8 joy2);
  int frames = 0;
  Screen *GetScreen();
  // Predicted next screen, which will be right unless
  // switching slides.
  Screen *GetNextScreen();
  CompiledTalk talk;
  
  // Indicates which screen from Talk we're looking at.
  int slide_idx = 0;
  int anim_idx = 0;
  int count = 0;
  
  uint8 old_joy1 = 0;
};

struct SNES {
  SNES(const string &cart);

  void Update(uint8 joy1, uint8 joy2);
  Screen *GetScreen();
  Screen screen;
 private:
  void Run();
  ArcFour rc;
  std::unique_ptr<std::thread> th;
};

#endif
