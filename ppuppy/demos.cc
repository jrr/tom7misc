
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <mutex>

#include "demos.h"
#include "convert.h"

#include "util.h"
#include "talk.h"
#include "base/logging.h"

void BouncingBalls::Ball::Update() {
  bx += bdx;
  by += bdy;
  if (bx < 0) { bx = 0; bdx = -bdx; }
  if (by < 0) { by = 0; bdy = -bdy; }
  if (bx > (31 * 8)) { bx = 31 * 8; bdx = -bdx; }
  if (by > (29 * 8)) { by = 29 * 8; bdy = -bdy; }
}

inline bool Contains(const BouncingBalls::Ball &ball, int x, int y) {
  int dx = x - ball.bx;
  int dy = y - ball.by;
  return ((dx * dx) + (dy * dy)) < ball.sqdia;
}

void BouncingBalls::Draw() {
  frames++;
  if (true || frames % 8 == 0) {
    ball1.Update();
    ball2.Update();
  }

  screen.palette[0] = 0x1d;
  screen.palette[1] = 0x15;
  screen.palette[2] = 0x27;
  screen.palette[3] = 0x2a;
  screen.palette[4] = 0x0;
  
  // Here, using palette 0 for the entire screen.
  for (int y = 0; y < NUM_SCANLINES; y++) {
    for (int x = 0; x < NUM_COLS; x++) {
      int idx = y * NUM_COLS + x;
      screen.attr[idx] = 0; // (frames & 1) ? 0xFF : 0x00;
    }
  }
  
  // Compute the image a single pixel at a time. Assumes we have
  // plenty of time during vblank.
  for (int y = 0; y < NUM_SCANLINES; y++) {
    for (int x = 0; x < 256; x++) {
      // Write the bits into the appropriate byte.
      const int idx = (y * NUM_COLS) + (x >> 3);
      // The bits are arranged from msb to lsb, like
      // you would want. (1 << 7) is the leftmost.
      const int bidx = 7 - (x & 7);
      const int b = ((x & 7) == 2) | ((y & 7) == 3);
      uint8 lobit = Contains(ball1, x, y) ? (1 << bidx) : (b << bidx);
      uint8 hibit = Contains(ball2, x, y) ? (1 << bidx) : 0;
      // Keep all other bits the same.
      const uint8 keep_mask = ~(1 << bidx);
      screen.color_lo[idx] = (screen.color_lo[idx] & keep_mask) | lobit;
      screen.color_hi[idx] = (screen.color_hi[idx] & keep_mask) | hibit;
    }
  }
}

Slideshow::Slideshow(const string &meta_file,
		     const string &slide_data_file) :
  talk(meta_file, slide_data_file) {
  fprintf(stderr, "Loaded %d slides and %d screens from %s.\n",
	  talk.NumSlides(),
	  talk.NumScreens(),
	  meta_file.c_str());
  CHECK(talk.NumSlides() > 0) << "No slides?";
}

void Slideshow::Update(uint8 joy1, uint8 joy2) {
  if ((joy1 & ~old_joy1) & RIGHT) {
    slide_idx ++;
    if (slide_idx >= talk.NumSlides()) slide_idx = 0;
    anim_idx = 0;
    count = 0;
  } else if ((joy1 & ~old_joy1) & LEFT) {
    slide_idx --;
    if (slide_idx < 0) slide_idx = talk.NumSlides() - 1;
    anim_idx = 0;
    count = 0;
  } else {
    // advance anim
    CHECK(slide_idx >= 0 && slide_idx < talk.NumSlides());
    CompiledTalk::Slide *slide = talk.GetSlide(slide_idx);
    CHECK(anim_idx >= 0 && anim_idx < slide->screens.size());
    count++;
    if (count > slide->screens[anim_idx].second) {
      anim_idx ++;
      if (anim_idx >= slide->screens.size()) anim_idx = 0;
      count = 0;
    }
  }
    
  old_joy1 = joy1;
}

Screen *Slideshow::GetScreen() {
  CHECK(slide_idx >= 0 && slide_idx < talk.NumSlides());
  CompiledTalk::Slide *slide = talk.GetSlide(slide_idx);
  CHECK(anim_idx >= 0 && anim_idx < slide->screens.size())
    << anim_idx << " " << slide->screens.size();
  int screen_idx = slide->screens[anim_idx].first;
  CHECK(screen_idx >= 0 && screen_idx < talk.NumScreens());
  return talk.GetScreen(screen_idx);
}

Screen *Slideshow::GetNextScreen() {
  // Always predict that we'll be on the same slide.
  CompiledTalk::Slide *slide = talk.GetSlide(slide_idx);

  int tct = count + 1;
  int ai = anim_idx;
  if (tct > slide->screens[ai].second) {
    ai ++;
    if (ai >= slide->screens.size()) ai = 0;
    tct = 0;
  }
  int screen_idx = slide->screens[ai].first;
  return talk.GetScreen(screen_idx);
}
