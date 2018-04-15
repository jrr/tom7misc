
#include <stdio.h>

#include "demos.h"
#include "convert.h"

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

Slideshow::Slideshow(const vector<string> &filenames) {
  screens.reserve(filenames.size());
  for (const string &f : filenames) {
    screens.push_back(ScreenFromFile(f));
  }
}

void Slideshow::Update() { frames++; }

Screen *Slideshow::GetScreen() {
  return &screens[(frames >> 8) % screens.size()];
}
