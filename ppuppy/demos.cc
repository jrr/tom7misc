
#include "demos.h"

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
  ball1.Update();
  ball2.Update();

  // Here, using palette 0 for the entire screen.
  for (int i = 0; i < NUM_SCANLINES * NUM_COLS; i++)
    screen.attr[i] = 0;
  
  // Compute the image a single pixel at a time. Assumes we have
  // plenty of time during vblank.
  for (int y = 0; y < NUM_SCANLINES; y++) {
    for (int x = 0; x < 256; x++) {
      // Write the bits into the appropriate byte.
      const int idx = (y * NUM_COLS) + (x >> 3);
      // The bits are arranged from msb to lsb, like
      // you would want. (1 << 7) is the leftmost.
      const int bidx = 7 - (x & 3);
      uint8 lobit = Contains(ball1, x, y) ? (1 << bidx) : 0;
      uint8 hibit = Contains(ball2, x, y) ? (1 << bidx) : 0;
      // Keep all other bits the same.
      const uint8 keep_mask = ~(1 << bidx);
      screen.color_lo[idx] = (screen.color_lo[idx] & keep_mask) | lobit;
      screen.color_hi[idx] = (screen.color_hi[idx] & keep_mask) | hibit;
    }
  }
}
