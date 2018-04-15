// Computes the PPU data for an image.

#ifndef __SCREEN_H
#define __SCREEN_H

#include "ppuppy.h"


static constexpr int NUM_COLS = 32;
static constexpr int NUM_COARSE = 30;
static constexpr int NUM_FINE = 8;
static constexpr int NUM_SCANLINES = NUM_COARSE * NUM_FINE;

// Unencoded screen. This represents the actual bytes we want
// the PPU to see for each byte of the packet.
// We ignore nametable, since that value is only used by the
// PPU to determine later requests (driver can use it as a
// sync signal though). Attributes, low and high color bits
// all have the same resolution.
struct Screen {
  uint8 attr[NUM_SCANLINES * NUM_COLS];
  uint8 color_lo[NUM_SCANLINES * NUM_COLS];
  uint8 color_hi[NUM_SCANLINES * NUM_COLS];
};

#endif
