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

// Same, but pre-encoded for the output pins.
struct EncodedScreen {
  // PERF: I can probably get away with uint16. Is it faster?
  uint32 encoded_attr[NUM_SCANLINES * NUM_COLS];
  uint32 encoded_color_lo[NUM_SCANLINES * NUM_COLS];
  uint32 encoded_color_hi[NUM_SCANLINES * NUM_COLS];
};

void EncodeScreen(const Screen &screen, EncodedScreen *encoded);

#endif
