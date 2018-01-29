#ifndef __HEADLESS_GRAPHICS_H
#define __HEADLESS_GRAPHICS_H

#include <vector>
#include <string>

#include "pftwo.h"

// Write the pixel into the surface. Replaces alpha channel; does not blend.
void SetPixel(int w, int h, int x, int y,
	      uint8 a, uint8 r, uint8 g, uint8 b,
	      vector<uint8> *argb);

void SaveARGB(const vector<uint8> &argb, int width, int height,
	      const string &filename);

inline constexpr uint8 Mix4(uint8 v1, uint8 v2, uint8 v3, uint8 v4) {
  return (uint8)(((uint32)v1 + (uint32)v2 + (uint32)v3 + (uint32)v4) >> 2);
}

#endif
