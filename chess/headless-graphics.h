#ifndef __HEADLESS_GRAPHICS_H
#define __HEADLESS_GRAPHICS_H

#include <vector>
#include <string>
#include <cstdint>

// Write the pixel into the surface. Replaces alpha channel; does not blend.
void SetPixel(int w, int h, int x, int y,
	      uint8_t a, uint8_t r, uint8_t g, uint8_t b,
	      std::vector<uint8_t> *argb);

void SaveARGB(const std::vector<uint8_t> &argb, int width, int height,
	      const std::string &filename);

inline constexpr uint8_t Mix4(uint8_t v1, uint8_t v2, uint8_t v3, uint8_t v4) {
  return (uint8_t)(((uint32_t)v1 + (uint32_t)v2 +
		    (uint32_t)v3 + (uint32_t)v4) >> 2);
}

#endif
