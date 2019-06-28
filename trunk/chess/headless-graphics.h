#ifndef __HEADLESS_GRAPHICS_H
#define __HEADLESS_GRAPHICS_H

#include <vector>
#include <string>
#include <cstdint>

// Write the pixel into the surface. Replaces alpha channel; does not blend.
void SetPixel(int w, int h, int x, int y,
	      uint8_t r, uint8_t g, uint8_t b, uint8_t a,
	      std::vector<uint8_t> *rgba);

// Fill rectangle with the given color; replaces alpha channel.
void FillRect(int w, int h,
	      int rectx, int recty, int rectw, int recth,
	      uint8_t r, uint8_t g, uint8_t b, uint8_t a,
	      std::vector<uint8_t> *rgba);

void SaveARGB(const std::vector<uint8_t> &argb, int width, int height,
	      const std::string &filename);

void SaveRGBA(const std::vector<uint8_t> &rgba, int width, int height,
	      const std::string &filename);

inline constexpr uint8_t Mix4(uint8_t v1, uint8_t v2, uint8_t v3, uint8_t v4) {
  return (uint8_t)(((uint32_t)v1 + (uint32_t)v2 +
		    (uint32_t)v3 + (uint32_t)v4) >> 2);
}

#endif
