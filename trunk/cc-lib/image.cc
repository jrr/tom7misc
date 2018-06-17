
#include "image.h"

#include <string>
#include <vector>
#include <cstdint>
#include <utility>
#include <cstring>

#include "stb_image.h"
#include "stb_image_write.h"
#include "base/logging.h"

using namespace std;

// static
ImageRGBA *ImageRGBA::Load(const string &filename) {
  vector<uint8> ret;
  int width, height, bpp_unused;
  uint8 *stb_rgba = stbi_load(filename.c_str(),
			      &width, &height, &bpp_unused, 4);
  const int bytes = width * height * 4;
  ret.resize(bytes);
  if (stb_rgba == nullptr) return nullptr;
  // TODO: Is this portable (or even correct) wrt to endianness?
  memcpy(ret.data(), stb_rgba, bytes);
  // Does this move image data all the way in, or do we need to
  // write a move constructor manually? Better way?
  return new ImageRGBA(std::move(ret), width, height);
}

ImageRGBA::ImageRGBA(const vector<uint8> &rgba, int width, int height)
  : width(width), height(height), rgba(rgba) {
  CHECK(rgba.size() == width * height * 4);
}

ImageRGBA::ImageRGBA(int width, int height)
  : width(width), height(height), rgba(width * height * 4) {
  Clear(0, 0, 0, 0);
}

void ImageRGBA::Save(const std::string &filename) const {
  CHECK(rgba.size() == width * height * 4);
  stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
}

ImageRGBA *ImageRGBA::Copy() const {
  return new ImageRGBA(rgba, width, height);
}

ImageRGBA::uint32 ImageRGBA::GetPixel(int x, int y) const {
  // Treat out-of-bounds reads as containing 00,00,00,00.
  if (x < 0 || x >= width ||
      y < 0 || y >= height) return 0;
  const int base = (y * width + x) << 2;
  return (rgba[base] << 24) |
    (rgba[base + 1] << 16) |
    (rgba[base + 2] << 8) |
    rgba[base + 3];
}

void ImageRGBA::Clear32(uint32 color) {
  // PERF: This can be optimized by writing 32 bits at a time,
  // but beware endianness, etc.
  Clear((color >> 24) & 255,
	(color >> 16) & 255,
	(color >> 8) & 255,
	color & 255);
}

void ImageRGBA::Clear(uint8 r, uint8 g, uint8 b, uint8 a) {
  for (int i = 0; i < width * height * 4; i += 4) {
    rgba[i + 0] = r;
    rgba[i + 1] = g;
    rgba[i + 2] = b;
    rgba[i + 3] = a;
  }
}

void ImageRGBA::SetPixel(int x, int y,
			 uint8 r, uint8 g, uint8 b, uint8 a) {
  if (x < 0 || x >= width ||
      y < 0 || y >= height) return;
  int i = (y * width + x) * 4;
  rgba[i + 0] = r;
  rgba[i + 1] = g;
  rgba[i + 2] = b;
  rgba[i + 3] = a;
}

void ImageRGBA::SetPixel32(int x, int y, uint32 color) {
  if (x < 0 || x >= width ||
      y < 0 || y >= height) return;
  int i = (y * width + x) * 4;
  rgba[i + 0] = (color >> 24) & 255;
  rgba[i + 1] = (color >> 16) & 255;
  rgba[i + 2] = (color >>  8) & 255;
  rgba[i + 3] = (color      ) & 255;
}

ImageA::ImageA(const vector<uint8> &alpha, int width, int height)
    : width(width), height(height), alpha(alpha) {
  CHECK(alpha.size() == width * height);
}

ImageA *ImageA::Copy() const {
  return new ImageA(alpha, width, height);
}
