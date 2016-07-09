
#include "image.h"

#include <string>
#include <vector>
#include <cstdint>
#include <utility>

#include "stb_image.h"
#include "stb_image_write.h"

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

void ImageRGBA::Save(const std::string &filename) const {
  CHECK(rgba.size() == width * height * 4);
  stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
}

ImageRGBA *ImageRGBA::Copy() const {
  return new ImageRGBA(rgba, width, height);
}


ImageA::ImageA(const vector<uint8> &alpha, int width, int height)
    : width(width), height(height), alpha(alpha) {
  CHECK(alpha.size() == width * height);
}

ImageA *ImageA::Copy() const {
  return new ImageA(alpha, width, height);
}
