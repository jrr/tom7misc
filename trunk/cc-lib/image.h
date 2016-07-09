// Images residing in RAM as vectors of pixels.

#ifndef __IMAGE_H
#define __IMAGE_H

#include <string>
#include <vector>
#include <cstdint>

#include "base/logging.h"

// 4-channel image in R-G-B-A order.
struct ImageRGBA {
  using uint8 = uint8_t;
  ImageRGBA(const std::vector<uint8> &rgba, int width, int height);
  
  static ImageRGBA *Load(const std::string &filename);
  void Save(const std::string &filename) const;

  ImageRGBA *Copy() const;

  const int width, height;
  // Size width * height * 4.
  std::vector<uint8> rgba;
};

// Single-channel bitmap.
struct ImageA {
  using uint8 = uint8_t;
  ImageA(const std::vector<uint8> &alpha, int width, int height);

  ImageA *Copy() const;
  
  const int width, height;
  // Size width * height.
  std::vector<uint8> alpha;
};

#endif
