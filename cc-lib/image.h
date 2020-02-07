// Images residing in RAM as vectors of pixels.
// Goal is to be simple and portable.

#ifndef __IMAGE_H
#define __IMAGE_H

#include <string>
#include <vector>
#include <cstdint>

#include "base/logging.h"

// 4-channel image in R-G-B-A order.
// uint32s are represented like 0xRRGGBBAA irrespective of native
// byte order.
struct ImageRGBA {
  using uint8 = uint8_t;
  using uint32 = uint32_t;
  ImageRGBA(const std::vector<uint8> &rgba, int width, int height);
  ImageRGBA(int width, int height);
  
  static ImageRGBA *Load(const std::string &filename);
  void Save(const std::string &filename) const;
  std::vector<uint8_t> SaveToVec() const;
  std::string SaveToString() const;
  
  ImageRGBA *Copy() const;

  // In RGBA order, where R value is MSB. x/y must be in bounds.
  uint32 GetPixel(int x, int y) const;
  // Clear the image to a single value.
  void Clear(uint8 r, uint8 g, uint8 b, uint8 a);
  void Clear32(uint32 rgba);

  void SetPixel(int x, int y, uint8 r, uint8 g, uint8 b, uint8 a);
  void SetPixel32(int x, int y, uint32 rgba);

  // Blend pixel with existing data.
  // Note: Currently assumes existing alpha is 0xFF.
  void BlendPixel(int x, int y, uint8 r, uint8 g, uint8 b, uint8 a);
  void BlendPixel32(int x, int y, uint32 rgba);
  
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
