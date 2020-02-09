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
  void BlendPixel32(int x, int y, uint32 color);

  // Embedded 9x9 pixel font.
  // TODO: Support a 2x option.
  void BlendText(int x, int y,
		 uint8 r, uint8 g, uint8 b, uint8 a,
		 const std::string &s);
  void BlendText32(int x, int y, uint32 color, const std::string &s);

  // Clipped. Alpha blending.
  // This draws a crisp pixel line using Bresenham's algorithm.
  void BlendLine(int x1, int y1, int x2, int y2,
		 uint8 r, uint8 g, uint8 b, uint8 a);
  void BlendLine32(int x1, int y1, int x2, int y2, uint32 color);

  // Clipped. Alpha blending.
  // Blends an anti-aliased line using Wu's algorithm; slower.
  // Endpoints are pixel coordinates, but can be sub-pixel.
  void BlendLineAA(float x1, float y1, float x2, float y2,
		   uint8 r, uint8 g, uint8 b, uint8 a);
  void BlendLineAA32(float x1, float y1, float x2, float y2, uint32 color);

  
  const int width, height;
  // Size width * height * 4.
  std::vector<uint8> rgba;
};

// Single-channel bitmap.
struct ImageA {
  using uint8 = uint8_t;
  ImageA(const std::vector<uint8> &alpha, int width, int height);

  ImageA *Copy() const;

  // TODO: Text drawing is easy here!
  
  const int width, height;
  // Size width * height.
  std::vector<uint8> alpha;
};

#endif
