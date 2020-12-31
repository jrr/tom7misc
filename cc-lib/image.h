// Images residing in RAM as vectors of pixels.
// Goal is to be simple and portable.

#ifndef _CC_LIB_IMAGE_H
#define _CC_LIB_IMAGE_H

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
  static ImageRGBA *LoadFromMemory(const std::vector<uint8> &bytes);
  static ImageRGBA *LoadFromMemory(const char *data, size_t size);
  // Saves in RGBA PNG format. Returns true if successful.
  bool Save(const std::string &filename) const;
  std::vector<uint8> SaveToVec() const;
  std::string SaveToString() const;

  // Quality in [1, 100]. Returns true if successful.
  bool SaveJPG(const std::string &filename, int quality = 90) const;
  // TODO: jpg to vec, to string

  ImageRGBA *Copy() const;
  // Crop (or pad), returning a new image of the given width and height.
  // If this includes any area outside the input image, fill with
  // fill_color.
  ImageRGBA Crop32(int x, int y, int w, int h,
		   uint32 fill_color = 0x00000000) const;
  
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

  // Blend a filled rectangle. Clips.
  void BlendRect(int x, int y, int w, int h,
		 uint8 r, uint8 g, uint8 b, uint8 a);
  void BlendRect32(int x, int y, int w, int h, uint32 color);
  
  // Embedded 9x9 pixel font.
  void BlendText(int x, int y,
		 uint8 r, uint8 g, uint8 b, uint8 a,
		 const std::string &s);
  void BlendText32(int x, int y, uint32 color, const std::string &s);

  // Same font, but scaled to (crisp) 2x2 pixels.
  void BlendText2x(int x, int y,
		   uint8 r, uint8 g, uint8 b, uint8 a,
		   const std::string &s);
  void BlendText2x32(int x, int y, uint32 color, const std::string &s);
  
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

  // Clipped, alpha blending.
  void BlendImage(int x, int y, const ImageRGBA &other);
  
  const int width, height;
  // Size width * height * 4.
  std::vector<uint8> rgba;
};

// Single-channel bitmap.
struct ImageA {
  using uint8 = uint8_t;
  ImageA(const std::vector<uint8> &alpha, int width, int height);
  ImageA(int width, int height);
  
  ImageA *Copy() const;
  // Generally appropriate for enlarging, not shrinking.
  ImageA ResizeBilinear(int new_width, int new_height) const;
  
  // TODO: Text drawing is easy here!

  void Clear(uint8 value);
  
  // Clipped.
  inline void SetPixel(int x, int y, uint8 v);
  // x/y must be in bounds.
  inline uint8 GetPixel(int x, int y) const;

  // Treats the input pixels as being "located" at their top-left
  // corners (not their centers).
  // x/y out of bounds will repeat edge pixels.
  float SampleBilinear(float x, float y) const;
  
  const int width, height;
  // Size width * height.
  std::vector<uint8> alpha;
};


// Implementations follow.

uint8_t ImageA::GetPixel(int x, int y) const {
  return alpha[y * width + x];
}

void ImageA::SetPixel(int x, int y, uint8_t value) {
  if (x < 0 || y < 0) return;
  if (x >= width || y >= height) return;
  alpha[y * width + x] = value;
}


#endif
