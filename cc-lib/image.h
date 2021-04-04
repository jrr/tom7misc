// Images residing in RAM as vectors of pixels.
// Goal is to be simple and portable.

#ifndef _CC_LIB_IMAGE_H
#define _CC_LIB_IMAGE_H

#include <string>
#include <vector>
#include <cstdint>
#include <tuple>
#include <optional>
#include <algorithm>

#include "base/logging.h"

struct ImageA;

// 4-channel image in R-G-B-A order.
// uint32s are represented like 0xRRGGBBAA irrespective of native
// byte order.
struct ImageRGBA {
  using uint8 = uint8_t;
  using uint32 = uint32_t;
  ImageRGBA(const std::vector<uint8> &rgba, int width, int height);
  ImageRGBA(int width, int height);
  ImageRGBA() : width(0), height(0) {}

  // TODO: copy/assignment

  int Width() const { return width; }
  int Height() const { return height; }


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

  // TODO: Replace with copy constructor
  ImageRGBA *Copy() const;
  // Crop (or pad), returning a new image of the given width and height.
  // If this includes any area outside the input image, fill with
  // fill_color.
  ImageRGBA Crop32(int x, int y, int w, int h,
                   uint32 fill_color = 0x00000000) const;

  // Scale by a positive integer factor, crisp pixels.
  ImageRGBA ScaleBy(int scale) const;

  // In RGBA order, where R value is MSB. x/y must be in bounds.
  inline uint32 GetPixel32(int x, int y) const;
  inline std::tuple<uint8, uint8, uint8, uint8> GetPixel(int x, int y) const;

  // Clear the image to a single value.
  void Clear(uint8 r, uint8 g, uint8 b, uint8 a);
  void Clear32(uint32 rgba);

  inline void SetPixel(int x, int y, uint8 r, uint8 g, uint8 b, uint8 a);
  inline void SetPixel32(int x, int y, uint32 rgba);

  // Blend pixel with existing data.
  // Note: Currently assumes existing alpha is 0xFF.
  void BlendPixel(int x, int y, uint8 r, uint8 g, uint8 b, uint8 a);
  void BlendPixel32(int x, int y, uint32 color);

  // Blend a filled rectangle. Clips.
  void BlendRect(int x, int y, int w, int h,
                 uint8 r, uint8 g, uint8 b, uint8 a);
  void BlendRect32(int x, int y, int w, int h, uint32 color);

  // Hollow box, one pixel width.
  // nullopt corner_color = color for a crisp box, but setting
  // the corners to 50% alpha makes a subtle roundrect effect.
  void BlendBox32(int x, int y, int w, int h,
                  uint32 color, std::optional<uint32> corner_color);

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
  // Clipped, but copy source alpha and ignore current image contents.
  void CopyImage(int x, int y, const ImageRGBA &other);

  // Extract single channel.
  ImageA Red() const;
  ImageA Green() const;
  ImageA Blue() const;
  ImageA Alpha() const;

  // Images must all be the same dimensions.
  static ImageRGBA FromChannels(const ImageA &red,
                                const ImageA &green,
                                const ImageA &blue,
                                const ImageA &alpha);

private:
  int width, height;
  // Size width * height * 4.
  std::vector<uint8> rgba;
};

// Single-channel 8-bit bitmap.
struct ImageA {
  using uint8 = uint8_t;
  ImageA(const std::vector<uint8> &alpha, int width, int height);
  ImageA(int width, int height);
  // Empty image sometimes useful for filling vectors, etc.
  ImageA() : ImageA(0, 0) {}
  // Value semantics.
  ImageA(const ImageA &other) = default;
  ImageA(ImageA &&other) = default;
  ImageA &operator =(const ImageA &other) = default;
  ImageA &operator =(ImageA &&other) = default;

  bool operator ==(const ImageA &other) const;
  std::size_t Hash() const;

  int Width() const { return width; }
  int Height() const { return height; }

  ImageA *Copy() const;
  // Scale by a positive integer factor, crisp pixels.
  ImageA ScaleBy(int scale) const;
  // Generally appropriate for enlarging, not shrinking.
  ImageA ResizeBilinear(int new_width, int new_height) const;
  // Nearest-neighbor; works "fine" for enlarging and shrinking.
  ImageA ResizeNearest(int new_width, int new_height) const;
  // Make a four-channel image of the same size, R=v, G=v, B=v, A=0xFF.
  ImageRGBA GreyscaleRGBA() const;
  // Make a four-channel alpha mask of the same size, RGB=rgb, A=v
  ImageRGBA AlphaMaskRGBA(uint8 r, uint8 g, uint8 b) const;

  // Only increases values.
  void BlendText(int x, int y, uint8 v, const std::string &s);

  void Clear(uint8 value);

  void BlendImage(int x, int y, const ImageA &other);

  // Clipped.
  inline void SetPixel(int x, int y, uint8 v);
  // x/y must be in bounds.
  inline uint8 GetPixel(int x, int y) const;
  inline void BlendPixel(int x, int y, uint8 v);

  // Output value is a float in [0, 255].
  // Treats the input pixels as being "located" at their top-left
  // corners (not their centers).
  // x/y out of bounds will repeat edge pixels.
  float SampleBilinear(float x, float y) const;

private:
  int width, height;
  // Size width * height.
  std::vector<uint8> alpha;
};

// Like ImageA, but float pixel values (clipped to [0,1]).
struct ImageF {
  ImageF(const std::vector<float> &alpha, int width, int height);
  ImageF(int width, int height);
  ImageF() : ImageF(0, 0) {}
  explicit ImageF(const ImageA &other);
  // Value semantics.
  ImageF(const ImageF &other) = default;
  ImageF(ImageF &&other) = default;
  ImageF &operator =(const ImageF &other) = default;
  ImageF &operator =(ImageF &&other) = default;

  int Width() const { return width; }
  int Height() const { return height; }

  // Generally appropriate for enlarging, not shrinking.
  ImageF ResizeBilinear(int new_width, int new_height) const;

  // Convert to 8-bit ImageA, rounding.
  ImageA Make8Bit() const;

  // Only increases values.
  void BlendText(int x, int y, float v, const std::string &s);

  void Clear(float value);

  // Clipped.
  inline void SetPixel(int x, int y, float v);
  // x/y must be in bounds.
  inline float GetPixel(int x, int y) const;
  inline void BlendPixel(int x, int y, float v);

  // Treats the input pixels as being "located" at their top-left
  // corners (not their centers).
  // x/y out of bounds will repeat edge pixels.
  float SampleBilinear(float x, float y) const;
  // Same but with an implied value (usually 0 or 1) outside the
  // image.
  float SampleBilinear(float x, float y, float outside_value) const;

private:
  int width, height;
  // Size width * height.
  std::vector<float> alpha;
};


// Implementations follow.


ImageRGBA::uint32 ImageRGBA::GetPixel32(int x, int y) const {
  // Treat out-of-bounds reads as containing 00,00,00,00.
  if (x < 0 || x >= width ||
      y < 0 || y >= height) return 0;
  const int base = (y * width + x) << 2;
  const uint8_t r = rgba[base + 0];
  const uint8_t g = rgba[base + 1];
  const uint8_t b = rgba[base + 2];
  const uint8_t a = rgba[base + 3];
  return ((uint32)r << 24) | ((uint32)g << 16) | ((uint32)b << 8) | (uint32)a;
}

std::tuple<uint8_t, uint8_t, uint8_t, uint8_t>
ImageRGBA::GetPixel(int x, int y) const {
  // Treat out-of-bounds reads as containing 00,00,00,00.
  if (x < 0 || x >= width ||
      y < 0 || y >= height) return std::make_tuple(0, 0, 0, 0);
  const int base = (y * width + x) << 2;
  return std::make_tuple(
      rgba[base], rgba[base + 1], rgba[base + 2], rgba[base + 3]);
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


uint8_t ImageA::GetPixel(int x, int y) const {
  return alpha[y * width + x];
}

void ImageA::SetPixel(int x, int y, uint8_t value) {
  if (x < 0 || y < 0) return;
  if (x >= width || y >= height) return;
  alpha[y * width + x] = value;
}

void ImageA::BlendPixel(int x, int y, uint8_t v) {
  if (x < 0 || y < 0) return;
  if (x >= width || y >= height) return;
  // XXX test this blending math
  uint8_t old = GetPixel(x, y);
  uint16_t opaque_part = 255 * v;
  uint16_t transparent_part = (255 - v) * old;
  uint8_t new_value =
    0xFF & ((opaque_part + transparent_part) / (uint16_t)255);
  SetPixel(x, y, new_value);
}


float ImageF::GetPixel(int x, int y) const {
  return alpha[y * width + x];
}

void ImageF::SetPixel(int x, int y, float value) {
  if (x < 0 || y < 0) return;
  if (x >= width || y >= height) return;
  alpha[y * width + x] = std::clamp(value, 0.0f, 1.0f);
}

void ImageF::BlendPixel(int x, int y, float value) {
  if (x < 0 || y < 0) return;
  if (x >= width || y >= height) return;
  const float old = GetPixel(x, y);
  const float opaque_part = value;
  const float transparent_part = (1.0f - value) * old;
  SetPixel(x, y, opaque_part + transparent_part);
}


#endif
