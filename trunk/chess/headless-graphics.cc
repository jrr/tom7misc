#include "headless-graphics.h"

#include <vector>
#include <string>
#include <cstdint>

// #include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image_write.h"
#include "../cc-lib/base/logging.h"

using namespace std;
using uint8 = uint8_t;

void SetPixel(int w, int h, int x, int y,
	      uint8 r, uint8 g, uint8 b, uint8 a,
	      vector<uint8> *argb) {
  int i = (w * y + x) * 4;
  (*argb)[i + 0] = r;
  (*argb)[i + 1] = g;
  (*argb)[i + 2] = b;
  (*argb)[i + 3] = a;
}

void FillRect(int w, int h,
	      int rectx, int recty, int rectw, int recth,
	      uint8 r, uint8 g, uint8 b, uint8 a,
	      vector<uint8> *rgba) {
  for (int yy = 0; yy < recth; yy++) {
    for (int xx = 0; xx < rectw; xx++) {
      SetPixel(w, h, rectx + xx, recty + yy, r, g, b, a, rgba);
    }
  }
}

void SaveARGB(const vector<uint8> &argb, int width, int height,
	      const string &filename) {
  CHECK(argb.size() == width * height * 4);
  vector<uint8> rgba;
  rgba.resize(width * height * 4);
  for (int i = 0; i < width * height * 4; i += 4) {
    uint8 a = argb[i + 0];
    uint8 r = argb[i + 1];
    uint8 g = argb[i + 2];
    uint8 b = argb[i + 3];
    
    rgba[i + 0] = r;
    rgba[i + 1] = g;
    rgba[i + 2] = b;
    rgba[i + 3] = a;
    // 3, 2, 1, 0   / 1 2 3 0 has alpha in the right place at least.
  }
  stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
}

void SaveRGBA(const vector<uint8> &rgba, int width, int height,
	      const string &filename) {
  CHECK(rgba.size() == width * height * 4);
  stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
}
