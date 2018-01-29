#include "headless-graphics.h"

#include <vector>
#include <string>

#include "pftwo.h"

#include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image_write.h"

void SetPixel(int w, int h, int x, int y,
	      uint8 a, uint8 r, uint8 g, uint8 b,
	      vector<uint8> *argb) {
  int i = (w * y + x) * 4;
  (*argb)[i + 0] = a;
  (*argb)[i + 1] = r;
  (*argb)[i + 2] = g;
  (*argb)[i + 3] = b;
}

void SaveARGB(const vector<uint8> &argb, int width, int height,
	      const string &filename) {
  CHECK(argb.size() == width * height * 4);
  vector<uint8> rgba;
  rgba.resize(width * height * 4);
  for (int i = 0; i < width * height * 4; i += 4) {
    // This is all messed up, maybe because I got ARGB wrong in emu
    // (TODO: fix that) and also stb_ wants little-endian words. But
    // anyway, it works like this.
    uint8 b = argb[i + 0];
    uint8 g = argb[i + 1];
    uint8 r = argb[i + 2];
    uint8 a = argb[i + 3];
    
    rgba[i + 0] = r;
    rgba[i + 1] = g;
    rgba[i + 2] = b;
    rgba[i + 3] = a;
    // 3, 2, 1, 0   / 1 2 3 0 has alpha in the right place at least.
  }
  stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
}
