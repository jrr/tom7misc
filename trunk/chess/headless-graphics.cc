#include "headless-graphics.h"

#include <vector>
#include <string>
#include <cstdint>
#include <memory>

#include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image_write.h"
#include "../cc-lib/base/logging.h"

using namespace std;
using uint8 = uint8_t;
using uint32 = uint32_t;

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

HeadlessFont::HeadlessFont(int char_width,
			   int char_height,
			   int styles,
			   int overlap) :
  char_width(char_width), char_height(char_height),
  styles(styles), overlap(overlap) {

}

HeadlessFont *HeadlessFont::Create(const std::string &filename,
				   const std::string &charmap,
				   int char_width,
				   int char_height,
				   int styles,
				   int overlap) {
  std::unique_ptr<HeadlessFont> hf{
    new HeadlessFont{char_width, char_height, styles, overlap}};

  int bpp = 0;
  uint8 *stb_rgba = stbi_load(filename.c_str(),
			      &hf->font_width,
			      &hf->font_height, &bpp, 4);
  if (stb_rgba == nullptr) return nullptr;
  if (bpp != 4) return nullptr;
  
  hf->font.reserve(hf->font_width * hf->font_height * 4);
  for (int i = 0; i < hf->font_width * hf->font_height * 4; i++) {
    hf->font.push_back(stb_rgba[i]);
  }
  stbi_image_free(stb_rgba);

  // TODO: Can support multiple dimmings, scales here.
  // Since this is headless, there is less need for computing these
  // up front. Could just do it dynamically.
  
  for (int j = 0; j < 256; j++) {
    hf->chars[j] = -1;
  }

  if (charmap.length() > 256) return nullptr;
  for (int i = 0; i < charmap.length(); i++) {
    hf->chars[(uint8)charmap[i]] = i;
  }

  return hf.release();
}

void HeadlessFont::DrawPlain(int x, int y,
			     const std::string &s,
			     std::vector<uint8_t> *rgba,
			     int width, int height) {
  int xx = x;
  for (int i = 0; i < s.size(); i++) {
    DrawChar(s[i], 0, xx, y, rgba, width, height);
    xx += char_width;
    xx -= overlap;
  }
}

// Draw one character to the surface, clipping and blending.
// Char is user-facing char, not yet translated.
// The style must be in bounds for the font.
// Note: Blends alpha but assumes dest has alpha=255.
void HeadlessFont::DrawChar(uint8 c, int style,
			    int dstx, int dsty,
			    vector<uint8> *rgba,
			    int width, int height) {
  const int fontchar = chars[c];
  // Unmapped.
  if (fontchar < 0) return;
  
  int srcx = fontchar * char_width;
  int srcy = style * char_height;
  for (int y = 0; y < char_height; y++) {
    const int syy = srcy + y;
    const int dyy = dsty + y;
    // Off bottom of image?
    if (dyy >= height) break;
    if (dyy < 0) continue;
    for (int x = 0; x < char_width; x++) {
      const int sxx = srcx + x;
      const int dxx = dstx + x;
      if (dxx >= width) break;
      if (dxx < 0) continue;
      
      const int sidx = (syy * font_width + sxx) * 4;
      const uint8 sr = font[sidx + 0];
      const uint8 sg = font[sidx + 1];
      const uint8 sb = font[sidx + 2];
      const uint8 sa = font[sidx + 3];

      const int didx = (dyy * width + dxx) * 4;
      const uint8 dr = (*rgba)[didx + 0];
      const uint8 dg = (*rgba)[didx + 1];
      const uint8 db = (*rgba)[didx + 2];
      // const uint8 da = (*rgba)[didx + 3];

      // TODO: Figure out how to blend when dest is also transparent.

      // so a + oma = 255.
      const uint32 oma = 0xFF - sa;
      
      // Terrible performance, obviously. Can we divide by 256?
      // we want (r * a/255) + (oldr * (1-a)/255),
      // which is (r * a)/255 + (oldr * (1-a))/255
      // which is (r * a + oldr * (1-a))/255
      uint32 rr = (((uint32)sr * (uint32)sa) + (dr * oma)) / 0xFF;
      if (rr > 0xFF) rr = 0xFF;
      
      uint32 gg = (((uint32)sg * (uint32)sa) + (dg * oma)) / 0xFF;
      if (gg > 0xFF) gg = 0xFF;
      
      uint32 bb = (((uint32)sb * (uint32)sa) + (db * oma)) / 0xFF;
      if (bb > 0xFF) bb = 0xFF;
      
      (*rgba)[didx + 0] = rr;
      (*rgba)[didx + 1] = gg;
      (*rgba)[didx + 2] = bb;
      (*rgba)[didx + 3] = 0xFF;
    }
  }
}
