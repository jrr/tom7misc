#include "graphics.h"

#include <vector>
#include <string>

#include "pftwo.h"

#include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image_write.h"

// assumes ARGB, surfaces exactly the same size, etc.
void CopyARGB(const vector<uint8> &argb, SDL_Surface *surface) {
  // int bpp = surface->format->BytesPerPixel;
  Uint8 * p = (Uint8 *)surface->pixels;
  memcpy(p, argb.data(), surface->w * surface->h * 4);
}

void BlitARGB(const vector<uint8> &argb, int w, int h,
	      int x, int y, SDL_Surface *surface) {
  for (int i = 0; i < h; i++) {
    int yy = y + i;
    Uint8 *p = (Uint8 *)surface->pixels +
      (surface->w * 4 * yy) + x * 4;
    memcpy(p, argb.data() + (i * w * 4), w * 4);
  }
}

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

void HalveARGB(const vector<uint8> &argb, int width, int height,
	       SDL_Surface *surface) {
  // PERF
  const int halfwidth = width >> 1;
  const int halfheight = height >> 1;
  vector<uint8> argb_half;
  argb_half.resize(halfwidth * halfheight * 4);
  for (int y = 0; y < halfheight; y++) {
    for (int x = 0; x < halfwidth; x++) {
      #define PIXEL(i) \
	argb_half[(y * halfwidth + x) * 4 + (i)] =	\
	  Mix4(argb[(y * 2 * width + x * 2) * 4 + (i)], \
	       argb[(y * 2 * width + x * 2 + 1) * 4 + (i)], \
	       argb[((y * 2 + 1) * width + x * 2) * 4 + (i)],	\
	       argb[((y * 2 + 1) * width + x * 2 + 1) * 4 + (i)])
      PIXEL(0);
      PIXEL(1);
      PIXEL(2);
      PIXEL(3);
      #undef PIXEL
    }
  }
  CopyARGB(argb_half, surface);
}

void BlitARGBHalf(const vector<uint8> &argb, int width, int height,
		  int xpos, int ypos,
		  SDL_Surface *surface) {
  const int halfwidth = width >> 1;
  const int halfheight = height >> 1;
  // argb_half.resize(halfwidth * halfheight * 4);
  for (int y = 0; y < halfheight; y++) {
    int yy = ypos + y;
    Uint8 *p = (Uint8 *)surface->pixels +
      (surface->w * 4 * yy) + xpos * 4;

    for (int x = 0; x < halfwidth; x++) {
      #define PIXEL(i) \
	*p = \
	  Mix4(argb[(y * 2 * width + x * 2) * 4 + (i)], \
	       argb[(y * 2 * width + x * 2 + 1) * 4 + (i)], \
	       argb[((y * 2 + 1) * width + x * 2) * 4 + (i)],	\
	       argb[((y * 2 + 1) * width + x * 2 + 1) * 4 + (i)]); \
	p++
      PIXEL(0);
      PIXEL(1);
      PIXEL(2);
      PIXEL(3);
      #undef PIXEL
    }
  }
  // CopyARGB(argb_half, surface);
}

// Map a pixel to a Uint32 value for the given pixel format.
// Assumes 32-bit pixel format.
inline Uint32 MakePixel(const SDL_PixelFormat *fmt,
			uint8 r, uint8 g, uint8 b, uint8 a) {
  return ((Uint32)r << fmt->Rshift) |
    ((Uint32)g << fmt->Gshift) |
    ((Uint32)b << fmt->Bshift) |
    // Sometimes (like when writing to the screen buffer on windows),
    // the alpha channel is empty, and its shift value is the same as
    // another color channel (e.g. blue). So AND with the mask before
    // writing it, which prevents a full-alpha channel from leaking
    // into other channels if this is the case.
    (((Uint32)a << fmt->Ashift) & fmt->Amask);
}

void BlitRGBA2x(const vector<uint8> &rgba, int w, int h,
		int x, int y, SDL_Surface *surface) {
  const SDL_PixelFormat *fmt = surface->format;
  for (int i = 0; i < h; i++) {
    int yy = y + i * 2;
    Uint32 *p1 = (Uint32 *)((Uint8 *)surface->pixels +
			    (surface->w * 4 * yy) + x * 4);
    Uint32 *p2 = (Uint32 *)((Uint8 *)surface->pixels +
			    (surface->w * 4 * (yy + 1)) + x * 4);
    // Write two identical lines (to p1, p2).
    // Write each pixel (from inp) twice.
    const uint8 *inp = (rgba.data() + (i * w * 4));
    for (int j = 0; j < w; j++) {
      Uint32 c = MakePixel(fmt, inp[0], inp[1], inp[2], inp[3]);
      *p1 = c; p1++;
      *p1 = c; p1++;
      *p2 = c; p2++;
      *p2 = c; p2++;
      inp += 4;
    }
  }
 
}
