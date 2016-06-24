#include "../cc-lib/sdl/sdlutil.h"
#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"

#include <CL/cl.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <algorithm>
#include <tuple>
#include <utility>
#include <set>
#include <vector>
#include <map>
#include <unordered_set>
#include <deque>

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/util.h"
#include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image_write.h"
#include "../cc-lib/vector-util.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/color-util.h"
#include "../cc-lib/base/macros.h"

#include "clutil.h"
#include "timer.h"

using namespace std;

using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

// NES NTSC Palette, in RGB triplets.
static constexpr uint8 ntsc_palette[64 * 3] = {
  0x80,0x80,0x80, 0x00,0x3D,0xA6, 0x00,0x12,0xB0, 0x44,0x00,0x96,
  0xA1,0x00,0x5E, 0xC7,0x00,0x28, 0xBA,0x06,0x00, 0x8C,0x17,0x00,
  0x5C,0x2F,0x00, 0x10,0x45,0x00, 0x05,0x4A,0x00, 0x00,0x47,0x2E,
  0x00,0x41,0x66, 0x00,0x00,0x00, 0x05,0x05,0x05, 0x05,0x05,0x05,
  0xC7,0xC7,0xC7, 0x00,0x77,0xFF, 0x21,0x55,0xFF, 0x82,0x37,0xFA,
  0xEB,0x2F,0xB5, 0xFF,0x29,0x50, 0xFF,0x22,0x00, 0xD6,0x32,0x00,
  0xC4,0x62,0x00, 0x35,0x80,0x00, 0x05,0x8F,0x00, 0x00,0x8A,0x55,
  0x00,0x99,0xCC, 0x21,0x21,0x21, 0x09,0x09,0x09, 0x09,0x09,0x09,
  0xFF,0xFF,0xFF, 0x0F,0xD7,0xFF, 0x69,0xA2,0xFF, 0xD4,0x80,0xFF,
  0xFF,0x45,0xF3, 0xFF,0x61,0x8B, 0xFF,0x88,0x33, 0xFF,0x9C,0x12,
  0xFA,0xBC,0x20, 0x9F,0xE3,0x0E, 0x2B,0xF0,0x35, 0x0C,0xF0,0xA4,
  0x05,0xFB,0xFF, 0x5E,0x5E,0x5E, 0x0D,0x0D,0x0D, 0x0D,0x0D,0x0D,
  0xFF,0xFF,0xFF, 0xA6,0xFC,0xFF, 0xB3,0xEC,0xFF, 0xDA,0xAB,0xEB,
  0xFF,0xA8,0xF9, 0xFF,0xAB,0xB3, 0xFF,0xD2,0xB0, 0xFF,0xEF,0xA6,
  0xFF,0xF7,0x9C, 0xD7,0xE8,0x95, 0xA6,0xED,0xAF, 0xA2,0xF2,0xDA,
  0x99,0xFF,0xFC, 0xDD,0xDD,0xDD, 0x11,0x11,0x11, 0x11,0x11,0x11,
};

// Graphics.
#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr;
#define WIDTH 64
#define HEIGHT 1
#define SCALE 5
#define SCREENW (WIDTH << SCALE)
#define SCREENH (HEIGHT << SCALE)
static SDL_Surface *screen = nullptr;

std::mutex print_mutex;
#define Printf(fmt, ...) do {		\
  MutexLock Printf_ml(&print_mutex);		\
  printf(fmt, ##__VA_ARGS__);			\
  } while (0);

struct ImageRGBA {
  static ImageRGBA *Load(const string &filename) {
    vector<uint8> ret;
    int width, height, bpp_unused;
    uint8 *stb_rgba = stbi_load(filename.c_str(), 
				&width, &height, &bpp_unused, 4);
    const int bytes = width * height * 4;
    ret.resize(bytes);
    if (stb_rgba == nullptr) return nullptr;
    memcpy(ret.data(), stb_rgba, bytes);
    // Does this move image data all the way in, or do we need to
    // write a move constructor manually? Better way?
    return new ImageRGBA(std::move(ret), width, height);
  }

  ImageRGBA(const vector<uint8> &rgba, int width, int height) 
    : width(width), height(height), rgba(rgba) {
    CHECK(rgba.size() == width * height * 4);
  }

  void Save(const string &filename) {
    CHECK(rgba.size() == width * height * 4);
    stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
  }

  ImageRGBA *Copy() const {
    return new ImageRGBA(rgba, width, height);
  }

  // TODO:: Save a vector of images of the same size in a grid.
  const int width, height;
  vector<uint8> rgba;
};

static uint8 FloatByte(float f) {
  if (f <= 0.0f) return 0;
  if (f >= 1.0f) return 255;
  else return f * 255.0;
}

static constexpr inline float ByteFloat(uint8 ch) {
  constexpr float inv255 = 1.0f / 255.0f;
  return ch * inv255;
}

struct Pixel {
  Pixel(uint8 idx, uint8 r, uint8 g, uint8 b) : idx(idx), r(r), g(g), b(b) {}
  // NES palette entry in [0, 63].
  uint8 idx;
  // Actual color value.
  uint8 r, g, b;
};
vector<Pixel> pixels{WIDTH * HEIGHT, Pixel{0, 0, 0, 0}};
// delta_e[i * 64 + j] gives the distance between the nes palette entries in LAB space.
float delta_e[64 * 64];

// Give "pixel" coordinates; scales up into a chunky pixel.
inline void DrawChunk(int x, int y, const Pixel &p) {
#if SCALE < 2
  for (int yy = 0; yy < (1 << SCALE); yy++) {
    for (int xx = 0; xx < (1 << SCALE); xx++) {
      sdlutil::drawpixel(screen, (x << SCALE) + xx, (y << SCALE) + yy, p.r, p.g, p.b);
    }
  }
#else
  sdlutil::FillRectRGB(screen,
		       x << SCALE, y << SCALE,
		       1 << SCALE, 1 << SCALE,
		       p.r, p.g, p.b);
  #if SCALE >= 4
  font->drawto_plain(screen, (x << SCALE) + 1, (y << SCALE) + 1, StringPrintf("%02x", p.idx));
  #endif
#endif
}

inline void UpdatePixel(int x, int y) {
  const Pixel &p = pixels[y * WIDTH + x];
  DrawChunk(x, y, p);
}

// Do two rectangles of the same size overlap?
// XXX might be off-by-one problems here
inline bool Overlap(int w, int h,
		    int ax, int ay,
		    int bx, int by) {
  const int axx = ax + w, ayy = ay + h;
  const int bxx = bx + w, byy = by + h;

  return !(ax > bxx || bx > axx ||
	   ay < byy || by < ayy);
}

static void Redraw() {
  for (int py = 0; py < HEIGHT; py++) {
    for (int px = 0; px < WIDTH; px++) {
      UpdatePixel(px, py);
    }
  }
}

static float Error() {
  float err = 0.0f;
  for (int i = 0; i < WIDTH * HEIGHT; i++) {
    const int ix = i % WIDTH;
    const int iy = i / WIDTH;
    const int irow = pixels[i].idx * WIDTH;
    for (int j = i + 1; j < WIDTH * HEIGHT; j++) {
      const int jx = j % WIDTH;
      const int jy = j / WIDTH;
      const int dx = ix - jx;
      const int dy = iy - jy;
      // Inverse square law, so don't bother taking square root.
      const float sqdist = dx * dx + dy * dy;
      err += delta_e[irow + pixels[j].idx] / sqdist;
    }
  }
  return err;
}

static void UIThread() {
  ArcFour rc("ui");
  
  Printf("Init NES palette.\n");
  for (int i = 0; i < 64; i++) {
    pixels[i] = Pixel{(uint8)i,
		      ntsc_palette[i * 3 + 0],
		      ntsc_palette[i * 3 + 1],
		      ntsc_palette[i * 3 + 2]};
  }

  Printf("Init delta_e table.\n");
  auto GetLab = [](int i, float *l, float *a, float *b) {
    ColorUtil::RGBToLAB(ByteFloat(pixels[i].r),
			ByteFloat(pixels[i].g),
			ByteFloat(pixels[i].b),
			l, a, b);
  };
  for (int i = 0; i < 64; i++) {
    float il, ia, ib;
    GetLab(i, &il, &ia, &ib);
    for (int j = 0; j < 64; j++) {
      float jl, ja, jb;
      GetLab(j, &jl, &ja, &jb);
      delta_e[i * 64 + j] = ColorUtil::DeltaE(il, ia, ib, jl, ja, jb);
    }
  }
  
  Printf("Initial draw:\n");
  Redraw();

  int swaps = 0, segments = 0;;
  auto SwapPixels = [&swaps](int ax, int ay, int bx, int by) {
    swaps++;
    const Pixel tmp = pixels[ay * WIDTH + ax];
    pixels[ay * WIDTH + ax] = pixels[by * WIDTH + bx];
    pixels[by * WIDTH + bx] = tmp;
    UpdatePixel(ax, ay);
    UpdatePixel(bx, by);
  };

  int frame_num = 0;

  float current_error = Error();
  for (int round = 0; ; round++) {
    int old_swaps = swaps;
    if (round & 1) {
      for (int s = 0; s < 100; s++) {
	int i = RandTo(&rc, WIDTH * HEIGHT);
	int j = RandTo(&rc, WIDTH * HEIGHT);
	if (i != j) {
	  // Try swapping.
	  const float before = current_error;
	  const Pixel tmp = pixels[i];
	  pixels[i] = pixels[j];
	  pixels[j] = tmp;
	  const float after = Error();
	  if (after < before) {
	    current_error = after;
	    swaps++;
	  } else {
	    // swap back.
	    const Pixel tmp = pixels[i];
	    pixels[i] = pixels[j];
	    pixels[j] = tmp;
	  }
	}
      }
    } else {
      // Try moving a single segment. wlog the swap can be
      // defined by three points A <= B <= C
      //
      //     A          B         C  
      //  +-------------------------------------------+
      //  |  |##########|         |                   |
      //  +-------------------------------------------+
      //
      //     AB         C           
      //  +-------------------------------------------+
      //  |  ||         |##########                   |
      //  +-------------------------------------------+

      int a = RandTo(&rc, WIDTH);
      int b = RandTo(&rc, WIDTH);
      int c = RandTo(&rc, WIDTH);

      // TODO: Would be nice to have a library for sorting fixed-size
      // "arrays" like this, though obviously this is not the bottleneck
      // here.
      if (c < b) std::swap(b, c);
      if (b < a) std::swap(a, b);
      // now a <= b and a <= c
      if (c < b) std::swap(b, c);
      // Now a <= b <= c
      CHECK_LE(a, b);
      CHECK_LE(b, c);
      CHECK_LE(a, c);
      
      int w = b - a;
      if (w > 0) {
	vector<Pixel> newpixels;
	newpixels.reserve(pixels.size());
	for (int i = 0; i < a; i++) newpixels.push_back(pixels[i]);
	for (int i = b; i < c; i++) newpixels.push_back(pixels[i]);
	for (int i = a; i < b; i++) newpixels.push_back(pixels[i]);
	for (int i = c; i < pixels.size(); i++) newpixels.push_back(pixels[i]);

	pixels.swap(newpixels);
	const float after = Error();
	if (after < current_error) {
	  swaps += w;
	  segments++;
	  current_error = after;
	} else {
	  // undo
	  newpixels.swap(pixels);
	}
      }
    }

    if (old_swaps != swaps) {
      printf("%.4f error. %d swaps. %d segs.\n", current_error, swaps, segments);
      Redraw();
      SDL_Flip(screen);
    }
      
    SDL_Event event;
    if (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
	Printf("QUIT.\n");
	return;

      } else if (event.type == SDL_KEYDOWN) {
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  Printf("ESCAPE.\n");
	  return;
	default:;
	}
      }
    } 
  }
}

int SDL_main(int argc, char **argv) {
  /*
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  */
  
  /* Initialize SDL and network, if we're using it. */
  CHECK(SDL_Init(SDL_INIT_VIDEO |
		 SDL_INIT_TIMER | 
		 SDL_INIT_AUDIO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);

  font = Font::create(screen,
		      "font.png",
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";
  
  UIThread();
  
  SDL_Quit();
  return 0;
}
