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
#include "../cc-lib/lines.h"
#include "../cc-lib/image.h"

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

static constexpr uint8 best_permutation[] = {
  0x2a, 0x29, 0x38, 0x28, 0x27, 0x26, 0x16, 0x18,
  0x3a, 0x39, 0x37, 0x36, 0x35, 0x15, 0x17, 0x06,
  0x30, 0x20, 0x3d, 0x34, 0x24, 0x25, 0x05, 0x07,
  0x3c, 0x31, 0x32, 0x33, 0x23, 0x14, 0x04, 0x08,
  0x3b, 0x2c, 0x21, 0x22, 0x12, 0x13, 0x02, 0x03,
  0x2b, 0x10, 0x1c, 0x11, 0x01, 0x0c, 0x2e, 0x1e,
  0x1a, 0x1b, 0x00, 0x2d, 0x1d, 0x3f, 0x1f, 0x0e,
  0x19, 0x0a, 0x09, 0x0b, 0x3e, 0x2f, 0x0f, 0x0d,
};

static constexpr uint8 smoothest_permutation[] = {
0x2c, 0x3c, 0x20, 0x21, 0x22, 0x23, 0x24, 0x13,
0x2b, 0x31, 0x30, 0x32, 0x1c, 0x34, 0x14, 0x12,
0x3a, 0x3b, 0x3d, 0x10, 0x33, 0x25, 0x11, 0x02,
0x2a, 0x39, 0x36, 0x1b, 0x35, 0x04, 0x01, 0x03,
0x38, 0x37, 0x19, 0x00, 0x2d, 0x0c, 0x1d, 0x2e,
0x29, 0x1a, 0x18, 0x05, 0x08, 0x0b, 0x3f, 0x2f,
0x28, 0x26, 0x15, 0x07, 0x09, 0x3e, 0x1e, 0x1f,
0x27, 0x16, 0x17, 0x06, 0x0a, 0x0f, 0x0e, 0x0d,
};

// Graphics.
#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr;
#define WIDTH 4
#define HEIGHT 4
#define DEPTH 4

#define SCALE 6
#define SCREENW (WIDTH << SCALE)
#define SCREENH (HEIGHT << SCALE)
static SDL_Surface *screen = nullptr;

std::mutex print_mutex;
#define Printf(fmt, ...) do {		\
  MutexLock Printf_ml(&print_mutex);		\
  printf(fmt, ##__VA_ARGS__);			\
  } while (0);

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
float max_delta_e = 0.0f;

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

static float Unsmoothness() {
  float err = 0.0f;
  for (int i = 0; i < WIDTH * HEIGHT; i++) {
    // Reference color.
    const int ix = i % WIDTH;
    const int iy = i / WIDTH;
    const int irow = pixels[i].idx * WIDTH * HEIGHT;
    for (int j = 0; j < WIDTH * HEIGHT; j++) {
      // Test color.
      if (i == j) continue;
      const int jx = j % WIDTH;
      const int jy = j / WIDTH;

      // Now this is unsmooth if any cells along the vector
      // have an error higher than the current error.
      const float start_de = delta_e[irow + pixels[j].idx];

      for (const std::pair<int, int> point : Line<int>{jx, jy, ix, iy}) {
	// Includes first point, but that's fine (deltadelta = 0).
	const int x = point.first;
	const int y = point.second;
	const float cell_de = delta_e[irow + pixels[y * WIDTH + x].idx];
	if (cell_de > start_de) {
	  err += (cell_de - start_de);
	}
      }
    }
  }

  return err;
}

static float Dissimilarity() {
  float err = 0.0f;
  for (int i = 0; i < WIDTH * HEIGHT; i++) {
    const int ix = i % WIDTH;
    const int iy = i / WIDTH;
    const int irow = pixels[i].idx * WIDTH * HEIGHT;
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

static float Error() {
  return Unsmoothness();
  // return Dissimilarity(); // Unsmoothness();
}

static void UIThread() {
  ArcFour rc("ui");

  Printf("Init delta_e table.\n");
  auto GetLab = [](int i, float *l, float *a, float *b) {
    ColorUtil::RGBToLAB(ntsc_palette[i * 3 + 0] / 255.0f,
			ntsc_palette[i * 3 + 1] / 255.0f,
			ntsc_palette[i * 3 + 2] / 255.0f,
			l, a, b);
  };
  for (int i = 0; i < 64; i++) {
    float il, ia, ib;
    GetLab(i, &il, &ia, &ib);
    for (int j = 0; j < 64; j++) {
      float jl, ja, jb;
      GetLab(j, &jl, &ja, &jb);
      float de = ColorUtil::DeltaE(il, ia, ib, jl, ja, jb);
      delta_e[i * 64 + j] = de;
      max_delta_e = std::max(de, max_delta_e);
    }
  }
  
  Printf("Init pixels.\n");
  for (int i = 0; i < 64; i++) {
    // int idx = i;
    int idx = best_permutation[i];
    pixels[i] = Pixel{(uint8)idx,
		      ntsc_palette[idx * 3 + 0],
		      ntsc_palette[idx * 3 + 1],
		      ntsc_palette[idx * 3 + 2]};
  }
 
  Printf("Initial draw:\n");
  Redraw();

  bool paused = false;
  int mousex = 0, mousey = 0;
  bool mousemoved = true;
  int swaps = 0, segments = 0;;

  float current_error = Error();
  vector<Pixel> best = pixels;
  float best_error = current_error;

  auto Save = [&best_error, &best]() {
    printf("Best error: %.4f\n", best_error);
    for (int i = 0; i < best.size(); i++) {
      printf("0x%02x, ", best[i].idx);
      if ((i + 1) % 8 == 0) printf("\n");
    }
    printf("\n");
  };

  auto Swap2 = [&swaps, &current_error](int i, int j) {
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
  };

  auto SwapSeg = [&swaps, &current_error, &segments](int a, int b, int c) {
    // Try moving a single segment. wlog the swap can be
    // defined by three points 0 <= A <= B <= C <= pixels.size
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

    const int w = b - a;
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
  };

  enum Mode : int {
    MODE_SEARCH,
    MODE_SURFACE,
      
    NUM_MODES
  };
  Mode mode = MODE_SEARCH;
  
  int swap_src = 0, seg_a = 0;
  int successive_failures = 0;
  for (int round = 0; ; round++) {

    if (mode == MODE_SEARCH) {
      int old_swaps = swaps;
      if (!paused) {
	if (round & 1) {
	  Swap2(RandTo(&rc, WIDTH * HEIGHT), RandTo(&rc, WIDTH * HEIGHT));
	} else {
	  // Note that any of these can actually be past the end of the array.
	  int a = RandTo(&rc, WIDTH * HEIGHT + 1);
	  int b = RandTo(&rc, WIDTH * HEIGHT + 1);
	  int c = RandTo(&rc, WIDTH * HEIGHT + 1);

	  // TODO: Would be nice to have a library for sorting fixed-size
	  // "arrays" like this, though obviously this is not the bottleneck
	  // here.
	  if (c < b) std::swap(b, c);
	  if (b < a) std::swap(a, b);
	  // now a <= b and a <= c
	  if (c < b) std::swap(b, c);
	  // Now a <= b <= c
	  SwapSeg(a, b, c);
	}

	if (swaps == old_swaps) {
	  if (round & 1) {
	    for (int swap_dst = swap_src + 1; swap_dst < (WIDTH * HEIGHT); swap_dst++) {
	      Swap2(swap_src, swap_dst);
	    }
	    swap_src = (swap_src + 1) % (WIDTH * HEIGHT);
	  } else {
	    // printf("All segs.\n");
	    for (int seg_b = seg_a + 1; seg_b < WIDTH * HEIGHT; seg_b++) {
	      for (int seg_c = seg_b + 1; seg_c < WIDTH * HEIGHT + 1; seg_c++) {
		SwapSeg(seg_a, seg_b, seg_c);
	      }
	    }
	    seg_a = (seg_a + 1) % (WIDTH * HEIGHT);
	  }
	}

	if (swaps == old_swaps) {
	  // Still no successes...
	  successive_failures++;
	  if (successive_failures > 128) {
	    printf("%.4f error (best %.4f). %d swaps. %d segs.\n", current_error,
		   best_error, swaps, segments);
	    if (current_error < best_error) {
	      best = pixels;
	      best_error = current_error;
	    }
	    for (int i = 0; i < WIDTH * HEIGHT; i++) {
	      int j = RandTo(&rc, WIDTH * HEIGHT);
	      if (j != i) {
		Pixel tmp = pixels[i];
		pixels[i] = pixels[j];
		pixels[j] = tmp;
	      }
	    }
	    current_error = Error();
	    swaps = 0;
	    segments = 0;
	    successive_failures = 0;
	    // seg_a = 0;
	    // swap_src = 0;
	  }
	} else {
	  successive_failures = 0;
	  Redraw();
	  SDL_Flip(screen);
	}
      }
    } else if (mode == MODE_SURFACE) {
      if (mousemoved) {
	int mx = mousex >> SCALE;
	int my = mousey >> SCALE;
	if (mx >= 0 && my >= 0 && mx < WIDTH && my < HEIGHT) {
	  const Pixel &ref = pixels[my * WIDTH + mx];
	  const int refrow = ref.idx * WIDTH * HEIGHT;
	  for (int y = 0; y < HEIGHT; y++) {
	    for (int x = 0; x < WIDTH; x++) {
	      const Pixel &other = pixels[y * WIDTH + x];
	      float def = delta_e[refrow + other.idx] / max_delta_e;
	      float v = def * 255.0f;
	      sdlutil::FillRectRGB(screen,
				   x << SCALE, y << SCALE,
				   1 << SCALE, 1 << SCALE,
				   // p.r, p.g, p.b
				   v, v, v);
	      sdlutil::FillRectRGB(screen,
				   x << SCALE, y << SCALE,
				   1 << SCALE, 4,
				   other.r, other.g, other.b);
				   
	      font->drawto_plain(screen, (x << SCALE) + 1, (y << SCALE) + 1,
				 StringPrintf("%02x", other.idx));
	      font->drawto_plain(screen, (x << SCALE) + 1, (y << SCALE) + 1 + FONTHEIGHT,
				 StringPrintf("%.4f", def));
	    }
	  }
	  SDL_Flip(screen);
	}
      }
      mousemoved = false;
    }
      
    SDL_Event event;
    if (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
	Save();
	Printf("QUIT.\n");
	return;

      } else if (event.type == SDL_MOUSEMOTION) {
	SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;

	mousex = e->x;
	mousey = e->y;
	mousemoved = true;
	// (and immediately redraw)

      } else if (event.type == SDL_KEYDOWN) {
	switch (event.key.keysym.sym) {
	case SDLK_b:
	  printf("Take best.\n");
	  pixels = best;
	  current_error = Error();
	  successive_failures = 0;
	  segments = 0;
	  swaps = 0;
	  Redraw();
	  break;
	case SDLK_SPACE:
	  mode = (Mode)(((int)mode + 1) % NUM_MODES);
	  break;
	case SDLK_ESCAPE:
	  Save();
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
