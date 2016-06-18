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

// Graphics.
#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr;
#define SCALE 2
#define SCREENW 1920
#define SCREENH 1280
#define WIDTH (SCREENW >> SCALE)
#define HEIGHT (SCREENH >> SCALE)
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
  Pixel(uint8 r, uint8 g, uint8 b) : r(r), g(g), b(b) {
    ColorUtil::RGBToLAB(ByteFloat(r), ByteFloat(g), ByteFloat(b),
			&ll, &aa, &bb);
  }
  // Actual color value.
  uint8 r, g, b;
  // in L*a*b* space.
  float ll, aa, bb;
};
vector<Pixel> pixels{WIDTH * HEIGHT, Pixel{0,0,0}};

// Give "pixel" coordinates; scales up into a chunky pixel.
inline void DrawChunk(int x, int y, uint8 r, uint8 g, uint8 b) {
  for (int yy = 0; yy < (1 << SCALE); yy++) {
    for (int xx = 0; xx < (1 << SCALE); xx++) {
      sdlutil::drawpixel(screen, (x << SCALE) + xx, (y << SCALE) + yy, r, g, b);
    }
  }
}

static inline float PenaltyWrt(const Pixel &p, int x, int y) {
  if (x < 0 || y < 0 || x >= WIDTH || y >= HEIGHT) return 0.0f;
  const Pixel &other = pixels[y * WIDTH + x];
  return ColorUtil::DeltaE(p.ll, p.aa, p.bb, other.ll, other.aa, other.bb);
}

static float PenaltyInPosition(const Pixel &p, int x, int y) {
  return PenaltyWrt(p, x - 1, y - 1) +
    PenaltyWrt(p, x, y - 1) +
    PenaltyWrt(p, x + 1, y - 1) +
    PenaltyWrt(p, x - 1, y) +
    PenaltyWrt(p, x + 1, y) +
    PenaltyWrt(p, x - 1, y + 1) +
    PenaltyWrt(p, x, y + 1) +
    PenaltyWrt(p, x + 1, y + 1);
}

static float PenaltyUD(const Pixel &p, int x, int y) {
  return PenaltyWrt(p, x - 1, y - 1) +
    PenaltyWrt(p, x, y - 1) +
    PenaltyWrt(p, x + 1, y - 1) +
    PenaltyWrt(p, x - 1, y + 1) +
    PenaltyWrt(p, x, y + 1) +
    PenaltyWrt(p, x + 1, y + 1);
}

static float PenaltyLR(const Pixel &p, int x, int y) {
  return PenaltyWrt(p, x - 1, y - 1) +
    PenaltyWrt(p, x + 1, y - 1) +
    PenaltyWrt(p, x - 1, y) +
    PenaltyWrt(p, x + 1, y) +
    PenaltyWrt(p, x - 1, y + 1) +
    PenaltyWrt(p, x + 1, y + 1);
}

inline void UpdatePixel(int x, int y) {
  const Pixel &p = pixels[y * WIDTH + x];
  DrawChunk(x, y, p.r, p.g, p.b);
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

static void UIThread() {
  ArcFour rc("ui");

  std::unique_ptr<ImageRGBA> img{ImageRGBA::Load("spirals.jpg")};
  CHECK(img.get());

  static constexpr int SWAPS_PER_FRAME = 1000;
  Asynchronously write_frames{16};
  
  Printf("Init with random:\n");
  for (int i = 0; i < pixels.size(); i++) {
    pixels[i] = Pixel{rc.Byte(), rc.Byte(), rc.Byte()};
  }
  
  Printf("Blit image:\n");

  CHECK(img->width <= WIDTH) << img->width;
  CHECK(img->height <= HEIGHT) << img->height;
  int startx = (WIDTH - img->width) >> 1;
  int starty = (HEIGHT - img->height) >> 1;
  for (int y = 0; y < img->height; y++) {
    for (int x = 0; x < img->width; x++) {
      uint8 r = img->rgba[(y * img->width + x) * 4 + 0];
      uint8 g = img->rgba[(y * img->width + x) * 4 + 1];
      uint8 b = img->rgba[(y * img->width + x) * 4 + 2];
      int dy = y + starty;
      int dx = x + startx;
      int idx = dy * WIDTH + dx;
      CHECK(idx >= 0 && idx < WIDTH * HEIGHT) << dx << " " << dy;
      pixels[idx] = Pixel{r, g, b};
    }
  }
  
  
  Printf("Initial draw:\n");
  Redraw();

  int swaps = 0;
  enum {
    STRATEGY_RECT = 0,
    STRATEGY_RANDOM_PAIR = 1,
    STRATEGY_ALL_PIXELS = 2,
    STRATEGY_ROWS = 3,
    STRATEGY_COLUMNS = 4,
    STRATEGY_LOCAL = 5,

    NUM_STRATEGIES,
  };
  vector<double> error_by_strategy(NUM_STRATEGIES, 0.0);
  
  auto SwapPixels = [&swaps](int ax, int ay, int bx, int by) {
    swaps++;
    const Pixel tmp = pixels[ay * WIDTH + ax];
    pixels[ay * WIDTH + ax] = pixels[by * WIDTH + bx];
    pixels[by * WIDTH + bx] = tmp;
    UpdatePixel(ax, ay);
    UpdatePixel(bx, by);
  };

  int frame_num = 0;

  for (int round = 0; ; round++) {
    switch (round & 7) {
    default:
    case 4:
    case 5:
    case 6: {
      // Generate two non-overlapping rectangles.
      int w, h, ax, ay, bx, by;
      do {
	w = 1 + RandTo(&rc, WIDTH / 16);
	h = 1 + RandTo(&rc, HEIGHT / 16);
	ax = RandTo(&rc, WIDTH - w);
	ay = RandTo(&rc, HEIGHT - h);
	bx = RandTo(&rc, WIDTH - w);
	by = RandTo(&rc, HEIGHT - h);
      } while (Overlap(w, h, ax, ay, bx, by));

      float now = 0.0f, then = 0.0f;

      auto EdgeError = [](
	  // Edge, as a start position, unit vector, pixel length.
	  int px, int py, int dpx, int dpy, int len,
	  // Start position for error (not offset) for error source.
	  // If testing actual current error, same as px, py.
	  int ex, int ey,
	  // Unit vector giving error offset.
	  int edx, int edy) -> float {
	float err = 0.0f;
	for (int z = 0; z < len; z++) {
	  const Pixel &p = pixels[py * WIDTH + px];

	  for (int e = 1; e < /* len + */ 1; e++) {
	    err += PenaltyWrt(p, ex + edx * e, ey + edy * e);
	    // But also perpendicular.
	    err += PenaltyWrt(p, ex + edx * e - edy, ey + edy * e - edx);
	    err += PenaltyWrt(p, ex + edx * e + edy, ey + edy * e + edx);
	  }
	    
	  px += dpx;
	  py += dpy;
	  ex += dpx;
	  ey += dpy;
	}
	return err;
      };

      // Top, right, bottom, left
      now += EdgeError(ax, ay, 1, 0, w, ax, ay, 0, -1);
      now += EdgeError(ax + w - 1, ay, 0, 1, h, ax + w - 1, ay, 1, 0);
      now += EdgeError(ax, ay + h - 1, 1, 0, w, ax, ay + h - 1, 0, 1);
      now += EdgeError(ax, ay, 0, 1, h, ax, ay, -1, 0);
      // Same, but for rectangle B.
      now += EdgeError(bx, by, 1, 0, w, bx, by, 0, -1);
      now += EdgeError(bx + w - 1, by, 0, 1, h, bx + w - 1, by, 1, 0);
      now += EdgeError(bx, by + h - 1, 1, 0, w, bx, by + h - 1, 0, 1);
      now += EdgeError(bx, by, 0, 1, h, bx, by, -1, 0);
      
      // Same, but error positions are swapped.
      then += EdgeError(ax, ay, 1, 0, w, bx, by, 0, -1);
      then += EdgeError(ax + w - 1, ay, 0, 1, h, bx + w - 1, by, 1, 0);
      then += EdgeError(ax, ay + h - 1, 1, 0, w, bx, by + h - 1, 0, 1);
      then += EdgeError(ax, ay, 0, 1, h, bx, by, -1, 0);

      then += EdgeError(bx, by, 1, 0, w, ax, ay, 0, -1);
      then += EdgeError(bx + w - 1, by, 0, 1, h, ax + w - 1, ay, 1, 0);
      then += EdgeError(bx, by + h - 1, 1, 0, w, ax, ay + h - 1, 0, 1);
      then += EdgeError(bx, by, 0, 1, h, ax, ay, -1, 0);


      sdlutil::drawbox(screen, ax << SCALE, ay << SCALE, w << SCALE, h << SCALE,
		       0xFF, 0, 0);
      sdlutil::drawbox(screen, bx << SCALE, by << SCALE, w << SCALE, h << SCALE,
		       0, 0xFF, 0);
      int amx = ax + (w >> 1), amy = ay + (h >> 1);
      int bmx = bx + (w >> 1), bmy = by + (h >> 1);
      sdlutil::drawclipline(screen, amx << SCALE, amy << SCALE,
			    bmx << SCALE, bmy << SCALE, 0xFF, 0xFF, 0);
      
      Printf("Rect before %f, after %f\n", now, then);
      
      if (then < now) {
	// Improvement! Swap the rectangles, which can just be done pixel-by-pixel because
	// they don't overlap.
	error_by_strategy[STRATEGY_RECT] += (now - then);
	for (int y = 0; y < h; y++) {
	  for (int x = 0; x < w; x++) {
	    SwapPixels(ax + x, ay + y, bx + x, by + y);
	  }
	}
      }
      
      break;
    }
    case 0:
      // Try every pixel, but only swap with pixels
      // strictly above it.
      for (int ay = 1; ay < WIDTH; ay++) {
	for (int ax = 0; ax < HEIGHT; ax++) {
	  int by = RandTo(&rc, ay);
	  int bx = RandTo(&rc, WIDTH);
	  const Pixel &a = pixels[ay * WIDTH + ax];
	  const Pixel &b = pixels[by * WIDTH + bx];
	  const float now = PenaltyInPosition(a, ax, ay) + PenaltyInPosition(b, bx, by);
	  const float then = PenaltyInPosition(b, ax, ay) + PenaltyInPosition(a, bx, by);
	  if (then < now) {
	    error_by_strategy[STRATEGY_ALL_PIXELS] += (now - then);
	    SwapPixels(ax, ay, bx, by);
	  }
	}
	break;
      }
    case 1:
      // Take random pairs of pixels.
      for (int i = 0; i < 1000; i++) {
	int ax = RandTo(&rc, WIDTH);
	int ay = RandTo(&rc, HEIGHT);
	int bx = RandTo(&rc, WIDTH);
	int by = RandTo(&rc, HEIGHT);
	const Pixel &a = pixels[ay * WIDTH + ax];
	const Pixel &b = pixels[by * WIDTH + bx];
	const float now = PenaltyInPosition(a, ax, ay) + PenaltyInPosition(b, bx, by);
	const float then = PenaltyInPosition(b, ax, ay) + PenaltyInPosition(a, bx, by);
	if (then < now) {
	  error_by_strategy[STRATEGY_RANDOM_PAIR] += (now - then);
	  SwapPixels(ax, ay, bx, by);
	}
      }
      break;
    case 7:
      // Swap only with nearby pixels.
      for (int ay = 1; ay < HEIGHT - 1; ay++) {
	for (int ax = 1; ax < WIDTH - 1; ax++) {
	  for (const int by : { ay - 1, ay, ay + 1 }) {
	    for (const int bx : { ax - 1, ax, ax + 1}) {
	      // printf("%d %d %d %d\n", ax, ay, bx, by);
	      if (ax != bx || ay != by) {
		CHECK(bx >= 0 && bx < WIDTH);
		CHECK(by >= 0 && by < WIDTH);
		const Pixel &a = pixels[ay * WIDTH + ax];
		const Pixel &b = pixels[by * WIDTH + bx];

		// We don't want to use PenaltyInPosition because when computing the
		// replaced position, it always sees itself as one of the new neighbors,
		// which makes a swap very likely even when it will not reduce overall
		// error. Explicitly include points A and B in error calculation (they
		// remain adjacent to each other).
		float now = 0.0, then = 0.0;
		// Neighborhood of A.
		for (int aey : { ay - 1, ay, ay + 1 }) {
		  for (int aex : { ax - 1, ax, ax + 1 }) {
		    if ((aex == ax && aey == ay) ||
			(aex == bx && aey == by)) continue;
		    now += PenaltyWrt(a, aex, aey);
		    then += PenaltyWrt(b, aex, aey);
		  }
		}

		// Neighborhood of B.
		for (int bey : { by - 1, by, by + 1 }) {
		  for (int bex : { bx - 1, bx, bx + 1 }) {
		    if ((bex == ax && bey == ay) ||
			(bex == bx && bey == by)) continue;
		    now += PenaltyWrt(b, bex, bey);
		    then += PenaltyWrt(a, bex, bey);
		  }
		}

		if (then < now) {
		  error_by_strategy[STRATEGY_LOCAL] += (now - then);
		  SwapPixels(ax, ay, bx, by);
		}
	      }
	    }
	  }
	}
      }
      break;
    }
      
    // printf("%d swaps.\n", swaps);

    SDL_Flip(screen);

    if (swaps > SWAPS_PER_FRAME) {
      Redraw();
      Printf("Round %d. Error reduction:\n", round);
#define STRAT(s) Printf(#s ": %.2f.\n", error_by_strategy[s])
      STRAT(STRATEGY_RECT);
      STRAT(STRATEGY_RANDOM_PAIR);
      STRAT(STRATEGY_ALL_PIXELS);
      STRAT(STRATEGY_ROWS);
      STRAT(STRATEGY_COLUMNS);
      STRAT(STRATEGY_LOCAL);
#undef STRAT

      vector<uint8> *rgba = new vector<uint8>(WIDTH * HEIGHT * 4, 0);
      for (int i = 0; i < pixels.size(); i++) {
	(*rgba)[i * 4 + 0] = pixels[i].r;
	(*rgba)[i * 4 + 1] = pixels[i].g;
	(*rgba)[i * 4 + 2] = pixels[i].b;
	(*rgba)[i * 4 + 3] = 0xFF;
      }
      write_frames.Run([rgba, frame_num, swaps]() {
	string filename = StringPrintf("cluster/frame-%d.png", frame_num);
	stbi_write_png(filename.c_str(), WIDTH, HEIGHT, 4, rgba->data(), 4 * WIDTH);
	delete rgba;
	Printf("Wrote %s (%d swaps)\n", filename.c_str(), swaps);
      });
      swaps = 0;
      frame_num++;
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
