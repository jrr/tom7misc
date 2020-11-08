
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <deque>
#include <unordered_map>
#include <unistd.h>
#include <cmath>
#include <tuple>

#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/sdl/cursor.h"
#include "../cc-lib/lines.h"
#include "../cc-lib/util.h"
#include "../cc-lib/image.h"
#include "../cc-lib/color-util.h"

#include <api/fftw3.h>

/* there are some non-ascii symbols in the font */
#define CHECKMARK "\xF2"
#define ESC "\xF3"
#define HEART "\xF4"
/* here L means "long" */
#define LCMARK1 "\xF5"
#define LCMARK2 "\xF6"
#define LCHECKMARK LCMARK1 LCMARK2
#define LRARROW1 "\xF7"
#define LRARROW2 "\xF8"
#define LRARROW LRARROW1 LRARROW2
#define LLARROW1 "\xF9"
#define LLARROW2 "\xFA"
#define LLARROW LLARROW1 LLARROW2

/* BAR_0 ... BAR_10 are guaranteed to be consecutive */
#define BAR_0 "\xE0"
#define BAR_1 "\xE1"
#define BAR_2 "\xE2"
#define BAR_3 "\xE3"
#define BAR_4 "\xE4"
#define BAR_5 "\xE5"
#define BAR_6 "\xE6"
#define BAR_7 "\xE7"
#define BAR_8 "\xE8"
#define BAR_9 "\xE9"
#define BAR_10 "\xEA"
#define BARSTART "\xEB"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" CHECKMARK ESC HEART LCMARK1 LCMARK2 BAR_0 BAR_1 BAR_2 BAR_3 BAR_4 BAR_5 BAR_6 BAR_7 BAR_8 BAR_9 BAR_10 BARSTART LRARROW LLARROW

#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr, *font2x = nullptr, *font4x = nullptr;

static SDL_Cursor *cursor_arrow = nullptr, *cursor_bucket = nullptr;
static SDL_Cursor *cursor_hand = nullptr, *cursor_hand_closed = nullptr;
static SDL_Cursor *cursor_eraser = nullptr;
#define VIDEOH 1080
#define STATUSH 128
#define SCREENW 1920
#define SCREENH (VIDEOH + STATUSH)


constexpr int SQUARE = 512;

constexpr int IMGX = 32;
constexpr int IMGY = 32;
constexpr int FFTX = IMGX + SQUARE + 32;
constexpr int FFTY = IMGY;


using Rect = std::tuple<int, int, int, int>;
constexpr Rect IMGRECT = {IMGX, IMGY, IMGX + SQUARE, IMGY + SQUARE};
constexpr Rect FFTRECT = {FFTX, FFTY, FFTX + SQUARE, FFTY + SQUARE};

static SDL_Surface *screen = nullptr;

static constexpr uint32 COLORS[] = {
  // black, white
  /* 1 */ 0xFF000000,
  /* 2 */ 0xFFFFFFFF,
  // 'nice' RGB
  /* 3 */ 0xFFbd3838, // R
  /* 4 */ 0xFF0ba112, // G
  /* 5 */ 0xFF1664CE, // B
  // intense
  /* 6 */ 0xFFf943ea, // magenta
  /* 7 */ 0xFFeef943, // yellow
  /* 8 */ 0xFF29edef, // cyan
  // misc
  /* 9 */ 0xFFc77e00, // orange/brown
  // Note zero is actually like "10"
  /* 0 */ 0xFF7500c7, // purple
};

/*
  FFTW_R2HC computes a real-input DFT with output in halfcomplex format, i.e. real and imaginary parts for a transform of size n stored as:
  r0, r1, r2, ..., rn/2, i(n+1)/2-1, ..., i2, i1

  (Logical N=n, inverse is FFTW_HC2R.)
  FFTW_HC2R computes the reverse of FFTW_R2HC, above. (Logical N=n, inverse is FFTW_R2HC.)
  FFTW_DHT computes a discrete Hartley transform. (Logical N=n, inverse is FFTW_DHT.)
  FFTW_REDFT00 computes an REDFT00 transform, i.e. a DCT-I. (Logical N=2*(n-1), inverse is FFTW_REDFT00.)
  FFTW_REDFT10 computes an REDFT10 transform, i.e. a DCT-II (sometimes called “the” DCT). (Logical N=2*n, inverse is FFTW_REDFT01.)
  FFTW_REDFT01 computes an REDFT01 transform, i.e. a DCT-III (sometimes called “the” IDCT, being the inverse of DCT-II). (Logical N=2*n, inverse is FFTW_REDFT=10.)
  FFTW_REDFT11 computes an REDFT11 transform, i.e. a DCT-IV. (Logical N=2*n, inverse is FFTW_REDFT11.)
  FFTW_RODFT00 computes an RODFT00 transform, i.e. a DST-I. (Logical N=2*(n+1), inverse is FFTW_RODFT00.)
  FFTW_RODFT10 computes an RODFT10 transform, i.e. a DST-II. (Logical N=2*n, inverse is FFTW_RODFT01.)
  FFTW_RODFT01 computes an RODFT01 transform, i.e. a DST-III. (Logical N=2*n, inverse is FFTW_RODFT=10.)
  FFTW_RODFT11 computes an RODFT11 transform, i.e. a DST-IV. (Logical N=2*n, inverse is FFTW_RODFT11.)
*/

// Real-to-real 2D FFT transform for size*size square.
struct FFTBuffer2D {
  // PERF: Use float, not double (fftwf_ prefix... but need to build library
  // for this I guess)
  using element_type = double;
  
  FFTBuffer2D(int size) : size(size) {
    in = (double*) fftw_malloc(sizeof (element_type) * size * size);
    out = (double*) fftw_malloc(sizeof (element_type) * size * size);

    auto [fwd, inv] = make_tuple(FFTW_DHT, FFTW_DHT);
    // auto [fwd, inv] = make_tuple(FFTW_REDFT00, FFTW_REDFT00);
    // auto [fwd, inv] = make_tuple(FFTW_REDFT10, FFTW_REDFT01);
    
    plan = fftw_plan_r2r_2d(size, size, in, out,
			    fwd, fwd,
			    FFTW_MEASURE);
    printf("Forward plan:\n");
    fftw_print_plan(plan);

    rplan = fftw_plan_r2r_2d(size, size, out, in,
			     inv, inv,
			     FFTW_MEASURE);
    
    printf("Inverse plan:\n");
    fftw_print_plan(rplan);
  }
  
  ~FFTBuffer2D() {
    fftw_free(in);
    fftw_free(out);
    fftw_destroy_plan(plan);
    fftw_destroy_plan(rplan);    
  }

  void Forward() {
    fftw_execute(plan);
    for (int i = 0; i < size * size; i++) {
      out[i] *= (1.0 / size);
    }
  }

  void Inverse() {
    fftw_execute(rplan);
    for (int i = 0; i < size * size; i++) {
      in[i] *= (1.0 / size);
    }
  }
  
  const int size;
  // Row-major.
  element_type *in = nullptr, *out = nullptr;
  
private:
  
  fftw_plan plan, rplan;
};

// Mode basically controls what happens when we use the mouse.
enum class Mode {
  ERASING,
  DRAWING,
  FILLING,
};

struct UI {
  Mode mode = Mode::DRAWING;
  bool ui_dirty = true;

  UI();
  void Loop();
  void DrawStatus();
  void Draw();

  uint8 current_value = 0xFF;

  ImageA img, fft, gamut;
  FFTBuffer2D fft_buffer;
  
  void DrawThick(int x0, int y0,
		 int x1, int y1,
		 uint8 value);
  void FloodFill(int x, int y, uint8 value);

  void Forward();

  void ImgChanged();
  void FftChanged();

  void DrawImg();
  void DrawFft();
  
  int mousex = 0, mousey = 0;
  bool dragging = false;
};

UI::UI() : img(SQUARE, SQUARE), fft(SQUARE, SQUARE), gamut(SQUARE, SQUARE),
	   fft_buffer(SQUARE) {
  img.Clear(0x00);

  ImgChanged();
}

void UI::ImgChanged() {

  // XXX do FFT
  Forward();
  
}

void UI::FftChanged() {
  // XXX do inverse fft
}


void UI::Forward () {
  constexpr float o255 = 1.0f / 255.0f;
  for (int y = 0; y < SQUARE; y++) {
    for (int x = 0; x < SQUARE; x++) {
      fft_buffer.in[y * SQUARE + x] = (float)img.GetPixel(x, y) * o255;
    }
  }
  fft_buffer.Forward();

  // Copy FFT result floats to "fft image".
  // XXX I should do some normalization here, I think; what?
  FFTBuffer2D::element_type mx = -9999.0f, mn = 9999.0f;

  int over = 0, under = 0;
  for (int y = 0; y < SQUARE; y++) {
    for (int x = 0; x < SQUARE; x++) {
      FFTBuffer2D::element_type f = fft_buffer.out[y * SQUARE + x];
      // float rf = 
      mx = std::max(f, mx);
      mn = std::min(f, mn);
      uint8 g = 0x77;
      if (f > 1.0f) {
	over++;
	g = 0xFF;
	f = 1.0f;
      } else if (f < -1.0f) {
	under++;
	g = 0x00;
	f = -1.0f;
      }

      uint8 v = (uint8) (127.5f + (f * 127.5f));
      fft.SetPixel(x, y, v);
      gamut.SetPixel(x, y, g);
    }
  }
  printf("Max: %.2f, Min: %.2f   Over: %d, Under: %d\n",
	 mx, mn, over, under);
  
}

void UI::DrawImg() {
  // Draw to screen
  for (int y = 0; y < SQUARE; y++) {
    for (int x = 0; x < SQUARE; x++) {
      const uint8 v = img.GetPixel(x, y);
      const uint32 color = SDL_MapRGBA(screen->format, v, v, v, 0xFF);
      sdlutil::SetPixel32(screen, IMGX + x, IMGY + y, color);
    }
  }
}

void UI::DrawFft() {
  // Draw to screen
  for (int y = 0; y < SQUARE; y++) {
    for (int x = 0; x < SQUARE; x++) {
      const uint8 v = fft.GetPixel(x, y);
      const uint8 gam = gamut.GetPixel(x, y);

      uint8 r = v;
      uint8 g = v;
      uint8 b = v;
      if (gam == 0xFF) {
	g = 0x0;
	b = 0x0;
      } else if (gam == 0x00) {
	g = 0xFF;
	r = 0x0;
	b = 0x0;
      }
      const uint32 color = SDL_MapRGBA(screen->format, r, g, b, 0xFF);
      sdlutil::SetPixel32(screen, FFTX + x, FFTY + y, color);
    }
  }
}


// Clips the line to the rectangle (with both in, say, screen
// coordinates), and puts it in rectangle-local coordinates. Returns
// nullopt if the whole line is outside the rectangle.
std::optional<std::tuple<int, int, int, int>>
ClipLineToRect(std::tuple<int, int, int, int> line,
	       std::tuple<int, int, int, int> rect) {
  auto [lx0, ly0, lx1, ly1] = line;
  auto [rx0, ry0, rx1, ry1] = rect;
  // This algorithm wants rectangle bounds to be inclusive...
  rx1++;
  ry1++;
  
  // Cohen-Sutherland
  using OutCode = int;
  constexpr int INSIDE = 0b0000;
  constexpr int LEFT   = 0b0001;
  constexpr int RIGHT  = 0b0010;
  constexpr int BOTTOM = 0b0100;
  constexpr int TOP    = 0b1000;
  
  auto ComputeOutCode = [rx0, ry0, rx1, ry1](int xx, int yy) {
      OutCode code = INSIDE;
      if (xx < rx0)
	code |= LEFT;
      else if (xx > rx1)
	code |= RIGHT;

      // (XXX BOTTOM and TOP using Euclidean, not screen)
      if (yy < ry0)
	code |= BOTTOM;
      else if (yy > ry1)
	code |= TOP;
      return code;
    };

  OutCode outcode0 = ComputeOutCode(lx0, ly0);
  OutCode outcode1 = ComputeOutCode(lx1, ly1);

  // Loop until both points are inside window.
  while (0 != (outcode0 | outcode1)) {
    if (outcode0 & outcode1)
      return nullopt;

    // Then clip: Calculate the line segment to clip from an outside
    // point to an intersection with clip edge.

    // At least one endpoint is outside the clip rectangle; pick it.
    OutCode outcodeOut = outcode1 > outcode0 ? outcode1 : outcode0;

    // Now find the intersection point;
    // use formulas:
    //   slope = (y1 - y0) / (x1 - x0)
    //   x = x0 + (1 / slope) * (ym - y0), where ym is ymin or ymax
    //   y = y0 + slope * (xm - x0), where xm is xmin or xmax
    // No need to worry about divide-by-zero because, in each case, the
    // outcode bit being tested guarantees the denominator is non-zero

    // XXX not obvious that this terminates? Note I keep the intermediate points
    // as integers.
    int x, y;
    if (outcodeOut & TOP) {           // point is above the clip window
      x = lx0 + (lx1 - lx0) * (ry1 - ly0) / float(ly1 - ly0);
      y = ry1;
    } else if (outcodeOut & BOTTOM) { // point is below the clip window
      x = lx0 + (lx1 - lx0) * (ry0 - ly0) / float(ly1 - ly0);
      y = ry0;
    } else if (outcodeOut & RIGHT) {  // point is to the right of clip window
      y = ly0 + (ly1 - ly0) * (rx1 - lx0) / float(lx1 - lx0);
      x = rx1;
    } else {
      // (outcodeOut & LEFT)
      // point is to the left of clip window
      y = ly0 + (ly1 - ly0) * (rx0 - lx0) / float(lx1 - lx0);
      x = rx0;
    }

    // Now we move outside point to intersection point to clip
    // and get ready for next pass.
    if (outcodeOut == outcode0) {
      lx0 = x;
      ly0 = y;
      outcode0 = ComputeOutCode(lx0, ly0);
    } else {
      lx1 = x;
      ly1 = y;
      outcode1 = ComputeOutCode(lx1, ly1);
    }
  }
  return {{lx0 - rx0, ly0 - ry0, lx1 - rx0, ly1 - ry0}};
}

// Draw a line, if it falls on either the original or FFT image.
void UI::DrawThick(int x0, int y0,
		   int x1, int y1,
		   uint8 value) {

  std::tuple<int, int, int, int> line = {x0, y0, x1, y1};
  auto imgo = ClipLineToRect(line, IMGRECT);

  if (imgo.has_value()) {
    static constexpr int THICKNESS = 3;

    auto ThickPixel = [this, value](int x, int y) {
	static constexpr int LO = THICKNESS >> 1;
	for (int xx = x - LO; xx < x - LO + THICKNESS; xx++) {
	  for (int yy = y - LO; yy < y - LO + THICKNESS; yy++) {
	    img.SetPixel(xx, yy, value);
	  }
	}
      };
    
    const auto [x0, y0, x1, y1] = imgo.value();
    for (const auto [x, y] : Line<int>{x0, y0, x1, y1}) {
      ThickPixel(x, y);
    }

    ImgChanged();
  } 

  auto ffto = ClipLineToRect(line, IMGRECT);
  if (ffto.has_value()) {
    // ...

    FftChanged();
  }
}

std::optional<std::tuple<int, int>>
ClipPointToRect(std::tuple<int, int> point, Rect rect) {
  const auto [x0, y0, x1, y1] = rect;
  const auto [x, y] = point;
  if (x >= x1) return nullopt;
  if (y >= y1) return nullopt;

  int xx = x - x0;
  if (xx < 0) return nullopt;
  int yy = y - y0;
  if (yy < 0) return nullopt;
  return {{xx, yy}};
}

void UI::FloodFill(int x, int y, uint8 value) {

  auto imgo = ClipPointToRect({x, y}, IMGRECT);
  if (imgo.has_value()) {
    auto [x, y] = imgo.value();
    const uint8 replace_value = img.GetPixel(x, y);
    
    // Treat the border of the image as a color != to the value
    // being replaced.
    auto GetPixel = [this, value, replace_value](int x, int y) -> uint8 {
	if (x >= 0 && y >= 0 &&
	    x < img.width && y < img.height) {
	  return img.GetPixel(x, y);
	} else {
	  return ~replace_value;
	}
      };

    std::vector<std::pair<int, int>> todo;
    if (value != replace_value)
      todo.emplace_back(x, y);

    while (!todo.empty()) {
      const auto [xx, yy] = todo.back();
      todo.pop_back();

      uint8 c = GetPixel(xx, yy);
      if (c == replace_value) {
	img.SetPixel(xx, yy, value);
	todo.emplace_back(xx - 1, yy);
	todo.emplace_back(xx + 1, yy);
	todo.emplace_back(xx, yy - 1);
	todo.emplace_back(xx, yy + 1);
      }
    }

    ImgChanged();
  }

  // XXX for fft too..
  
}

void UI::Loop() {
  for (;;) {

    SDL_Event event;
    if (SDL_PollEvent(&event)) {
      switch (event.type) {
      case SDL_QUIT:
        printf("QUIT.\n");
        return;

      case SDL_MOUSEMOTION: {
        SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;

        const int oldx = mousex, oldy = mousey;

        mousex = e->x;
        mousey = e->y;

        if (dragging) {
          switch (mode) {
          case Mode::DRAWING:
            DrawThick(oldx, oldy, mousex, mousey, current_value);
            break;
          case Mode::ERASING:
            DrawThick(oldx, oldy, mousex, mousey, 0x00);
            break;
          default:;
          }
          ui_dirty = true;
        }
        break;
      }

      case SDL_KEYDOWN: {
        switch (event.key.keysym.sym) {
        case SDLK_ESCAPE:
          printf("ESCAPE.\n");
          return;

        case SDLK_e: {
          mode = Mode::ERASING;
          SDL_SetCursor(cursor_eraser);
          ui_dirty = true;
          break;
        }

        case SDLK_d: {
          mode = Mode::DRAWING;
          SDL_SetCursor(cursor_arrow);
          ui_dirty = true;
          break;
        }

        case SDLK_f: {
          mode = Mode::FILLING;
          SDL_SetCursor(cursor_bucket);
          ui_dirty = true;
          break;
        }

        case SDLK_q: {
	  img.Clear(0x0);
          ui_dirty = true;
          break;
        }
	  
        case SDLK_KP_PLUS:
        case SDLK_EQUALS:
        case SDLK_PLUS:
	  if (current_value < 0xF0) current_value += 0x10;
	  else current_value = 0xFF;
	  ui_dirty = true;
	  break;
	  
        case SDLK_KP_MINUS:
        case SDLK_MINUS:
	  if (current_value >= 0x10) current_value -= 0x10;
	  else current_value = 0x00;
	  ui_dirty = true;
	  break;

	case SDLK_l: {
	  if (event.key.keysym.mod & KMOD_CTRL) {
	    printf("Load drawing.png...\n");
	    fflush(stdout);
	    std::unique_ptr<ImageRGBA> rgba(ImageRGBA::Load("drawing.png"));
	    CHECK(rgba.get());
	    for (int y = 0; y < SQUARE; y++) {
	      for (int x = 0; x < SQUARE; x++) {
		uint32 px = 0;
		if (x < rgba->width && y < rgba->height)
		  px = rgba->GetPixel(x, y);
		constexpr float o255 = 1.0f / 255.0f;
		float r = ((px >> 24) & 0xFF) * o255;
		float g = ((px >> 16) & 0xFF) * o255;
		float b = ((px >> 8) & 0xFF) * o255;
		float a = (px & 0xFF) * o255;
		float ll, aa, bb;
		ColorUtil::RGBToLAB(r, g, b, &ll, &aa, &bb);
		constexpr float scale100to255 = 255.0f / 100.0f;
		float light = ll * scale100to255 * a;
		img.SetPixel(x, y, (uint8)light);
	      }
	    }
		   
	    ImgChanged();
	    ui_dirty = true;
	  }
	  break;
	}
	  
        case SDLK_s: {
	  if (event.key.keysym.mod & KMOD_CTRL) {
#if 0
	    sdlutil::SavePNG("drawing.png", drawing);
	    printf("Wrote drawing.png\n");
	    fflush(stdout);
#endif
	  }
	  break;
        }

        case SDLK_0:
        case SDLK_1:
        case SDLK_2:
        case SDLK_3:
        case SDLK_4:
        case SDLK_5:
        case SDLK_6:
        case SDLK_7:
        case SDLK_8:
        case SDLK_9: {
          // Not only are these in order, but they map to their
          // ASCII Values!
          const int n = ((event.key.keysym.sym - SDLK_0) + 9) % 10;
	  current_value = (uint8)((n / 9.0f) * 255.0f);
          ui_dirty = true;
          break;
        }

        default:;
        }
        break;
      }

      case SDL_MOUSEBUTTONDOWN: {
        // LMB/RMB, drag, etc.
	SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;
        mousex = e->x;
        mousey = e->y;

        dragging = true;
        if (mode == Mode::DRAWING) {
          DrawThick(mousex, mousey, mousex, mousey, current_value);
          ui_dirty = true;
        } else if (mode == Mode::ERASING) {
          DrawThick(mousex, mousey, mousex, mousey, 0x00);
          ui_dirty = true;

        } else if (mode == Mode::FILLING) {
          FloodFill(mousex, mousey, current_value);

          ui_dirty = true;
        }

        break;
      }

      case SDL_MOUSEBUTTONUP: {
        // LMB/RMB, drag, etc.
        dragging = false;
        break;
      }

      default:;
      }
    }

    if (ui_dirty) {
      sdlutil::clearsurface(screen, 0xFFFFFFFF);
      Draw();
      SDL_Flip(screen);
      ui_dirty = false;
    }
  }

}

void UI::DrawStatus() {
  int erasecolor = 1;
  int drawcolor = 1;
  int fillcolor = 1;

  switch (mode) {
  case Mode::ERASING:
    erasecolor = 2;
    break;

  case Mode::DRAWING:
    drawcolor = 2;
    break;

  case Mode::FILLING:
    fillcolor = 2;
    break;
  }

#define KEY(s) "^3" s "^<"
  const string modestring =
    StringPrintf(KEY("E") "^%drase^<  "
                 KEY("D") "^%draw^<  "
                 KEY("F") "^%dill^<  "
                 ,
                 erasecolor,
                 drawcolor, fillcolor
                 );
  font2x->draw(5, SCREENH - (FONTHEIGHT * 2) - 1, modestring);
#undef KEY

  // Color swatches.
  switch (mode) {
  case Mode::ERASING:
  case Mode::DRAWING:
  case Mode::FILLING: {
    const int yy = SCREENH - (FONTHEIGHT * 4) - 1;
    static constexpr int SWATCHWIDTH = 64;
    for (int i = 0; i < 10; i++) {
      uint8 value = (uint8)((i / 10.0f) * 255.0f);
      const uint32 value32 = SDL_MapRGBA(screen->format,
					 value, value, value, 0xFF);            
      sdlutil::fillrect(screen, value32,
                        SWATCHWIDTH * i, yy,
                        SWATCHWIDTH, FONTHEIGHT * 2);
      font2x->draw(SWATCHWIDTH * i + (SWATCHWIDTH >> 1) - FONTWIDTH, yy + 1,
                   StringPrintf("%d", (i + 1) % 10));
      
      if (value == current_value) {
        uint32 outline = current_value > 0x77 ? 0xFF000000 : 0xFFFFFF00;

        for (int w = 0; w < 3; w++) {
          sdlutil::DrawBox32(screen,
                             SWATCHWIDTH * i + w, yy + w,
                             SWATCHWIDTH - w * 2, FONTHEIGHT * 2 - w * 2,
                             outline);
        }
      }
    }

    break;
  }
  }
}

struct Typewriter {
  Typewriter(Font *fon, int x, int y, int w, int h) :
    fon(fon),
    startx(x), starty(y),
    width(w), height(h) { }

  // Assumes
  void Write(const string &s) {
    int w = fon->sizex(s);
    if (current_line_width > 0 &&
        current_line_width + w > width) {
      current_line++;
      current_line_width = 0;
    }
    fon->draw(startx + current_line_width,
              starty + current_line * fon->height,
              s);
    current_line_width += w;
  }

  void Newline() {
    current_line++;
    current_line_width = 0;
  }

  int current_line = 0;
  int current_line_width = 0;
  Font *fon = nullptr;
  const int startx = 0, starty = 0, width = 0, height = 0;
};


void UI::Draw() {
  // Status stuff, always outside the 1920x1080 window.
  DrawStatus();

  // On-screen stuff
  DrawImg();
  DrawFft();
}

int main(int argc, char **argv) {
  /* Initialize SDL and network, if we're using it. */
  CHECK(SDL_Init(SDL_INIT_VIDEO |
		 SDL_INIT_TIMER |
		 SDL_INIT_AUDIO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
		      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  SDL_Surface *icon = SDL_LoadBMP("drawfft.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);
  
  font = Font::create(screen,
		      "font.png",
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";


  
  font2x = Font::CreateX(2,
			 screen,
			 "font.png",
			 FONTCHARS,
			 FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font2x != nullptr) << "Couldn't load font.";

  font4x = Font::CreateX(4,
			 screen,
			 "font.png",
			 FONTCHARS,
			 FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font4x != nullptr) << "Couldn't load font.";

  CHECK((cursor_arrow = Cursor::MakeArrow()));
  CHECK((cursor_bucket = Cursor::MakeBucket()));
  CHECK((cursor_hand = Cursor::MakeHand()));
  CHECK((cursor_hand_closed = Cursor::MakeHandClosed()));
  CHECK((cursor_eraser = Cursor::MakeEraser()));

  SDL_SetCursor(cursor_arrow);
  SDL_ShowCursor(SDL_ENABLE);

  UI ui;
  
  ui.Loop();

  SDL_Quit();
  return 0;
}

