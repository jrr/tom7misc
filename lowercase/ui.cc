
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>

#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "timer.h"

#include "unblinder.h"
#include "unblinder-mk0.h"

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/stb_truetype.h"
#include "../cc-lib/image.h"
#include "../cc-lib/util.h"
#include "../cc-lib/lines.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr;
#define SCREENW 1920
#define SCREENH 1280
static SDL_Surface *screen = nullptr;

enum class Mode {
  BITMAP,
};

struct TTFont {
  TTFont(const string &filename) {
    ttf_bytes = Util::ReadFileBytes(filename);
    CHECK(!ttf_bytes.empty()) << filename;
  
    stbtt_InitFont(&font, ttf_bytes.data(), stbtt_GetFontOffsetForIndex(ttf_bytes.data(), 0));
  }

  // Not cached, so this does a lot more allocation than you probably want.
  ImageA GetChar(char c, int size) {
    int width, height;
    uint8 *bitmap = stbtt_GetCodepointBitmap(&font, 0, stbtt_ScaleForPixelHeight(&font, size),
					     c, &width, &height, 0, 0);
    CHECK(bitmap != nullptr) << "Character " << (char)c << " size " << size;

    const int bytes = width * height;
    vector<uint8> ret;
    ret.resize(bytes);
    memcpy(ret.data(), bitmap, bytes);
    stbtt_FreeBitmap(bitmap, nullptr);
    return ImageA(std::move(ret), width, height);
  }

  // Pass DrawPixel(int x, int y, uint8 v) which should do the pixel blending.
  // XXX y position is the baseline, I think, but I have not really tested this.
  template<class DP>
  void BlitString(int x, int y, int size_px,
		  const string &text, const DP &DrawPixel,
		  bool subpixel = true) {
    const float scale = stbtt_ScaleForPixelHeight(&font, size_px);

    const int baseline = [&]() {
	int ascent = 0;
	stbtt_GetFontVMetrics(&font, &ascent, 0, 0);
	return (int) (ascent * scale);
      }();

    const int ypos = y + baseline;
    // Should stay integral if subpixel is false.
    float xpos = x;
    for (int idx = 0; idx < (int)text.size(); idx++) {

      int advance = 0, left_side_bearing = 0;
      stbtt_GetCodepointHMetrics(&font, text[idx], &advance, &left_side_bearing);

      int bitmap_w = 0, bitmap_h = 0;
      int xoff = 0, yoff = 0;
      uint8 *bitmap = nullptr;      
      if (subpixel) {
	const float x_shift = xpos - (float) floor(xpos);
	constexpr float y_shift = 0.0f;
	bitmap = stbtt_GetCodepointBitmapSubpixel(&font, scale, scale,
						  x_shift, y_shift,
						  text[idx],
						  &bitmap_w, &bitmap_h,
						  &xoff, &yoff);
      } else {
	bitmap = stbtt_GetCodepointBitmap(&font, scale, scale,
					  text[idx],
					  &bitmap_w, &bitmap_h,
					  &xoff, &yoff);
      }
      if (bitmap != nullptr) {
	for (int yy = 0; yy < bitmap_h; yy++) {
	  for (int xx = 0; xx < bitmap_w; xx++) {
	    DrawPixel(xpos + xx + xoff, ypos + yy + yoff, bitmap[yy * bitmap_w + xx]);
	  }
	}
	stbtt_FreeBitmap(bitmap, nullptr);
      }
	
      xpos += advance * scale;
      if (text[idx + 1] != '\0') {
	xpos += scale * stbtt_GetCodepointKernAdvance(&font, text[idx], text[idx + 1]);
      }
      
      if (!subpixel) {
	// Or floor?
	xpos = roundf(xpos);
      }
    }
  }

  // Measure the nominal width and height of the string using the same method as above.
  // (This does not mean that all pixels lie within the rectangle.)
  std::pair<int, int> MeasureString(const string &text, int size_px, bool subpixel = true) {
    const float scale = stbtt_ScaleForPixelHeight(&font, size_px);

    int ascent = 0, descent = 0, line_gap = 0;
    stbtt_GetFontVMetrics(&font, &ascent, &descent, &line_gap);

    float xpos = 0.0f;
    for (int idx = 0; idx < (int)text.size(); idx++) {

      int advance = 0, left_side_bearing = 0;
      stbtt_GetCodepointHMetrics(&font, text[idx], &advance, &left_side_bearing);

      xpos += advance * scale;
      if (text[idx + 1] != '\0') {
	xpos += scale * stbtt_GetCodepointKernAdvance(&font, text[idx], text[idx + 1]);
      }
      
      if (!subpixel) {
	// Or floor?
	xpos = roundf(xpos);
      }
    }
    return {xpos, ascent - descent + line_gap};
  }

  enum class PathType {
    LINE,
    // Quadratic bezier (one control point).
    BEZIER,
  };

  struct Path {
    PathType type = PathType::LINE;
    int x = 0, y = 0;
    // For Bezier curves.
    int cx = 0, cy = 0;

    Path(int x, int y) : type(PathType::LINE), x(x), y(y) {}
    Path(int x, int y, int cx, int cy) :
      type(PathType::BEZIER), x(x), y(y), cx(cx), cy(cy) {}
  };
  
  // Note: Source integer coordinates are int16.
  // XXX is this expected to be closed?
  struct Contour {
    int startx = 0, starty = 0;
    vector<Path> paths;
    Contour(int startx, int starty) : startx(startx), starty(starty) {}
  };

  // Can be empty (e.g. for 'space' character)
  vector<Contour> GetContours(int codepoint) {
    stbtt_vertex *vertices = nullptr;
    const int n = stbtt_GetCodepointShape(&font, codepoint, &vertices);
    if (n == 0) return {};
    CHECK(vertices != nullptr);

    CHECK(vertices[0].type == STBTT_vmove) << "All shapes should start with a moveto?";
    std::optional<Contour> cur;
    vector<Contour> out;
    for (int i = 0; i < n; i++) {
      const stbtt_vertex &v = vertices[i];
      switch (v.type) {
      case STBTT_vmove:
	if (cur.has_value()) {
	  out.emplace_back(cur.value());
	}
	cur.emplace(v.x, v.y);
	break;
      case STBTT_vline:
	CHECK(cur.has_value());
	cur.value().paths.emplace_back(v.x, v.y);
	break;
      case STBTT_vcurve:
	printf("vcurve\n");
	CHECK(cur.has_value());
	cur.value().paths.emplace_back(v.x, v.y, v.cx, v.cy);
	break;
      case STBTT_vcubic:
	CHECK(false) << "Expected only quadratic splines in TTF :(";
	break;
      }
    }
    CHECK(cur.has_value());
    out.emplace_back(cur.value());
    
    stbtt_FreeShape(&font, vertices);
    return out;
  }
  
private:
  vector<uint8> ttf_bytes;
  stbtt_fontinfo font;
};


namespace {
struct UI {
  Mode mode = Mode::BITMAP;
  bool ui_dirty = true, output_dirty = true;
  
  std::unique_ptr<Unblinder> unblinder;

  UI();
  void Loop();
  void Draw();
  
  uint64 current_bitmap = 0xFFFF00000000FFFFULL;
  Position current_prediction;
  // Position current_position;

  TTFont times{"times.ttf"};
  
  static constexpr int BITX = 260, BITY = 64, BITSCALE = 32;
  static constexpr int OUTX = 560, OUTY = 64, OUTSCALE = 32;
};
}  // namespace

UI::UI() {

  fflush(stderr);
}

void UI::Loop() {
  Mode mode = Mode::BITMAP;
  
  int mousex = 0, mousey = 0;
  (void)mousex; (void)mousey;
  for (;;) {

    SDL_Event event;
    if (SDL_PollEvent(&event)) {
      switch (event.type) {
      case SDL_QUIT:
	printf("QUIT.\n");
	return;

      case SDL_MOUSEMOTION: {
	SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;

	mousex = e->x;
	mousey = e->y;

	// Do dragging...
	break;
      }

      case SDL_KEYDOWN: {
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  printf("ESCAPE.\n");
	  return;
	default:;
	}
      }

      case SDL_MOUSEBUTTONDOWN: {
	// LMB/RMB, drag, etc.
	if (mode == Mode::BITMAP) {
	  // ...
	}
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

void UI::Draw() {
  font->draw(2, 2, "hi");

  switch (mode) {
  case Mode::BITMAP:

    char c = 'a';

    vector<TTFont::Contour> contours = times.GetContours(c);

    for (const auto &contour : contours) {
      printf("Contour:\n"
	     "start %d,%d\n", contour.startx, contour.starty);

      int x = contour.startx, y = contour.starty;

      
      for (const auto &p : contour.paths) {
	switch (p.type) {
	case TTFont::PathType::LINE:
	  // printf("  lineto %d,%d\n", p.x, p.y);
	  sdlutil::DrawClipLine32(screen, x, y, p.x, p.y,
				  0xFF0000FF);
	  x = p.x;
	  y = p.y;
	  break;
	case TTFont::PathType::BEZIER:
	  // printf("  bezier %d,%d (%d, %d)\n", p.x, p.y, p.cx, p.cy);
	  for (const auto [px, py] : 
		 TesselateQuadraticBezier<float>(x, y, p.cx, p.cy, p.x, p.y, 1.0)) {
	    sdlutil::DrawClipLine32(screen, x, y, (int)round(px), (int)roundf(py),
				    0xFF0000FF);
	    x = (int)roundf(px);
	    y = (int)roundf(py);
	  }
	  break;
	}
      }
    }
    
    break;
  }

  // Always draw output...
  
}

      
int main(int argc, char **argv) {

  // XXX This is specific to my machine. You probably want to remove it.
  // Assumes that processors 0-16 are available.
  // CHECK(SetProcessAffinityMask(GetCurrentProcess(), 0xF));

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

  SDL_Surface *icon = SDL_LoadBMP("unblind-icon.bmp");
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

  UI ui;
  ui.Loop();
  
  SDL_Quit();
  return 0;
}

