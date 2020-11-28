
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

    /*
      stbtt_GetFontBoundingBox(&font, &min_x, &min_y, &max_x, &max_y);
      printf("Bounding box for all chars: %d,%d -> %d,%d\n",
      min_x, min_y, max_x, max_y);
      max_height = max_y - min_y;
      norm = 1.0f / max_height;
    */

    stbtt_GetFontVMetrics(&font, &native_ascent, &native_descent, nullptr);

    // We use a normalized representation like this:
    //
    //      0,0
    //      +------------------------ ascent = 0.0
    //      |
    //      |
    //      |      ,---,
    //      |     |     |
    //      |     `.___.`
    //      |
    //      |       ___
    //      |      /   |
    //      |      `.  |
    //      |       |  |
    //      |       |  |
    //      |       |  |
    //      |       |  |
    //      +-------+--+------------- baseline = 0.75
    //      |       |  |
    //      |      /   ;
    //      |     /   /
    //      |    /   /
    //     -+---`   /
    //    | |      /
    //    `-+------  ---------------- descent = 1.0
    //
    // Note, y-axis is flipped.
    //
    // Note the coordinates can be negative or greater than 1.0, but
    // they are "nominally" in [0,1] vertically. It is normal for
    // horizontal coordinates to be significantly larger than 1.

    int height = native_ascent - native_descent;
    norm = 1.0f / height;
    baseline = native_ascent * norm;
  }

  // font uses "y positive up" coordinates.
  // ascent is the coordinate above the baseline (typically positive) and
  // descent is the coordinate below the baseline (typically negative).
  int native_ascent = 0, native_descent = 0;
  float norm = 0.0;
  float baseline = 0.0f;

  // Normalizes an input coordinate (which is usually int16) to be
  // *nominally* in the unit rectangle. 
  pair<float, float> Norm(float x, float y) {
    // x coordinate is easy; just scale by the same factor.
    x = norm * x;
    // y is flipped (want +y downward) and offset (want ascent to be 0.0).

    // flip around baseline
    y = -y;
    // baseline (0) becomes ascent
    y += native_ascent;
    y = norm * y;
    return {x, y};
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
	// printf("vcurve\n");
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
  bool ui_dirty = true;

  char current_char = 'a';
  
  UI();
  void Loop();
  void Draw();
  
  TTFont times{"times.ttf"};
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
	case SDLK_LEFT:
	  if (current_char > ' ') current_char--;
	  ui_dirty = true;
	  break;
	case SDLK_RIGHT:
	  if (current_char < '~') current_char++;
	  ui_dirty = true;
	  break;
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

    vector<TTFont::Contour> contours = times.GetContours(current_char);

    constexpr int XPOS = 128;
    constexpr int YPOS = 48;
    // Basically, the height of the font in pixels.
    constexpr int SCALE = 1000;
    auto Line = [&](float x1, float y1, float x2, float y2, uint32 color) {
	sdlutil::drawclipline(screen,
			      XPOS + x1 * SCALE, YPOS + y1 * SCALE,
			      XPOS + x2 * SCALE, YPOS + y2 * SCALE,
			      0xFF & (color >> 24),
			      0xFF & (color >> 16),
			      0xFF & (color >> 8));
      };

    // One screen pixel in normalized coordinates.
    double sqerr = 1.0f / (SCALE * SCALE);
    
    for (const auto &contour : contours) {
      auto [x, y] = times.Norm(contour.startx, contour.starty);
      printf("Contour:\n"
	     "start %d,%d (norm %.4f %.4f)\n", contour.startx, contour.starty,
	     x, y);
      for (const auto &p : contour.paths) {
	switch (p.type) {
	case TTFont::PathType::LINE: {
	  // printf("  lineto %d,%d\n", p.x, p.y);
	  auto [px, py] = times.Norm(p.x, p.y);
	  Line(x, y, px, py, 0x000000FF);
	  x = px;
	  y = py;
	  break;
	}
	case TTFont::PathType::BEZIER: {
	  // printf("  bezier %d,%d (%d, %d)\n", p.x, p.y, p.cx, p.cy);
	  auto [cx, cy] = times.Norm(p.cx, p.cy);
	  auto [px, py] = times.Norm(p.x, p.y);
	  // Line(x, y, cx, cy, 0x00FF00FF);
	  // Line(cx, cy, px, py, 0x0000FFFF);

	  for (const auto [xx, yy] : 
		 TesselateQuadraticBezier<double>(x, y, cx, cy, px, py, sqerr)) {
	    // times.Norm(1, 1.0f / 1000.0f).second)) {
	    Line(x, y, xx, yy, 0xFF0000FF);
	    x = xx;
	    y = yy;
	  }
	  break;
	}
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

  SDL_Surface *icon = SDL_LoadBMP("lowercase-icon.bmp");
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

