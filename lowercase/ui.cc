
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <unordered_set>
#include <unordered_map>
#include <utility>
#include <optional>

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
#include "../cc-lib/re2/re2.h"

#include "opt/opt.h"

#include "ttfops.h"
#include "ttf.h"
#include "fontdb.h"
#include "font-problem.h"
#include "loadfonts.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
[[maybe_unused]]
static Font *font = nullptr, *font2x = nullptr;
#define SCREENW 1920
#define SCREENH 1280
static SDL_Surface *screen = nullptr;

enum class Mode {
  BITMAP,
  SORTITION,
  SDFTEST,
  SCALETEST,
  LOOPTEST,
};

namespace {
using Type = FontDB::Type;
using Flag = FontDB::Flag;

static constexpr int WINDOW = 10;

struct UI {
  Mode mode = Mode::SORTITION;
  bool ui_dirty = true;
  // quit confirmations.
  int confirmations = 0;

  FontDB fontdb;

  // All the fonts we want to look at in this instance.
  vector<string> cur_filenames;

  UI();
  // set font at cur to the given type
  void SetType(Type t);
  void SetFlag(Flag f, bool on);
  void Loop();
  void Draw();

  void RecomputeLoop();
  
  int DrawChar(TTF *ttf, int sx, int sy, float scale, char c, char nc);
  int DrawString(TTF *ttf, int sx, int sy, float scale, const string &str);
  int DrawAlphabet(TTF *ttf, int sx, int sy, float scale);

  // Drawing for various modes
  void DrawSortition();
  void DrawSDF();
  
  // current index (into cur_filenames, fonts, etc.) that we act
  // on with keypresses etc.
  int cur = 0;

  // Parallel to cur_filenames, but not usually
  // as long. Use GetFont(idx).
  vector<TTF *> fonts;

  TTF *GetFont(int idx) {
    CHECK(idx < cur_filenames.size());
    while (idx >= fonts.size()) {
      fonts.push_back(nullptr);
    }

    if (fonts[idx] == nullptr) {
      printf("Load %s...\n", cur_filenames[idx].c_str());
      fflush(stdout);
      fonts[idx] = new TTF(cur_filenames[idx]);
    }

    return fonts[idx];
  }

  int current_scale = 100;
  bool draw_points = true;

  ArcFour rc{"lowercase"};

  // Convert lines to beziers.
  bool only_bezier = false;
  bool normalize = false;
  // XXX
  char current_char = 'a';
  float current_xscale = 1.0, current_yscale = 1.0;
  float current_xoff = 0.0, current_yoff = 0.0;
  TTF times{"times.ttf"};

  vector<FontProblem::Point> looptest_expected;
  vector<FontProblem::Point> looptest_actual;  
  std::optional<FontProblem::LoopAssignment> looptest_assignment;
};
}  // namespace




// Render the two characters to bitmaps at the given scale, and then
// compute the difference as the fraction of pixels in the second
// bitmap that are the same in the first.
// (Debug version of the code in ttfops.)
static
double BitmapDifference(const TTF &ttf,
			int c1, int c2,
			// Determines the base bitmap size for both
			// characters. c1 is unstretched.
			float scale,
			// Additional scale for c2, which can stretch it.
			// (we use scale * xscale2, scale * yscale2)
			float xscale2, float yscale2,
			// Offsets for c2. Maybe depends on scale?
			float xmov2, float ymov2,
			bool draw = false) {
  
  const stbtt_fontinfo *info = ttf.Font();
  CHECK(info != nullptr);

  if (draw) {
    printf("char %d scale %.2f\n", c1, scale);
    fflush(stdout);
  }

  float stb_scale = stbtt_ScaleForPixelHeight(info, scale);
  
  int width1, height1, xoff1, yoff1;
  uint8 *bit1 = stbtt_GetCodepointBitmapSubpixel(info,
						 // uniform scale
						 stb_scale, stb_scale,
						 // unshifted
						 0.0f, 0.0f,
						 c1,
						 &width1, &height1,
						 &xoff1, &yoff1);
  CHECK(bit1 != nullptr);

  // Get integral part and remainder (always in [0, 1)).
  auto SubPx = [](float f) -> pair<int, float> {
    int i = floorf(f);
    return {i, f - i};
  };

  const auto [int_x, subpixel_x] = SubPx(xmov2);
  const auto [int_y, subpixel_y] = SubPx(ymov2);

  #if 0
  int int_x = xmov2, int_y = ymov2;
  const float subpixel_x = xmov2 - int_x;
  const float subpixel_y = ymov2 - int_y;
#endif

  if (draw) {
    printf ("scale %.5f %.5f\n",
	    stb_scale * xscale2, stb_scale * yscale2);
  }
  
  int width2, height2, xoff2, yoff2;
  uint8 *bit2 = stbtt_GetCodepointBitmapSubpixel(info,
						 stb_scale * xscale2, stb_scale * yscale2,
						 subpixel_x, subpixel_y,
						 c2,
						 &width2, &height2,
						 &xoff2, &yoff2);

  CHECK(bit2 != nullptr);  

  // Draw to screen (separately).
  if (draw) {
    constexpr int X1 = 10, Y1 = 200;
    constexpr int X2 = 400, Y2 = 200;  

    auto DrawBitmap = [](int startx, int starty, uint8 *bm,
			 int width, int height,
			 uint8 rmask, uint8 gmask, uint8 bmask) {

	int idx = 0;
	for (int y = 0; y < height; y++) {
	  int yy = starty + y;
	  for (int x = 0; x < width; x++) {
	    uint8 v = bm[idx];
	    int xx = startx + x;
	    sdlutil::drawclippixel(screen, xx, yy,
				   v & rmask, v & gmask, v & bmask);
	    idx++;
	  }
	}
      };

    DrawBitmap(X1, Y1, bit1, width1, height1, 0xFF, 0x00, 0x00);
    DrawBitmap(X2, Y2, bit2, width2, height2, 0x00, 0xFF, 0x00);      

    font->draw(X1, Y1 - font->height, StringPrintf("^3%d^1x^3%d", width1, height1));
    font->draw(X2, Y2 - font->height, StringPrintf("^3%d^1x^3%d", width2, height2));  

    auto Locate = [](int x, int y) {
	for (int dy : {-1, 0, 1}) {
	  for (int dx : {-1, 0, 1}) {
	    if (dy ==0 && dx == 0) {
	      sdlutil::drawclippixel(screen, x + dx, y + dy, 0x00, 0x00, 0xFF);
	    } else {
	      sdlutil::drawclippixel(screen, x + dx, y + dy, 0xFF, 0xFF, 0xFF);
	    }
	  }
	}
      };

    Locate(X1 - xoff1, Y1 - yoff1);
    Locate(X2 - xoff2, Y2 - yoff2);  
  }

  // overlapping style.
  
  // Here we're working in a coordinate space where 0,0 is the origin for both
  // characters, and the scale is in pixels of the rendered bitmaps. The top left
  // of the bitmap (and the bounding box containing both bitmaps) is typically
  // negative:
  const int minx = std::min(xoff1, int_x + xoff2);
  const int miny = std::min(yoff1, int_y + yoff2);
  // (one past the right, bottom)
  const int maxx = std::max(width1 + xoff1,  width2 + int_x + xoff2);
  const int maxy = std::max(height1 + yoff1, height2 + int_y + yoff2);

  // Width and height of this bounding box.
  const int bbw = maxx - minx, bbh = maxy - miny;

  const int BX = 100, BY = 500;

  auto GetPx = [](const uint8 *bm, int width, int height, int x, int y) -> uint8 {
      // Empty outside the bitmap itself.
      if (x < 0 || y < 0 ||
	  x >= width || y >= height) return 0;
      return bm[y * width + x];
    };

  if (draw) {
  font->draw(BX, BY - font->height,
	     StringPrintf("min: %d,%d  max %d,%d  off1 %d,%d  off2 %d,%d",
			  minx, miny, maxx, maxy,
			  xoff1, yoff1,
			  xoff2, yoff2));
  }

  // For each pixel in the second bitmap, absolute difference between it
  // and the the aligned pixel in the first. Max difference per pixel 255.
  int numer255 = 0;
  const int denom = width2 * height2;
  // x,y now pixel coordinates in the bounding box.
  for (int y = 0; y < bbh; y++) {
    for (int x = 0; x < bbw; x++) {
      // coordinates in the shared font space
      const int ox = x + minx;
      const int oy = y + miny;

      // And coordinates within each bitmap (but they may actually be
      // out of bounds).
      const int x1 = ox - xoff1;
      const int y1 = oy - yoff1;

      const int x2 = ox - (xoff2 + int_x);
      const int y2 = oy - (yoff2 + int_y);

      // Pixel values for each bitmap.
      // Here 0 means transparent.
      const uint8 v1 = GetPx(bit1, width1, height1, x1, y1);
      const uint8 v2 = GetPx(bit2, width2, height2, x2, y2);

      if (x2 >= 0 && y2 >= 0 &&
	  x2 < width2 && y2 < height2) {
	numer255 += abs((int)v2 - (int)v1);
      }
      
      if (draw) {
	sdlutil::drawclippixel(screen, BX + x, BY + y, v1, v2, 0);
      }
    }
  }

  const double err = numer255 / (denom * 255.0);

  if (draw) {
    font->draw(BX, BY + bbh + 4, StringPrintf("Error: %.2f / %d px = %.4f%%",
					      numer255 / 255.0,
					      denom,
					      err * 100.0));
  }
  
  if (draw) {
    printf("Freeing..\n");
    fflush(stdout);
  }
  
  stbtt_FreeBitmap(bit1, nullptr);
  stbtt_FreeBitmap(bit2, nullptr);
  return err;
}


UI::UI() {
  int64 sorted = 0, unsorted = 0;
  int64 case_marked = 0, case_unmarked = 0;  
  // Done: random, (di)stressed, laser, helvetica, antique
  // "sans serif", serif, 3D, outline, handwriting, hand
  // shadow, arial, mirror, flipped, hollow, bats, icon
  // typewriter, geometric, times, bodoni, futura, basker,
  // courier, gothic, stencil, comic, ransom, modern, book
  // ITC, google, consol, deco, sans, frutiger, univer, cent,
  // andale, mono

  RE2 required = ".*"; // "Fantom.*"; // ".*Coal Train.*";

  for (const auto &[filename, info] : fontdb.Files()) {
    if (info.type == Type::UNKNOWN) {
      unsorted++;
    } else {
      sorted++;
    }

    if (info.flags.find(Flag::SAME_CASE) != info.flags.end()) {
      case_marked++;
    } else {
      case_unmarked++;
    }

    if (RE2::FullMatch(filename, required) &&
	info.type != Type::BROKEN) {

      // Only unlabeled ones while working...
      if (info.type == Type::UNKNOWN) {
	cur_filenames.push_back(filename);
      }
    }
  }

  std::sort(cur_filenames.begin(),
	    cur_filenames.end(),
	    [](const string &a, const string &b) {
	      auto aa = FontDB::GetBaseFilename(a);
	      auto bb = FontDB::GetBaseFilename(b);
	      return aa < bb;
	    });
  
  // Optional. Might be good to keep fonts in the same family
  // together, really..
  // Shuffle(&rc, &cur_filenames);

  // Instead of shuffling, just "cut the deck".
  {
    int idx = RandTo(&rc, cur_filenames.size());
    std::vector<std::string> cut;
    cut.reserve(cur_filenames.size());
    for (int i = idx; i < cur_filenames.size(); i++)
      cut.emplace_back(std::move(cur_filenames[i]));
    for (int i = 0; i < idx; i++)
      cut.emplace_back(std::move(cur_filenames[i]));
    cur_filenames = std::move(cut);
  }
  
  printf("All fonts: %d\n"
         "Sorted fonts: %lld\n"
         "Unsorted fonts: %lld\n"
	 "Case marked: %lld\n"
	 "Case unmarked: %lld\n",
	 fontdb.Size(),
	 sorted,
	 unsorted,
	 case_marked,
	 case_unmarked);
}

void UI::SetType(Type t) {
  fontdb.AssignType(cur_filenames[cur], t);
  if (cur < cur_filenames.size() - 1) {
    cur++;
  }
  ui_dirty = true;
}

void UI::SetFlag(Flag f, bool on) {
  fontdb.SetFlag(cur_filenames[cur], f, on);
  if (cur < cur_filenames.size() - 1) {
    cur++;
  }
  ui_dirty = true;
}

void UI::RecomputeLoop() {
  if (looptest_expected.size() > 0 &&
      looptest_actual.size() >= looptest_expected.size()) {
    printf("Expected:\n");
    for (const auto &[x, y] : looptest_expected) {
      printf("  {%d,%d},\n", (int)x, (int)y);
    }
    printf("Actual:\n");
    for (const auto &[x, y] : looptest_actual) {
      printf("  {%d,%d},\n", (int)x, (int)y);
    }
    
    Timer assn_timer;
    looptest_assignment.emplace(
	FontProblem::BestLoopAssignment(&rc,
					looptest_expected,
					looptest_actual));
    printf("Computed assignment in %.5fms\n",
	   assn_timer.MS());
  } else {
    looptest_assignment.reset();
  }
}

void UI::Loop() {

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

      case SDL_MOUSEBUTTONDOWN: {
        // LMB/RMB, drag, etc.
	SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;
        mousex = e->x;
        mousey = e->y;

	if (mode == Mode::LOOPTEST) {
	  if (e->button == SDL_BUTTON_LEFT) {
	    looptest_expected.emplace_back((float)mousex, (float)mousey);
	  } else {
	    looptest_actual.emplace_back((float)mousex, (float)mousey);
	  }

	  RecomputeLoop();

	  ui_dirty = true;
	}

	break;
      }
	
      case SDL_KEYDOWN: {

	// Need to press this consecutively in order
	// to exit without saving.
	if (event.key.keysym.sym != SDLK_ESCAPE)
	  confirmations = 0;

	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  printf("ESCAPE.\n");
	  if (fontdb.Dirty()) {
	    ui_dirty = true;
	    confirmations++;
	    if (confirmations >= 5)
	      return;
	  } else {
	    return;
	  }
	  break;

	case SDLK_a: {
	  
	  if (mode == Mode::SCALETEST) {
	    printf("Solving...\n");

	    TTF *ttf = GetFont(cur);
	    CHECK(ttf != nullptr);
	    
	    auto GetErr = [ttf](const std::array<double, 4> &args) {
		const auto [xscale2, yscale2, xoff2, yoff2] = args;
		return 
		  BitmapDifference(*ttf,
				   'A', 'a',
				   200,
				   xscale2, yscale2,
				   xoff2, yoff2,
				   // don't draw
				   false);
	      };

	    const auto [args, err] = 
	      Opt::Minimize<4>(GetErr, {0.1, 0.1, -50.0, -50.0}, {10.0, 10.0, 50.0, 50.0},
			       1000);

	    const auto [xscale2, yscale2, xoff2, yoff2] = args;
	    
	    printf("Done! Error: %.4f%%\n", err * 100.0);
	    current_xscale = xscale2;
	    current_yscale = yscale2;
	    current_xoff = xoff2;
	    current_yoff = yoff2;

	    double rerr = GetErr({xscale2, yscale2, xoff2, yoff2});
	    printf("Recomputed: %.4f%%\n", rerr * 100.0);
	    
	    ui_dirty = true;
	  }
	  break;
	}
	  
	case SDLK_TAB:
	  printf("TAB.\n");
	  switch (mode) {
	  default:
	  case Mode::BITMAP:
	    mode = Mode::SORTITION;
	    break;
	  case Mode::SORTITION:
	    mode = Mode::SDFTEST;
	    break;
	  case Mode::SDFTEST:
	    mode = Mode::SCALETEST;
	    break;
	  case Mode::SCALETEST:
	    mode = Mode::LOOPTEST;
	    break;
	  case Mode::LOOPTEST:
	    mode = Mode::BITMAP;
	    break;
	  }
	  ui_dirty = true;
	  break;

	case SDLK_0:
	  current_xoff = current_yoff = 0.0f;
	  current_scale = 100;
	  current_xscale = current_yscale = 1.0f;
	  ui_dirty = true;
	  break;

	case SDLK_PAGEUP:
	case SDLK_PAGEDOWN:
	case SDLK_UP:
	case SDLK_DOWN:
	case SDLK_LEFT:
	case SDLK_RIGHT: {
	  int dx = event.key.keysym.sym == SDLK_RIGHT ? 1 :
	    event.key.keysym.sym == SDLK_LEFT ? -1 : 0;
	  int dy = event.key.keysym.sym == SDLK_DOWN ? 1 :
	    event.key.keysym.sym == SDLK_UP ? -1 :
	    event.key.keysym.sym == SDLK_PAGEUP ? -10 :
	    event.key.keysym.sym == SDLK_PAGEDOWN ? 10 :
	    0;

	  switch (mode) {
	  case Mode::LOOPTEST:
	    break;
	  
	  case Mode::SORTITION:
	    cur += dy;
	    if (cur < 0) cur = 0;
	    if (cur >= cur_filenames.size())
	      cur = cur_filenames.size() - 1;
	    ui_dirty = true;
	    break;
	    
	  case Mode::BITMAP:
	  case Mode::SDFTEST:
	    cur += dy;
	    if (cur < 0) cur = 0;
	    if (cur >= cur_filenames.size()) cur = cur_filenames.size() - 1;
	    
	    current_char += dx;
	    if (current_char > 'z') current_char = 'z';
	    else if (current_char < 'A') current_char = 'A';
	    ui_dirty = true;
	    break;

	  case Mode::SCALETEST: {
	    float fdx = dx;
	    float fdy = dy;
	    if (event.key.keysym.mod & KMOD_SHIFT) {
	      fdx *= 10;
	      fdy *= 10;
	    } else if (event.key.keysym.mod & KMOD_ALT) {
	      fdx *= 0.1;
	      fdy *= 0.1;
	    }
	      
	    if (event.key.keysym.mod & KMOD_CTRL) {
	      current_xscale += 0.05 * fdx;
	      if (current_xscale < 0.01) current_xscale = 0.01;

	      current_yscale += 0.05 * fdy;
	      if (current_yscale < 0.01) current_yscale = 0.01;

	    } else {
	      current_xoff += fdx;
	      current_yoff += fdy;
	    }

	    ui_dirty = true;
	    break;
	  }
	  }
	  break;
	}


	case SDLK_PERIOD:
	  draw_points = !draw_points;
	  ui_dirty = true;
	  break;

	case SDLK_2:
	  if (event.key.keysym.mod & KMOD_SHIFT) {
	    printf("AT\n");
	    only_bezier = !only_bezier;
	    ui_dirty = true;
	  }
	  break;

	case SDLK_6:
	  if (event.key.keysym.mod & KMOD_SHIFT) {
	    printf("CARET\n");
	    normalize = !normalize;
	    ui_dirty = true;
	  }
	  break;
	  
	case SDLK_PLUS:
	case SDLK_EQUALS:
	  current_scale += 5;
	  ui_dirty = true;
	  break;

	case SDLK_MINUS:
	  if (current_scale > 15)
	    current_scale -= 5;
	  ui_dirty = true;
	  break;

	case SDLK_c:
	  SetFlag(Flag::SAME_CASE, false);
	  break;
	case SDLK_x:
	  SetFlag(Flag::SAME_CASE, true);
	  break;
	  
	case SDLK_g:
	  SetType(Type::SANS);
	  break;

	case SDLK_s:
	  SetType(Type::SERIF);
	  break;

	case SDLK_i:
	  SetType(Type::DINGBATS);
	  break;

	case SDLK_f:
	  SetType(Type::FANCY);
	  break;

	case SDLK_t:
	  SetType(Type::TECHNO);
	  break;

	case SDLK_d:
	  SetType(Type::DECORATIVE);
	  break;

	case SDLK_m:
	  SetType(Type::MESSY);
	  break;

	case SDLK_o:
	  SetType(Type::OTHER);
	  break;

	case SDLK_b:
	  SetType(Type::BROKEN);
	  break;

	case SDLK_v:
	  fontdb.Save();
	  ui_dirty = true;
	  break;


	default:;
	}
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


// scale is basically the height in pixels
// returns the pixel width to advance, with kerning if nc != 0
int UI::DrawChar(TTF *ttf, int sx, int sy, float scale, char c, char nc) {
  vector<TTF::Contour> contours = ttf->GetContours(c);

  auto Line = [&](float x1, float y1, float x2, float y2, uint32 color) {
      sdlutil::drawclipline(screen,
			    sx + x1 * scale, sy + y1 * scale,
			    sx + x2 * scale, sy + y2 * scale,
			    0xFF & (color >> 24),
			    0xFF & (color >> 16),
			    0xFF & (color >> 8));
    };

  // One screen pixel in normalized coordinates.
  const double sqerr = 1.0f / (scale * scale);

  for (const auto &contour : contours) {
    float x = contour.startx;
    float y = contour.starty;
    for (const auto &p : contour.paths) {
      switch (p.type) {
      case TTF::PathType::LINE: {
	Line(x, y, p.x, p.y, 0x000000FF);
	x = p.x;
	y = p.y;
	break;
      }
      case TTF::PathType::BEZIER: {
	for (const auto [xx, yy] :
	       TesselateQuadraticBezier<double>(x, y, p.cx, p.cy, p.x, p.y, sqerr)) {
	  Line(x, y, xx, yy, 0xFF0000FF);
	  x = xx;
	  y = yy;
	}
	break;
      }
      }
    }
  }

  auto PointAt = [&](float x, float y) {
      sdlutil::drawclippixel(screen, sx + x * scale, sy + y * scale - 1, 0, 0, 0xFF);

      sdlutil::drawclippixel(screen, sx + x * scale - 1, sy + y * scale, 0, 0, 0xFF);
      sdlutil::drawclippixel(screen, sx + x * scale, sy + y * scale, 0, 0, 0xFF);
      sdlutil::drawclippixel(screen, sx + x * scale - 1, sy + y * scale, 0, 0, 0xFF);

      sdlutil::drawclippixel(screen, sx + x * scale, sy + y * scale + 1, 0, 0, 0xFF);
    };

  if (draw_points) {
    // Now draw vertices to give a hint when there are "too many" control points.
    for (const auto &contour : contours) {
      PointAt(contour.startx, contour.starty);

      for (const auto &p : contour.paths) {
	switch (p.type) {
	case TTF::PathType::LINE: {
	  PointAt(p.x, p.y);
	  break;
	}
	case TTF::PathType::BEZIER: {
	  // auto [cx, cy] = ttf->Norm(p.cx, p.cy);
	  PointAt(p.x, p.y);
	  break;
	}
	}
      }
    }
  }


  return scale * ttf->NormKernAdvance(c, nc);
}


// Returns the (nominal) height used.
int UI::DrawString(TTF *ttf, int sx, int sy, float scale, const string &str) {
  for (int i = 0; i < str.size(); i++) {
    sx += DrawChar(ttf, sx, sy, scale, str[i], str[i + 1]);
  }
  return ttf->NormLineHeight() * scale;
}

// Returns the (nominal) height used.
int UI::DrawAlphabet(TTF *ttf, int sx, int sy, float scale) {
  int h = DrawString(ttf, sx, sy, scale,      "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  int h2 = DrawString(ttf, sx, sy + h, scale, "abcdefghijklmnopqrstuvwxyz");
  return h + h2;
}


void UI::DrawSortition() {

  font2x->draw(12, 4,
	       "[^6G^<]eometric " // Sans
	       "[^6S^<]erif "
	       "[^6F^<]ancy "
	       "[^6T^<]echno "
	       "[^6D^<]ecorative "
	       "[^6M^<]essy "
	       "[^6I^<]cons " // Dingbats
	       "[^6O^<]ther "
	       "[^6B^<]roken");

  if (fontdb.Dirty()) {
    font2x->draw(SCREENW - font2x->width * 8, 4, "(Sa[^6V^<]e?)");
  }
  if (confirmations > 0) {
    font2x->draw(SCREENW - font2x->width * 10, 32, StringPrintf("^2CONFIRM ^5%d",
								confirmations));
  }

  {
    double pct = (100.0 * fontdb.NumSorted()) / (double)fontdb.Size();
    string progress = StringPrintf("^6%.2f^4%%^<  %d^4/^<%d",
				   pct, cur, cur_filenames.size());
    font2x->draw(SCREENW - font2x->sizex(progress) - 8, SCREENH - font2x->height - 2,
		 progress);
  }

    
  int xpos = 64;
  int ypos = 64;
  constexpr int HISTORY = 2;
  for (int i = cur - HISTORY; i < cur + WINDOW - HISTORY; i++) {
    // Can be out of bounds, especially at the beginning...
    if (i >= 0 && i < cur_filenames.size()) {

      float scalescale = 1.0f;
      if (i < cur) scalescale = 0.60f;
      if (i == cur) scalescale = 1.2f;

      float scale = current_scale * scalescale;

      const string &ff = cur_filenames[i];
      std::optional<FontDB::Info> info = fontdb.Lookup(ff);

      string dest = "";
      string cstring = "";
      if (info.has_value()) {
	if (info.value().type != Type::UNKNOWN) {
	  dest = FontDB::TypeString(info.value().type);
	}
	const auto &flags = info.value().flags;
	auto it = flags.find(Flag::SAME_CASE);
	if (it != flags.end()) {
	  if (it->second) {
	    cstring = "^2[X]^< same case";
	  } else {
	    cstring = "^5C^<ase ok";
	  }
	}
      }

      
      string name = FontDB::GetBaseFilename(ff);
      font->draw(xpos, ypos, name);
      ypos += font->height + 1;

      if (i == cur) {
	font2x->draw(xpos - 52, ypos, "^6>^7>^8>");
      }

      font2x->draw(4, ypos + font2x->height, dest);
      font2x->draw(4, ypos + font2x->height * 2, cstring);      

      ypos += DrawAlphabet(GetFont(i), xpos, ypos, scale);
      ypos += 2;
      sdlutil::drawclipline(screen, xpos, ypos, SCREENW - 128, ypos,
			    0xAA, 0xAA, 0xAA);
      ypos += 2;
    } else {
      sdlutil::drawbox(screen, xpos, ypos, SCREENW - 128, 60,
		       0x77, 0x77, 0x77);
      ypos += 64;
    }
  }
}
  
void UI::DrawSDF() {

  // Now through TTF wrapper
#if 0
  const int SDF_SIZE = 64;
  const int PAD = 16;
  const uint8 ONEDGE = 200;

  const float falloff_min =
    (float)ONEDGE / (sqrtf(2.0f) * SDF_SIZE * 0.5f);
  const float falloff_max = (float)ONEDGE / PAD;
  const float FALLOFF = 0.5f * (falloff_min + falloff_max);
  
  printf("Falloff %.4f - %.4f = %.4f\n",
	 falloff_min, falloff_max, FALLOFF);
#endif

  static constexpr int SIZE = 64;
  
  using SDFConfig = FontProblem::SDFConfig;
  SDFConfig config;
  config.sdf_size = SIZE;
  config.pad_top = 4;
  config.pad_bot = 18;
  config.pad_left = 18;
  config.onedge_value = 200;

  const float base_padding = (config.pad_top + config.pad_bot) * 0.5f;
  
  const float falloff_min =
    (float)config.onedge_value / (sqrtf(2.0f) * config.sdf_size * 0.5f);
  const float falloff_max = (float)config.onedge_value / base_padding;
  CHECK(falloff_max > falloff_min);
  // config.falloff_per_pixel = 0.5f * (falloff_min + falloff_max);
  config.falloff_per_pixel = 0.25f * falloff_max + 0.75 * falloff_min;
  printf("Falloff per pixel: %.3f\n", config.falloff_per_pixel);
  
  const TTF *font = GetFont(cur);
  std::optional<ImageA> sdf = font->GetSDF(current_char, config.sdf_size,
					   config.pad_top, config.pad_bot, config.pad_left,
					   config.onedge_value, config.falloff_per_pixel);
  if (sdf.has_value()) {
    constexpr int X1 = 10, Y1 = 200;
    constexpr int X2 = 500, Y2 = 200;  
    constexpr int X3 = 10, Y3 = 500;
    constexpr int X4 = 500, Y4 = 500;
    constexpr int X5 = 10, Y5 = 800;
    constexpr int X6 = 500, Y6 = 800;

    auto DrawBitmap = [](int startx, int starty, const ImageA &sdf) {
	int idx = 0;
	for (int y = 0; y < sdf.Height(); y++) {
	  int yy = starty + y;
	  for (int x = 0; x < sdf.Width(); x++) {
	    uint8 v = sdf.GetPixel(x, y);
	    int xx = startx + x;
	    sdlutil::drawclippixel(screen, xx, yy, v, v, v);
	    idx++;
	  }
	}
      };

    auto DrawBitmapThresh = [](uint8 thresh,
				 int startx, int starty, const ImageA &sdf) {
	int idx = 0;
	for (int y = 0; y < sdf.Height(); y++) {
	  int yy = starty + y;
	  for (int x = 0; x < sdf.Width(); x++) {
	    uint8 v = sdf.GetPixel(x, y);
	    int xx = startx + x;
	    
	    uint8 vv = v >= thresh ? 0xFF : 0x00;
	    sdlutil::drawclippixel(screen, xx, yy, vv, vv, vv);
	    idx++;
	  }
	}
      };

    
    auto DrawBitmap2x = [](int startx, int starty, const ImageA &sdf) {
	int idx = 0;
	for (int y = 0; y < sdf.Height(); y++) {
	  int yy = starty + y * 2;
	  for (int x = 0; x < sdf.Width(); x++) {
	    uint8 v = sdf.GetPixel(x, y);
	    int xx = startx + x * 2;
	    sdlutil::drawclippixel(screen, xx, yy, v, v, v);
	    sdlutil::drawclippixel(screen, xx + 1, yy, v, v, v);
	    sdlutil::drawclippixel(screen, xx, yy + 1, v, v, v);
	    sdlutil::drawclippixel(screen, xx + 1, yy + 1, v, v, v);	    
	    idx++;
	  }
	}
      };

    auto DrawBitmapThresh2x = [](uint8 thresh,
				 int startx, int starty, const ImageA &sdf) {
	int idx = 0;
	for (int y = 0; y < sdf.Height(); y++) {
	  int yy = starty + y * 2;
	  for (int x = 0; x < sdf.Width(); x++) {
	    uint8 v = sdf.GetPixel(x, y);
	    int xx = startx + x * 2;
	    
	    uint8 vv = v >= thresh ? 0xFF : 0x00;
	    sdlutil::drawclippixel(screen, xx, yy, vv, vv, vv);
	    sdlutil::drawclippixel(screen, xx + 1, yy, vv, vv, vv);
	    sdlutil::drawclippixel(screen, xx, yy + 1, vv, vv, vv);
	    sdlutil::drawclippixel(screen, xx + 1, yy + 1, vv, vv, vv);	    
	    
	    idx++;
	  }
	}
      };

  
    DrawBitmap2x(X1, Y1, sdf.value());
    DrawBitmapThresh2x(config.onedge_value, X2, Y2, sdf.value());

    ImageA twox = sdf.value().ResizeBilinear(sdf.value().Width() * 2,
					     sdf.value().Height() * 2);
    DrawBitmap2x(X3, Y3, twox);
    DrawBitmapThresh2x(config.onedge_value, X4, Y4, twox);

    ImageA fourx = sdf.value().ResizeBilinear(sdf.value().Width() * 4,
					      sdf.value().Height() * 4);
    DrawBitmap(X5, Y5, fourx);
    DrawBitmapThresh(config.onedge_value, X6, Y6, fourx);
    
  } else {
    ::font->draw(100, 100, "No SDF");
  }
    
  
  #if 0
  const auto *info = times.Font();
  const int SIZE = 48;
  const uint8 PADDING = 16;
  
  float stb_scale = stbtt_ScaleForPixelHeight(info, SIZE);

  // Bias towards 1.0 because the interior distances tend to be
  // much smaller than exterior. But we want some dynamic range
  // there too.
  const uint8 ONEDGE = 200;
  
  int width, height, xoff, yoff;
  uint8 *bit = stbtt_GetCodepointSDF(info,
				     stb_scale,
				     current_char,
				     // padding
				     PADDING,
				     // onedge value
				     ONEDGE,
				     // falloff per pixel
				     2.0,
				     &width, &height,
				     &xoff, &yoff);
  CHECK(bit != nullptr);

  constexpr int X1 = 10, Y1 = 200;
  constexpr int X2 = 400, Y2 = 200;  

  auto DrawBitmap = [](int startx, int starty, uint8 *bm,
		       int width, int height) {
      int idx = 0;
      for (int y = 0; y < height; y++) {
	int yy = starty + y;
	for (int x = 0; x < width; x++) {
	  uint8 v = bm[idx];
	  int xx = startx + x;
	  sdlutil::drawclippixel(screen, xx, yy, v, v, v);
	  idx++;
	}
      }
    };

  auto DrawBitmapThresh = [](uint8 thresh,
			     int startx, int starty, uint8 *bm,
			     int width, int height) {
      int idx = 0;
      for (int y = 0; y < height; y++) {
	int yy = starty + y;
	for (int x = 0; x < width; x++) {
	  uint8 v = bm[idx];
	  int xx = startx + x;

	  uint8 vv = v >= thresh ? 0xFF : 0x00;
	  sdlutil::drawclippixel(screen, xx, yy, vv, vv, vv);
	  
	  idx++;
	}
      }
    };

  
  DrawBitmap(X1, Y1, bit, width, height);
  DrawBitmapThresh(ONEDGE, X2, Y2, bit, width, height);
#endif
}

void UI::Draw() {

  switch (mode) {
  case Mode::SORTITION:
    DrawSortition();
    break;

  case Mode::LOOPTEST: {

    // ^5 = green = expected
    // ^8 = blue = actual

    auto DrawAssignment = [this](const FontProblem::LoopAssignment &assn) {
	// here we have like
	//        0       1 2
	//        x       y z      <- expected
	//    a b c d e f g h i j  <- actual
	//    0 1 2 3 4 5 6 7 8 9 
	// This would be represented with point0 = 2,
	// and groups = {4, 1, 5}.

	int a = assn.point0;
	for (int e = 0; e < looptest_expected.size(); e++) {
	  int num = assn.groups[e];
	  for (int i = 0; i < num; i++) {
	    FontProblem::Point apt = looptest_actual[a];
	    FontProblem::Point ept = looptest_expected[e];

	    sdlutil::drawclipline(screen,
				  (int)apt.first, (int)apt.second,
				  (int)ept.first, (int)ept.second,
				  0xFF, 0x44, 0x44);
	    
	    a++;
	    if (a == looptest_actual.size()) a = 0;
	  }
	}
      };

    if (looptest_assignment.has_value())
      DrawAssignment(looptest_assignment.value());
    
    auto DrawLoop = [](const vector<FontProblem::Point> &pts,
		       uint8 r, uint8 g, uint8 b) {
	for (int i = 0; i < pts.size(); i++) {
	  const int next_i = i < pts.size() - 1 ? i + 1 : 0;
	  auto [sx, sy] = pts[i];
	  auto [dx, dy] = pts[next_i];
	  sdlutil::drawclipline(
	      screen,
	      sx, sy, dx, dy,
	      r, g, b);

	  sdlutil::drawbox(screen, sx - 2, sy - 2, 5, 5, r, g, b);
	}
      };

    DrawLoop(looptest_expected, 0x00, 0xAA, 0x00);
    DrawLoop(looptest_actual,   0x44, 0xFF, 0x44);
    
    const int LINE1 = SCREENH - 30 - font->height * 2;
    const int LINE2 = LINE1 + font->height;
    const int LINE3 = LINE2 + font->height;

    auto MakePts = [](const vector<FontProblem::Point> &pts) {
	string ret;
	for (const auto &p : pts) {
	  StringAppendF(&ret, "%d,%d  ", (int)p.first, (int)p.second);
	}
	return ret;
      };
    string epts = MakePts(looptest_expected);
    string apts = MakePts(looptest_actual);

    font->draw(12, LINE1, StringPrintf("^5Expected:^< %s", epts.c_str()));
    font->draw(12, LINE2, StringPrintf("^8  Actual:^< %s", apts.c_str()));
    if (looptest_assignment.has_value()) {
      string assn = StringPrintf("Assignment: ^2Point0^< = %d ^3groups^< = ",
				 looptest_assignment->point0);
      for (int n : looptest_assignment->groups)
	StringAppendF(&assn, "%d ", n);
      font->draw(12, LINE3, assn);
    } else {
      font->draw(12, LINE3, "(No assignment)");
    }
    
    break;
  }

  case Mode::SDFTEST:
    DrawSDF();
    break;
    
  case Mode::BITMAP: {

    vector<TTF::Contour> contours = times.GetContours(current_char);
    if (only_bezier) contours = TTF::MakeOnlyBezier(contours);
    if (normalize) contours = TTF::NormalizeOrder(contours,
						  0.0f, 0.0f);
    
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

    auto Point = [&](float x, float y, int n) {
	int xx = XPOS + x * SCALE;
	int yy = YPOS + y * SCALE;
	font->draw(xx, yy, StringPrintf("%d", n));
      };
    
    // One screen pixel in normalized coordinates.
    double sqerr = 1.0f / (SCALE * SCALE);

    for (const auto &contour : contours) {
      float x = contour.startx;
      float y = contour.starty;
      printf("CONTOUR. Start %.5f %.5f\n", x, y);
      Point(x, y, -1);
      for (int i = 0; i < contour.paths.size(); i++) {
	const auto &p = contour.paths[i];
	Point(p.x, p.y, i);
	switch (p.type) {
	case TTF::PathType::LINE: {
	  printf("   %d. LINE %.5f %.5f\n", i, p.x, p.y);
	  Line(x, y, p.x, p.y, 0x000000FF);
	  x = p.x;
	  y = p.y;
	  break;
	}
	case TTF::PathType::BEZIER: {
	  printf("   %d. BEZ  %.5f %.5f   %.5f %.5f\n",
		 i,
		 p.cx, p.cy,
		 p.x, p.y);
	  // Line(x, y, p.cx, p.cy, 0x00FF00FF);
	  // Line(p.cx, p.cy, p.px, p.py, 0x0000FFFF);
	  for (const auto [xx, yy] :
		 TesselateQuadraticBezier<double>(x, y, p.cx, p.cy, p.x, p.y, sqerr)) {
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

  case Mode::SCALETEST: {
    CHECK(cur >= 0 && cur < cur_filenames.size()) << cur;
    const string &ff = cur_filenames[cur];

    string name = FontDB::GetBaseFilename(ff);
    font2x->draw(3, 3, name);
    printf("%s\n", name.c_str());
    fflush(stdout);

    font2x->draw(3, font2x->height + 3,
		 StringPrintf("scale ^5%.2f^1x^5%.2f  ^0off ^6%.2f %.2f",
			      current_xscale, current_yscale,
			      current_xoff, current_yoff));
    
    TTF *ttf = GetFont(cur);
    CHECK(ttf != nullptr);

    [[maybe_unused]]
    double diff =
      BitmapDifference(*ttf,
		       'A', 'a',
		       // XXX need to figure this parameter out
		       // scale,
		       200,
		       current_xscale, current_yscale,
		       current_xoff, current_yoff,
		       true);
    
    break;
  }
  }
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

  font2x = Font::CreateX(2,
			 screen,
			 "font.png",
			 FONTCHARS,
			 FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font2x != nullptr) << "Couldn't load font2x";

  UI ui;
  ui.Loop();

  SDL_Quit();
  return 0;
}

