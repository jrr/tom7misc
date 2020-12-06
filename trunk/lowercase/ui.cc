
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <unordered_set>
#include <unordered_map>
#include <utility>

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

#include "ttf.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr, *font2x = nullptr;
#define SCREENW 1920
#define SCREENH 1280
static SDL_Surface *screen = nullptr;

enum class Mode {
  BITMAP,
  SORTITION,
};


namespace {

enum class Type {
  // good clean fonts. These should have "normal-looking"
  // letters with good construction, no effects like outlines etc.
  // different weights and obliques/italics are ok.
  SANS,
  SERIF,
  // Including cursive, blackletter, caligraphic
  FANCY,
  
  // Cyber-fonts, pixel outlines, etc.
  TECHNO,

  // Clean fonts that are not "fancy" or "techno" but have some other
  // decorative style that makes the letter shapes not be normal (e.g.
  // "old west" font).
  DECORATIVE,
  
  // Scans or distressed fonts with lots of control points. Might work
  // if rendered to bitmaps.
  MESSY,

  // Anything that's not actually letters.
  DINGBATS,
  
  // Font is working but doesn't fit above categories.
  // Anything with effects like outlines goes in here.
  OTHER,

  // Something is afoot, e.g. metrics look wrong, missing lowercase,
  // A-Z is some other language, something weird with the rendering, etc.
  BROKEN,
};

const char *TypeFilename(Type s) {
  switch (s) {
  case Type::SANS: return "sans.txt";
  case Type::SERIF: return "serif.txt";
  case Type::FANCY: return "fancy.txt";
  case Type::TECHNO: return "techno.txt";
  case Type::DECORATIVE: return "decorative.txt";
  case Type::MESSY: return "messy.txt";
  case Type::DINGBATS: return "dingbats.txt";
  case Type::OTHER: return "other.txt";
  case Type::BROKEN: return "broken.txt";
  default:
    LOG(FATAL) << "Bad Type?";
    return "";
  }
};

// Could include colors?
const char *TypeString(Type s) {
  switch (s) {
  case Type::SANS: return "sans";
  case Type::SERIF: return "serif";
  case Type::FANCY: return "fancy";
  case Type::TECHNO: return "techno";
  case Type::DECORATIVE: return "decorative";
  case Type::MESSY: return "messy";
  case Type::DINGBATS: return "dingbats";
  case Type::OTHER: return "other";
  case Type::BROKEN: return "broken";
  default:
    LOG(FATAL) << "Bad Type?";
    return "";
  }
};

struct FontSets {
  FontSets() {
    int64 total = 0;
    for (const Type t : {Type::SANS, Type::SERIF, Type::FANCY,
	  Type::TECHNO, Type::DECORATIVE,
	  Type::MESSY, Type::DINGBATS, Type::OTHER, Type::BROKEN}) {
      string set_filename = TypeFilename(t);
      int64 count = 0;
      for (const string &f : Util::ReadFileToLines(set_filename)) {
	CHECK(files.find(f) == files.end()) << "Duplicate in fontsets: " << f;
	files[f] = t;
	count++;
      }
      printf("%s: %lld\n", set_filename.c_str(), count);
      total += count;
    }
    printf("Total in FontSet: %lld\n", total);
  }

  bool Dirty() const {
    return dirty;
  }
  
  std::optional<Type> Lookup(const string &s) const {
    auto it = files.find(s);
    if (it == files.end()) return {};
    else return {it->second};
  }

  void Assign(const string &s, Type t) {
    files[s] = t;
    // (maybe don't set if this was already the case?)
    dirty = true;
  }
  
  void Save() {
    // (Note: If a set becomes totally empty, we won't save over it,
    // because it won't appear in this map...)
    std::unordered_map<Type, vector<string>> collated;
    for (const auto &[f, t] : files)
      collated[t].push_back(f);

    // sort the collated vectors
    for (auto it = collated.begin(); it != collated.end(); ++it) {
      std::sort(it->second.begin(), it->second.end());
    }

    for (const auto &[t, v] : collated) {
      string sf = TypeFilename(t);
      Util::WriteLinesToFile(sf, v);
      printf("Wrote %lld entries to %s\n", (int64)v.size(), sf.c_str());
    }

    dirty = false;
  }

  int64 Size() const {
    return files.size();
  }
  
  // Filename gets assigned to at most one fontset.
  std::unordered_map<string, Type> files;
  bool dirty = false;
};

static constexpr int WINDOW = 10;

struct UI {
  Mode mode = Mode::SORTITION;
  bool ui_dirty = true;
  // quit confirmations.
  int confirmations = 0;
  
  FontSets fontsets;
  
  // Fonts not in any above set (upon loading this
  // instance).
  vector<string> unsorted_filenames;

  UI();
  // set font at cur to the given type
  void SetType(Type t);
  void Loop();
  void Draw();

  int DrawChar(TTF *ttf, int sx, int sy, float scale, char c, char nc);
  int DrawString(TTF *ttf, int sx, int sy, float scale, const string &str);
  int DrawAlphabet(TTF *ttf, int sx, int sy, float scale);
  
  // current index (into unsorted_filenames, fonts, etc.) that we act
  // on with keypresses etc.
  int cur = 0;
  
  // Parallel to unsorted_filenames, but not usually
  // as long. Use GetFont(idx).
  vector<TTF *> fonts;

  TTF *GetFont(int idx) {
    CHECK(idx < unsorted_filenames.size());
    while (idx >= fonts.size())
      fonts.push_back(nullptr);

    if (fonts[idx] == nullptr) {
      printf("Load %s...\n", unsorted_filenames[idx].c_str());
      fflush(stdout);
      fonts[idx] = new TTF(unsorted_filenames[idx]);
    }

    return fonts[idx];
  }

  int current_scale = 100;
  bool draw_points = true;

  int64 num_all_fonts = 0;

  ArcFour rc{"lowercase"};
  
  // XXX
  char current_char = 'a';
  TTF times{"times.ttf"};
};
}  // namespace

UI::UI() {
  int64 sorted = 0;
  // Done: random, (di)stressed, laser, helvetica, antique
  // "sans serif", serif, 3D, outline, handwriting, hand
  // shadow, arial, mirror, flipped, hollow, bats, icon
  // typewriter, geometric, times, bodoni, futura, basker,
  // courier, gothic, stencil, comic, ransom, modern, book
  // ITC, google, consol, deco, sans, frutiger, univer, cent,
  // andale
  
  RE2 required = ".*[Mm]ono.*";

  // This file generated by cleandb.exe
  for (const string &file : Util::ReadFileToLines("all_fonts.txt")) {
    num_all_fonts++;
    // ad hoc filtering for one-off tasks (e.g. quickly check
    // all fonts with "handwriting" in the name)

    if (!RE2::FullMatch(file, required))
      continue;
    
    if (fontsets.Lookup(file).has_value()) {
      sorted++;
    } else {
      unsorted_filenames.push_back(file);
    }
  }

  // Optional. Might be good to keep fonts in the same family
  // together, really..
  // Shuffle(&rc, &unsorted_filenames);
    
  printf("All fonts: %d\n"
         "Sorted fonts: %d\n"
         "Unsorted fonts for this session: %lld\n",
	 num_all_fonts,
	 sorted,
	 (int64)unsorted_filenames.size());
}

void UI::SetType(Type t) {
  fontsets.Assign(unsorted_filenames[cur], t);
  if (cur < unsorted_filenames.size() - 1) {
    cur++;
  }
  ui_dirty = true;
}

void UI::Loop() {
  Mode mode = Mode::SORTITION;
  
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

	// Need to press this consecutively in order
	// to exit without saving.
	if (event.key.keysym.sym != SDLK_ESCAPE)
	  confirmations = 0;
	
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  printf("ESCAPE.\n");
	  if (fontsets.Dirty()) {
	    ui_dirty = true;
	    confirmations++;
	    if (confirmations >= 5)
	      return;
	  } else {
	    return;
	  }
	  break;
	case SDLK_LEFT:
	  if (current_char > ' ') current_char--;
	  ui_dirty = true;
	  break;
	case SDLK_RIGHT:
	  if (current_char < '~') current_char++;
	  ui_dirty = true;
	  break;

	  // Sortition mode...

	case SDLK_PERIOD:
	  draw_points = !draw_points;
	  ui_dirty = true;
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
	  
	case SDLK_DOWN:
	  cur++;
	  if (cur >= unsorted_filenames.size())
	    cur = unsorted_filenames.size() - 1;
	  ui_dirty = true;
	  break;

	case SDLK_UP:
	  cur--;
	  if (cur < 0) cur = 0;
	  ui_dirty = true;
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
	  fontsets.Save();
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


void UI::Draw() {
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

  if (fontsets.Dirty()) {
    font2x->draw(SCREENW - font2x->width * 8, 4, "(Sa[^6V^<]e?)");
  }
  if (confirmations > 0) {
    font2x->draw(SCREENW - font2x->width * 10, 32, StringPrintf("^2CONFIRM ^5%d",
								confirmations));
  }

  {
    double pct = (100.0 * fontsets.Size()) / (double)num_all_fonts;
    string progress = StringPrintf("^6%.2f^4%%^<  %d^4/^<%d",
				   pct, cur, unsorted_filenames.size());
    font2x->draw(SCREENW - font2x->sizex(progress) - 8, SCREENH - font2x->height - 2,
		 progress);
  }
  
  switch (mode) {
  case Mode::SORTITION: {

    int xpos = 64;
    int ypos = 64;
    constexpr int HISTORY = 2;
    for (int i = cur - HISTORY; i < cur + WINDOW - HISTORY; i++) {
      // Can be out of bounds, especially at the beginning...
      if (i >= 0 && i < unsorted_filenames.size()) {

	float scalescale = 1.0f;
	if (i < cur) scalescale = 0.60f;
	if (i == cur) scalescale = 1.2f;

	float scale = current_scale * scalescale;
	
	const string &ff = unsorted_filenames[i];
	std::optional<Type> typ = fontsets.Lookup(ff);

	string dest = "";
	if (typ.has_value()) {
	  dest = TypeString(typ.value());
	}
	
	int slash = ff.rfind("\\");
	string name = (slash == string::npos) ? ff : ff.substr(slash + 1, string::npos);
	font->draw(xpos, ypos, name);
	ypos += font->height + 1;

	if (i == cur) {
	  font2x->draw(xpos - 52, ypos, "^6>^7>^8>");
	}
	
	font2x->draw(4, ypos + font2x->height, dest);
	
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
      
    break;
  }    
  case Mode::BITMAP: {

    vector<TTF::Contour> contours = times.GetContours(current_char);

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
	  // printf("  bezier %d,%d (%d, %d)\n", p.x, p.y, p.cx, p.cy);
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

