
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
  SCALETEST,
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

  // Default, unknown state.
  UNKNOWN,
};

// Each flag can be true, false, unknown.
enum class Flag {
  SAME_CASE,
};

// Returns the true state, false state.
// All chars should be distinct!
const pair<char, char> FlagChar(Flag f) {
  switch (f) {
  case Flag::SAME_CASE:
    return {'C', 'c'};
  default:
    LOG(FATAL) << "Bad flag?";
    return {'X', 'x'};
  }
}

const pair<Flag, bool> CharFlag(char c) {
  switch (c) {
  default:
    LOG(FATAL) << "Bad flag char";
    // Fallthrough to suppress warnings.
  case 'C': return {Flag::SAME_CASE, true};
  case 'c': return {Flag::SAME_CASE, false};
  }
}

// XXX delete and use a single-file representation.
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
    // XXX unknown is not a real old-style fontset
  default:
    LOG(FATAL) << "Bad Type?";
    return "";
  }
}

// Could include colors?
const char *TypeString(Type t) {
  switch (t) {
  case Type::SANS: return "sans";
  case Type::SERIF: return "serif";
  case Type::FANCY: return "fancy";
  case Type::TECHNO: return "techno";
  case Type::DECORATIVE: return "decorative";
  case Type::MESSY: return "messy";
  case Type::DINGBATS: return "dingbats";
  case Type::OTHER: return "other";
  case Type::BROKEN: return "broken";
  case Type::UNKNOWN: return "unknown";
  default:
    LOG(FATAL) << "Bad Type?";
    return "";
  }
}

// Keeps track of properties of fonts.
// Fonts have at most one type.
// Separately, we can record flags.
struct FontDB {
  static constexpr const char *DATABASE_FILENAME = "font-db.txt";

  struct Info {
    Type t = Type::UNKNOWN;
    std::map<Flag, bool> flags;
    // In [0,1]. Negative means unknown/incalculable.
    float bitmap_diffs = -1.0f;
  };

  FontDB() {
    std::unordered_map<string, Type> string_type;
    for (const Type t : {Type::SANS, Type::SERIF, Type::FANCY,
	  Type::TECHNO, Type::DECORATIVE,
	  Type::MESSY, Type::DINGBATS, Type::OTHER, Type::BROKEN,
	  Type::UNKNOWN}) {
      string_type[TypeString(t)] = t;
    }
    
    for (string line : Util::ReadFileToLines(DATABASE_FILENAME)) {
      const string flagstring = Util::chop(line);
      const double diffscore = Util::ParseDouble(Util::chop(line), -1.0);
      const string typestring = Util::chop(line);
      const string filename = Util::losewhitel(line);

      auto it = string_type.find(typestring);
      CHECK(it != string_type.end()) << "Unknown type " << typestring;
      const Type type = it->second;
      CHECK(files.find(filename) == files.end()) <<
	"Duplicate in fontdb: " << filename;

      Info info;
      info.t = type;
      if (type != Type::UNKNOWN) num_sorted++;
      info.bitmap_diffs = diffscore;
      for (char c : flagstring) {
	if (c == '_') continue;
	auto [flag, on] = CharFlag(c);
	info.flags[flag] = on;
      }
      files[filename] = info;
    }
    
    printf("Total in FontDB: %lld\n", files.size());
  }

  bool Dirty() const {
    return dirty;
  }

  // XXX can probably assume success, fail if not
  std::optional<Info> Lookup(const string &s) const {
    auto it = files.find(s);
    if (it == files.end()) return {};
    else return {it->second};
  }

  void SetBitmapDiffs(const string &s, float bitmap_diffs = -1.0f) {
    files[s].bitmap_diffs = bitmap_diffs;
    dirty = true;
  }
  
  void AssignType(const string &s, Type t) {
    if (files[s].t != Type::UNKNOWN) num_sorted--;
    files[s].t = t;
    if (files[s].t != Type::UNKNOWN) num_sorted++;
    dirty = true;
  }

  void SetFlag(const string &s, Flag flag, bool on) {
    files[s].flags[flag] = on;
    dirty = true;
  }

  int64 NumSorted() const {
    return num_sorted;
  }
  
  void Save() {
    {
      vector<string> lines;
      // XXX sort by filename
      for (const auto &[filename, info] : files) {
	lines.push_back(StringPrintf("%s %.5f %s %s",
				     FlagString(info.flags).c_str(),
				     info.bitmap_diffs,
				     TypeString(info.t),
				     filename.c_str()));
      }
      Util::WriteLinesToFile(DATABASE_FILENAME, lines);
      printf("Wrote %lld entries to %s\n",
	     (int64)lines.size(),
	     DATABASE_FILENAME);
    }
      
    {
      // Temporary? P/R curve export
      struct Labeled {
	// "0" = more likely to be same case, 1 = least likely.
	float score = 0.0;
	bool same_case = false;
	Labeled(float score, bool same_case) :
	  score(score), same_case(same_case) {}
      };

      vector<Labeled> labs;
      for (const auto &[filename, info] : files) {
	if (info.bitmap_diffs >= 0.0 && info.bitmap_diffs <= 1.0) {
	  auto it = info.flags.find(Flag::SAME_CASE);
	  if (it != info.flags.end()) {
	    labs.emplace_back(info.bitmap_diffs, it->second);
	  }
	}
      }

      std::sort(labs.begin(), labs.end(),
		[](const Labeled &a, const Labeled &b) {
		  return a.score < b.score;
		});
      
      // "Positive" here means same case (this is a low score).
      //
      // As we go, assuming the threshold is set to the current value, what would
      // our P/R be? This means calling everything we've already seen a positive
      // and everything else a negative. So first, a parallel array giving the
      // number of true positives for the rest of the array (strictly higher scores).
      vector<int64> remaining_positives(labs.size(), 0);
      int64 total_positives = 0;
      {
	int64 pos_above = 0;
	for (int64 i = labs.size() - 1; i >= 0; i--) {
	  remaining_positives[i] = pos_above;
	  if (labs[i].same_case) {
	    total_positives++;
	    pos_above++;
	  }
	}
      }

      // Now compute precision at each threshold. Every item up to the threshold
      // being considered is predicted positive.
      int64 positives_so_far = 0;
      std::vector<string> lines;
      lines.reserve(labs.size() + 1);
      lines.push_back("threshold\t"
		      "recall\t"
		      "precision");
      for (int64 i = 0; i < labs.size(); i++) {
	if (labs[i].same_case) positives_so_far++;
	double precision = (double)positives_so_far / (i + 1);
	lines.push_back(StringPrintf("%.5f\t%.5f\t%.5f",
				     labs[i].score,
				     (total_positives - remaining_positives[i]) /
				     (double)total_positives,
				     precision));
      }
      Util::WriteLinesToFile("pr-curve.tsv", lines);
    }

    dirty = false;
  }

  int64 Size() const {
    return files.size();
  }

  const std::unordered_map<string, Info> &Files() const {
    return files;
  }
  
  
 private:
  static string FlagString(const std::map<Flag, bool> &flags) {
    if (flags.empty()) return "_";
    string ret;
    for (const auto [flag, val] : flags) {
      const auto [tc, fc] = FlagChar(flag);
      ret.push_back(val ? tc : fc);
    }
    return ret;
  }

  int64 num_sorted = 0;
  std::unordered_map<string, Info> files;
  bool dirty = false;
};

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

  int DrawChar(TTF *ttf, int sx, int sy, float scale, char c, char nc);
  int DrawString(TTF *ttf, int sx, int sy, float scale, const string &str);
  int DrawAlphabet(TTF *ttf, int sx, int sy, float scale);

  // Drawing for various modes
  void DrawSortition();
  
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

  // XXX
  char current_char = 'a';
  float current_xscale = 1.0, current_yscale = 1.0;
  float current_xoff = 0.0, current_yoff = 0.0;
  TTF times{"times.ttf"};
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
    if (info.t == Type::UNKNOWN) {
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
	info.t != Type::BROKEN) {
      cur_filenames.push_back(filename);
    }
  }
  
  // Optional. Might be good to keep fonts in the same family
  // together, really..
  Shuffle(&rc, &cur_filenames);

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
	    mode = Mode::SCALETEST;
	    break;
	  case Mode::SCALETEST:
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
	  case Mode::SORTITION:
	    cur += dy;
	    if (cur < 0) cur = 0;
	    if (cur >= cur_filenames.size())
	      cur = cur_filenames.size() - 1;
	    ui_dirty = true;
	    break;
	  case Mode::BITMAP:
	    current_char += dx;
	    if (current_char > 'Z') current_char = 'Z';
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
	if (info.value().t != Type::UNKNOWN) {
	  dest = TypeString(info.value().t);
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

      
      
      int slash = ff.rfind("\\");
      string name = (slash == string::npos) ? ff : ff.substr(slash + 1, string::npos);
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
  
void UI::Draw() {

  switch (mode) {
  case Mode::SORTITION:
    DrawSortition();
    break;

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

  case Mode::SCALETEST: {
    CHECK(cur >= 0 && cur < cur_filenames.size()) << cur;
    const string &ff = cur_filenames[cur];
    
    int slash = ff.rfind("\\");
    string name = (slash == string::npos) ? ff : ff.substr(slash + 1, string::npos);
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

