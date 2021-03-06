
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
#include "../cc-lib/sdl/cursor.h"
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
#include "network.h"

#include "bezier.h"

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

static SDL_Cursor *cursor_arrow = nullptr, *cursor_bucket = nullptr;
static SDL_Cursor *cursor_eraser = nullptr;

enum class Mode {
  BITMAP,
  SORTITION,
  SDFTEST,
  SCALETEST,
  LOOPTEST,
  BEZDIST,
  DRAW,
};

enum class DrawMode {
  ERASING,
  DRAWING,
  FILLING,
};

namespace {
using Type = FontDB::Type;
using Flag = FontDB::Flag;

static constexpr int WINDOW = 10;

struct GammaSlider {
  static constexpr float MIN = 0.05f;
  static constexpr float MAX = 1.95f;

  // screen coordinates
  GammaSlider(int x, int y,
              int width, int height) :
    x(x), y(y), width(width), height(height) {
  }

  // screen coordinates
  // returns true if dirty
  bool MouseDown(int mousex, int mousey) {
    if (mousex >= x && mousey >= y &&
        mousex < x + width && mousey < y + height) {
      dragging = true;
      return MouseMove(mousex, mousey);
    }
    return false;
  }

  void MouseUp() {
    dragging = false;
  }

  bool MouseMove(int mousex, int mousey) {
    if (dragging) {
      // XXX could use y for fine control?
      float f = std::clamp((mousex - x) / (float)width, 0.0f, 1.0f);
      value = f * (MAX - MIN) + MIN;
      return true;
    }
    return false;
  }

  float Value() const { return value; }

  void Draw() const {
    sdlutil::drawbox(screen, x, y, width, height,
                     0x55, 0x55, 0xCC);
    float f = (value - MIN) / (MAX - MIN);
    int pos = std::round(x + f * width);
    sdlutil::FillRectRGB(screen, pos - 2, y + 1, 4, height - 2,
                         0x77, 0xCC, 0xFF);
    // Maybe make this disappear after a while?
    font->draw(pos - 4, y + height + 2,
               StringPrintf("^1%.2f", value));
  }

private:
  bool dragging = false;

  int x = 0, y = 0;
  int width = 0;
  int height = 0;
  float value = 1.0;
};

struct UI {
  Mode mode = Mode::BEZDIST;
  DrawMode draw_mode = DrawMode::DRAWING;

  std::shared_mutex dirty_m;
  bool ui_dirty = true;
  void SetDirty(bool dirty = true) {
    WriteMutexLock ml(&dirty_m);
    ui_dirty = dirty;
  }
  bool IsDirty() {
    ReadMutexLock ml(&dirty_m);
    return ui_dirty;
  }

  // quit confirmations.
  int confirmations = 0;

  FontDB fontdb;

  // All the fonts we want to look at in this instance.
  vector<string> cur_filenames;

  UI();
  // set font at cur to the given type
  void SetType(Type t);
  void SetFlag(Flag f, bool on);
  void Run();
  void Loop();
  void Draw();

  void RecomputeLoop();

  int DrawChar(TTF *ttf, int sx, int sy, float scale, char c, char nc);
  int DrawString(TTF *ttf, int sx, int sy, float scale, const string &str);
  int DrawAlphabet(TTF *ttf, int sx, int sy, float scale);

  // Drawing for various modes
  void DrawSortition();
  void DrawSDF();
  void DrawDrawing();
  void DrawBez();

  void ClearDrawing();

  // Draw line into drawing. Coordinates are relative to drawing, but may
  // be outside it.
  void DrawThick(int x0, int y0, int x1, int y1, Uint32 color);
  void FloodFill(int x, int y, Uint32 color);
  void UpdateGamma();

  // current index (into cur_filenames, fonts, etc.) that we act
  // on with keypresses etc.
  int cur = 0;

  // Parallel to cur_filenames, but not usually
  // as long. Use GetFont(idx).
  vector<TTF *> fonts;

  void ResultThread();

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

  int bez_startx = 800, bez_starty = 128;
  int bez_cx = SCREENW - 32, bez_cy = SCREENH / 2;
  int bez_endx = 10, bez_endy = 188;
  int bez_px = 100, bez_py = 100;
  // double bez_time = 0.0;
  // int bez_cpx = 0, bez_cpy = 0;

  vector<FontProblem::Point> looptest_expected;
  vector<FontProblem::Point> looptest_actual;
  std::optional<FontProblem::LoopAssignment> looptest_assignment;

  static constexpr FontProblem::SDFConfig SDF_CONFIG{};
  std::unique_ptr<const Network> make_lowercase;
  std::unique_ptr<const Network> make_uppercase;
  static constexpr int DRAWING_X = 591;
  static constexpr int DRAWING_Y = 32;
  static constexpr int DRAWING_SIZE = 800;

  static constexpr int SDF_OUT_SCALE = 5;
  static constexpr int SDF_OUT_H = SDF_CONFIG.sdf_size * SDF_OUT_SCALE;
  static constexpr int SDF_OUT_W = SDF_OUT_H * 2;
  // gap between the lower and lower-upper images
  static constexpr int SDF_OUT_GAP = 32;

  static constexpr int LOW_X = 132;
  static constexpr int LOW_Y = 302;
  static constexpr int UP_X = 1478;
  static constexpr int UP_Y = 559;
  GammaSlider gamma_low{LOW_X,
                        LOW_Y + SDF_OUT_H * 2 + SDF_OUT_GAP + 4,
                        SDF_OUT_W, 12};
  GammaSlider gamma_up{UP_X, UP_Y + SDF_OUT_H + 4, SDF_OUT_W, 12};

  // Protects these.
  std::mutex result_m;
  std::condition_variable result_cv;
  SDL_Surface *drawing = nullptr;
  std::optional<FontProblem::Gen5ImagesResult> network_image_result;
  // Recompute everything.
  bool result_sdf_dirty = true;
  // Can just recompute the images (e.g. for gamma changes).
  bool result_images_dirty = true;
  bool result_should_die = false;
};
}  // namespace

void UI::ResultThread() {
  // Only this thread needs to access these temporaries.
  ImageA bitmap(DRAWING_SIZE, DRAWING_SIZE);
  FontProblem::Gen5Result gen5result;
  for (;;) {
    double copy_ms = 0.0, result_ms = 0.0, images_ms = 0.0;
    bool recompute_sdf = false;
    float gamma_low_value = 1.0f, gamma_up_value = 1.0f;
    {
      std::unique_lock<std::mutex> guard(result_m);
      result_cv.wait(guard, [this]() {
          return result_sdf_dirty || result_images_dirty;
        });
      // Lock held. First see if we should exit.
      if (result_should_die) return;

      gamma_low_value = gamma_low.Value();
      gamma_up_value = gamma_up.Value();

      // Now, do we have to re-run everything?
      if (result_sdf_dirty) {
        Timer copy_timer;

        // Copy bitmap from drawing so we can work
        // without interference.
        for (int y = 0; y < DRAWING_SIZE; y++) {
          for (int x = 0; x < DRAWING_SIZE; x++) {
            // PERF: the surface getpixel routine is awful,
            // and not inlineable. Can we assert a specific
            // pixel format when we create it?
            uint32 px = 0xFF00 & sdlutil::getpixel(drawing, x, y);
            // PERF and if we're not copying, we could just
            // generate the 1bpp image here?
            bitmap.SetPixel(x, y, px > 0x7F00 ? 255 : 0);
          }
        }
        copy_ms = copy_timer.MS();

        recompute_sdf = true;
      }

      // Either way, we'll un-dirty the state. However,
      // once we release the lock the inputs might be updated
      // before we finish. So clear the dirty state now.
      result_sdf_dirty = false;
      result_images_dirty = false;
    }

    // Do work. Lock not held.
    if (recompute_sdf) {
      Timer result_timer;
      ImageA sdf = FontProblem::SDFFromBitmap(SDF_CONFIG, bitmap);
      gen5result =
        FontProblem::Gen5(SDF_CONFIG, *make_lowercase, *make_uppercase, sdf);
      result_ms = result_timer.MS();
    }

    // Always recompute images if we got here.
    Timer image_timer;
    constexpr int SCALE = 5;
    constexpr int QUALITY = 3;
    FontProblem::Gen5ImagesResult result =
      FontProblem::Gen5Images(gen5result, SCALE, QUALITY,
                              gamma_low_value, gamma_up_value);
    images_ms = image_timer.MS();

    // Now write with lock.
    {
      std::lock_guard<std::mutex> guard(result_m);
      if (result_should_die) return;
      network_image_result.emplace(std::move(result));
    }

    // But now the UI is dirty.
    SetDirty();

    printf("Got result in %.4fs copy + %.4fs result + %.4fs image\n",
           copy_ms / 1000.0, result_ms / 1000.0, images_ms / 1000.0);
  }
}


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

  const stbtt_fontinfo *info = ttf.FontInfo();
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

  if (draw) {
    printf ("scale %.5f %.5f\n",
            stb_scale * xscale2, stb_scale * yscale2);
  }

  int width2, height2, xoff2, yoff2;
  uint8 *bit2 =
    stbtt_GetCodepointBitmapSubpixel(info,
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

    font->draw(X1, Y1 - font->height,
               StringPrintf("^3%d^1x^3%d", width1, height1));
    font->draw(X2, Y2 - font->height,
               StringPrintf("^3%d^1x^3%d", width2, height2));

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

  // Here we're working in a coordinate space where 0,0 is the origin
  // for both characters, and the scale is in pixels of the rendered
  // bitmaps. The top left of the bitmap (and the bounding box
  // containing both bitmaps) is typically negative:
  const int minx = std::min(xoff1, int_x + xoff2);
  const int miny = std::min(yoff1, int_y + yoff2);
  // (one past the right, bottom)
  const int maxx = std::max(width1 + xoff1,  width2 + int_x + xoff2);
  const int maxy = std::max(height1 + yoff1, height2 + int_y + yoff2);

  // Width and height of this bounding box.
  const int bbw = maxx - minx, bbh = maxy - miny;

  const int BX = 100, BY = 500;

  auto GetPx = [](const uint8 *bm,
                  int width, int height, int x, int y) -> uint8 {
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

  RE2 required = ".*";

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

  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));
  printf("Loaded networks.\n");

  drawing = sdlutil::makesurface(DRAWING_SIZE, DRAWING_SIZE, true);
  ClearDrawing();
  CHECK(drawing != nullptr);
}

void UI::ClearDrawing() {
  sdlutil::ClearSurface(drawing, 0, 0, 0, 0xFF);
  {
    std::unique_lock<std::mutex> guard(result_m);
    result_sdf_dirty = true;
  }
  result_cv.notify_all();
}

void UI::SetType(Type t) {
  if (mode == Mode::SORTITION) {
    fontdb.AssignType(cur_filenames[cur], t);
    if (cur < cur_filenames.size() - 1) {
      cur++;
    }
    SetDirty();
  }
}

void UI::SetFlag(Flag f, bool on) {
  if (mode == Mode::SORTITION) {
    fontdb.SetFlag(cur_filenames[cur], f, on);
    if (cur < cur_filenames.size() - 1) {
      cur++;
    }
    SetDirty();
  }
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

void UI::DrawThick(int x0, int y0,
                   int x1, int y1,
                   Uint32 color) {
  static constexpr int THICKNESS = 6;
  Line<int> l{x0, y0, x1, y1};

  {
    std::unique_lock<std::mutex> guard(result_m);
    const int w = drawing->w, h = drawing->h;

    Uint32 *bufp = (Uint32 *)drawing->pixels;
    int stride = drawing->pitch >> 2;
    auto SetPixel = [color, w, h, bufp, stride](int x, int y) {
        if (x >= 0 && y >= 0 &&
            x < w && y < h) {
          bufp[y * stride + x] = color;
        }
      };

    auto ThickPixel = [&SetPixel](int x, int y) {
        static constexpr int LO = THICKNESS >> 1;
        // static constexpr int RO = THICKNESS - LO;

        for (int xx = x - LO; xx < x - LO + THICKNESS; xx++) {
          for (int yy = y - LO; yy < y - LO + THICKNESS; yy++) {
            SetPixel(xx, yy);
          }
        }
      };

    ThickPixel(x0, y0);

    for (const auto [x, y] : Line<int>{x0, y0, x1, y1}) {
      ThickPixel(x, y);
    }

    result_sdf_dirty = true;
  }
  result_cv.notify_all();
}

void UI::UpdateGamma() {
  {
    std::unique_lock<std::mutex> guard(result_m);
    result_images_dirty = true;
  }
  result_cv.notify_all();
}

void UI::FloodFill(int x, int y, Uint32 color) {
  {
    std::unique_lock<std::mutex> guard(result_m);

    const int w = drawing->w, h = drawing->h;
    CHECK(w == DRAWING_SIZE && h == DRAWING_SIZE);
    if (x < 0 || y < 0 || x >= w || y >= h)
      return;

    Uint32 *bufp = (Uint32 *)drawing->pixels;
    int stride = drawing->pitch >> 2;

    const Uint32 replace_color = bufp[y * stride + x];

    auto GetPixel =
      [w, h, bufp, stride, replace_color](int x, int y) -> uint32 {
        if (x >= 0 && y >= 0 &&
            x < w && y < h) {
          return bufp[y * stride + x];
        } else {
          return ~replace_color;
        }
      };

    auto SetPixel = [color, w, h, bufp, stride](int x, int y) {
        if (x >= 0 && y >= 0 &&
            x < w && y < h) {
          bufp[y * stride + x] = color;
        }
      };

    std::vector<std::pair<int, int>> todo;
    if (color != replace_color)
      todo.emplace_back(x, y);

    while (!todo.empty()) {
      const auto [xx, yy] = todo.back();
      todo.pop_back();

      Uint32 c = GetPixel(xx, yy);
      if (c == replace_color) {
        SetPixel(xx, yy);
        todo.emplace_back(xx - 1, yy);
        todo.emplace_back(xx + 1, yy);
        todo.emplace_back(xx, yy - 1);
        todo.emplace_back(xx, yy + 1);
      }
    }
    result_sdf_dirty = true;
  }
  result_cv.notify_all();
}

// Returns (x, y, dist).
// PERF! There are definitely analytical solutions to this, and also
// much faster ways to optimize (root of a third-degree polynomial).
static std::tuple<float, float, float>
DistanceFromPointToBezierSearch(
    // The point to test
    float px, float py,
    // Bezier start point
    float sx, float sy,
    // Bezier ontrol point
    float cx, float cy,
    // Bezier end point
    float ex, float ey) {

  auto Bezier = [sx, sy, cx, cy, ex, ey](float t) ->
    std::pair<float, float> {
    float tt = t * t;
    // Get point on curve at t:
    float omt = 1.0 - t;
    float omtt = omt * omt;
    float bx = omtt * sx  +  2.0 * omt * t * cx  +  tt * ex;
    float by = omtt * sy  +  2.0 * omt * t * cy  +  tt * ey;
    return make_pair(bx, by);
  };

   auto [t, sqdist] =
    Opt::Minimize1D([&Bezier, px, py](double t) {
        const auto [bx, by] = Bezier(t);
        const float dx = bx - px;
        const float dy = by - py;
        return (dx * dx) + (dy * dy);
    }, 0.0, 1.0, 100);

  const auto [bx, by] = Bezier(t);
  return make_tuple(bx, by, sqrtf(sqdist));
}



void UI::Run() {
  std::thread result_thread([this]() { ResultThread(); });

  // Main thread.
  Loop();

  {
    std::unique_lock<std::mutex> guard(result_m);
    result_sdf_dirty = true;
    result_should_die = true;
  }
  result_cv.notify_all();
  result_thread.join();
}

void UI::Loop() {

  int mousex = 0, mousey = 0;
  bool dragging = false;
  int dragging_button = 0;
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

        const int oldx = mousex, oldy = mousey;

        mousex = e->x;
        mousey = e->y;


        if (mode == Mode::DRAW) {
          if (gamma_low.MouseMove(mousex, mousey) ||
              gamma_up.MouseMove(mousex, mousey)) {
            UpdateGamma();
            SetDirty();
          } else {
            if (dragging) {
              switch (draw_mode) {
              case DrawMode::DRAWING:
                DrawThick(oldx - DRAWING_X, oldy - DRAWING_Y,
                          mousex - DRAWING_X, mousey - DRAWING_Y,
                          0xFFFFFFFF);
                break;
              case DrawMode::ERASING:
                DrawThick(oldx - DRAWING_X, oldy - DRAWING_Y,
                          mousex - DRAWING_X, mousey - DRAWING_Y,
                          0xFF000000);
                break;
              default:
                break;
              }
              SetDirty();
            }
          }
        } else if (mode == Mode::BEZDIST) {
          if (dragging_button > 0) {
            switch (dragging_button) {
            case SDL_BUTTON_LEFT:
              bez_startx = e->x;
              bez_starty = e->y;
              break;
            case SDL_BUTTON_RIGHT:
              bez_endx = e->x;
              bez_endy = e->y;
              break;
            case SDL_BUTTON_MIDDLE:
              bez_cx = e->x;
              bez_cy = e->y;
              break;
            }
            SetDirty();
          } else {
            bez_px = e->x;
            bez_py = e->y;
            SetDirty();
          }
        }

        break;
      }

      case SDL_MOUSEBUTTONDOWN: {
        // LMB/RMB, drag, etc.
        SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;
        mousex = e->x;
        mousey = e->y;
        dragging_button = e->button;

        if (mode == Mode::DRAW) {

          if (gamma_low.MouseDown(mousex, mousey) ||
              gamma_up.MouseDown(mousex, mousey)) {
            UpdateGamma();
            SetDirty();
          } else {
            dragging = true;

            switch (draw_mode) {
            case DrawMode::DRAWING:
              // Make sure that a click also makes a pixel.
              DrawThick(mousex - DRAWING_X, mousey - DRAWING_Y,
                        mousex - DRAWING_X, mousey - DRAWING_Y,
                        0xFFFFFFFF);
              break;
            case DrawMode::ERASING:
              DrawThick(mousex - DRAWING_X, mousey - DRAWING_Y,
                        mousex - DRAWING_X, mousey - DRAWING_Y,
                        0xFF000000);
              break;
            case DrawMode::FILLING:
              // XXX should be able to flood-fill erase too
              FloodFill(mousex - DRAWING_X, mousey - DRAWING_Y,
                        0xFFFFFFFF);
              break;
            }

            SetDirty();
          }

        } else if (mode == Mode::LOOPTEST) {

          if (e->button == SDL_BUTTON_LEFT) {
            looptest_expected.emplace_back((float)mousex, (float)mousey);
          } else {
            looptest_actual.emplace_back((float)mousex, (float)mousey);
          }

          RecomputeLoop();

          SetDirty();

        } else if (mode == Mode::BEZDIST) {

          switch (e->button) {
          case SDL_BUTTON_LEFT:
            bez_startx = e->x;
            bez_starty = e->y;
            break;
          case SDL_BUTTON_RIGHT:
            bez_endx = e->x;
            bez_endy = e->y;
            break;
          case SDL_BUTTON_MIDDLE:
            bez_cx = e->x;
            bez_cy = e->y;
            break;
          }

          SetDirty();
        }

        break;
      }

      case SDL_MOUSEBUTTONUP: {
        // LMB/RMB, drag, etc.
        gamma_low.MouseUp();
        gamma_up.MouseUp();
        dragging = false;
        dragging_button = 0;
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
            SetDirty();
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
              Opt::Minimize<4>(GetErr,
                               {0.1, 0.1, -50.0, -50.0},
                               {10.0, 10.0, 50.0, 50.0},
                               1000);

            const auto [xscale2, yscale2, xoff2, yoff2] = args;

            printf("Done! Error: %.4f%%\n", err * 100.0);
            current_xscale = xscale2;
            current_yscale = yscale2;
            current_xoff = xoff2;
            current_yoff = yoff2;

            double rerr = GetErr({xscale2, yscale2, xoff2, yoff2});
            printf("Recomputed: %.4f%%\n", rerr * 100.0);

            SetDirty();
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
            mode = Mode::BEZDIST;
            break;
          case Mode::BEZDIST:
            mode = Mode::DRAW;
            break;
          case Mode::DRAW:
            mode = Mode::BITMAP;
            break;
          }
          SetDirty();
          break;

        case SDLK_0:
          current_xoff = current_yoff = 0.0f;
          current_scale = 100;
          current_xscale = current_yscale = 1.0f;
          SetDirty();
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

          case Mode::DRAW:
            // TODO: Shift bitmap around
            break;

          case Mode::BEZDIST:
            break;

          case Mode::LOOPTEST:
            break;

          case Mode::SORTITION:
            cur += dy;
            if (cur < 0) cur = 0;
            if (cur >= cur_filenames.size())
              cur = cur_filenames.size() - 1;
            SetDirty();
            break;

          case Mode::BITMAP:
          case Mode::SDFTEST:
            cur += dy;
            if (cur < 0) cur = 0;
            if (cur >= cur_filenames.size()) cur = cur_filenames.size() - 1;

            current_char += dx;
            if (current_char > 'z') current_char = 'z';
            else if (current_char < 'A') current_char = 'A';
            SetDirty();
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

            SetDirty();
            break;
          }
          }
          break;
        }


        case SDLK_PERIOD:
          draw_points = !draw_points;
          SetDirty();
          break;

        case SDLK_2:
          if (event.key.keysym.mod & KMOD_SHIFT) {
            printf("AT\n");
            only_bezier = !only_bezier;
            SetDirty();
          }
          break;

        case SDLK_6:
          if (event.key.keysym.mod & KMOD_SHIFT) {
            printf("CARET\n");
            normalize = !normalize;
            SetDirty();
          }
          break;

        case SDLK_PLUS:
        case SDLK_EQUALS:
          current_scale += 5;
          SetDirty();
          break;

        case SDLK_MINUS:
          if (current_scale > 15)
            current_scale -= 5;
          SetDirty();
          break;

        case SDLK_c: {
          if (mode == Mode::DRAW) {
            ClearDrawing();
            SetDirty();
          } else {
            SetFlag(Flag::SAME_CASE, false);
          }
          break;
        }
        case SDLK_x:
          SetFlag(Flag::SAME_CASE, true);
          break;

        case SDLK_g:
          if (mode == Mode::DRAW) {
            draw_mode = DrawMode::FILLING;
            SDL_SetCursor(cursor_bucket);
            SetDirty();
          } else {
            SetType(Type::SANS);
          }
          break;

        case SDLK_e:
          if (mode == Mode::DRAW) {
            draw_mode = DrawMode::ERASING;
            SDL_SetCursor(cursor_eraser);
            SetDirty();
          }
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
          if (mode == Mode::DRAW) {
            draw_mode = DrawMode::DRAWING;
            SDL_SetCursor(cursor_arrow);
            SetDirty();
          } else {
            SetType(Type::DECORATIVE);
          }
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
          SetDirty();
          break;


        default:;
        }
      }

      default:;
      }
    }

    if (IsDirty()) {
      if (mode == Mode::DRAW) {
        sdlutil::clearsurface(screen, 0xFF000033);
      } else if (mode == Mode::BEZDIST) {
        sdlutil::clearsurface(screen, 0xFF000000);
      } else {
        sdlutil::clearsurface(screen, 0xFFFFFFFF);
      }
      Draw();
      SDL_Flip(screen);
      SetDirty(false);
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
    float x = contour.StartX();
    float y = contour.StartY();
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
               TesselateQuadraticBezier<double>(x, y, p.cx, p.cy, p.x, p.y,
                                                sqerr)) {
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
      sdlutil::drawclippixel(screen, sx + x * scale, sy + y * scale - 1,
                             0, 0, 0xFF);

      sdlutil::drawclippixel(screen, sx + x * scale - 1, sy + y * scale,
                             0, 0, 0xFF);
      sdlutil::drawclippixel(screen, sx + x * scale, sy + y * scale,
                             0, 0, 0xFF);
      sdlutil::drawclippixel(screen, sx + x * scale - 1, sy + y * scale,
                             0, 0, 0xFF);

      sdlutil::drawclippixel(screen, sx + x * scale, sy + y * scale + 1,
                             0, 0, 0xFF);
    };

  if (draw_points) {
    // Now draw vertices to give a hint when there are "too many"
    // control points.
    for (const auto &contour : contours) {
      PointAt(contour.StartX(), contour.StartY());

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
    font2x->draw(SCREENW - font2x->width * 10, 32,
                 StringPrintf("^2CONFIRM ^5%d",
                              confirmations));
  }

  {
    double pct = (100.0 * fontdb.NumSorted()) / (double)fontdb.Size();
    string progress = StringPrintf("^6%.2f^4%%^<  %d^4/^<%d",
                                   pct, cur, cur_filenames.size());
    font2x->draw(SCREENW - font2x->sizex(progress) - 8,
                 SCREENH - font2x->height - 2,
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
  static constexpr int SIZE = 32;

  using SDFConfig = FontProblem::SDFConfig;
  SDFConfig config;
  config.sdf_size = SIZE;
  config.pad_top = 1;
  config.pad_bot = 4;
  config.pad_left = 4;
  config.onedge_value = 200;
  config.falloff_per_pixel = 18.0f;

  /*
  const float base_padding = (config.pad_top + config.pad_bot) * 0.5f;

  const float falloff_min =
    (float)config.onedge_value / (sqrtf(2.0f) * config.sdf_size * 0.5f);
  const float falloff_max = (float)config.onedge_value / base_padding;
  CHECK(falloff_max > falloff_min);
  // config.falloff_per_pixel = 0.5f * (falloff_min + falloff_max);
  config.falloff_per_pixel = 0.25f * falloff_max + 0.75 * falloff_min;
  */
  printf("Falloff per pixel: %.3f\n", config.falloff_per_pixel);

  const TTF *font = GetFont(cur);
  std::optional<ImageA> sdf =
    font->GetSDF(current_char, config.sdf_size,
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
}

static void BlitImage(const ImageRGBA &img, int xpos, int ypos) {
  // PERF should invest in fast blit of ImageRGBA to SDL screen
  for (int y = 0; y < img.Height(); y++) {
    for (int x = 0; x < img.Width(); x++) {
      int xx = xpos + x;
      int yy = ypos + y;
      if (yy >= 0 && yy < SCREENH &&
          xx >= 0 && xx < SCREENW) {
        auto [r, g, b, _] = img.GetPixel(x, y);
        sdlutil::drawpixel(screen, xpos + x, ypos + y, r, g, b);
      }
    }
  }
}

void UI::DrawDrawing() {
  {
    std::unique_lock<std::mutex> guard(result_m);
    sdlutil::blitall(drawing, screen, DRAWING_X, DRAWING_Y);
    if (network_image_result.has_value()) {
      const auto &res = network_image_result.value();
      BlitImage(res.input, 808, 849);
      BlitImage(res.low, LOW_X, LOW_Y + SDF_OUT_H + SDF_OUT_GAP);
      BlitImage(res.low_up, LOW_X, LOW_Y);
      BlitImage(res.up, UP_X, 559);
      BlitImage(res.up_low, UP_X, 856);

      constexpr int TEXTW = 100;
      // nominal
      constexpr int MAX_BAR_WIDTH = 200;
      constexpr int BAR_CENTER = 100;
      for (int i = 0; i < 26; i++) {
        const int lowy = 740 + i * (font->height + 2);
        const float f = res.low_pred[i];
        font->draw(LOW_X, lowy,
                   StringPrintf("%c: %+.8f", 'A' + i, f));
        if (f < 0.0f) {
          const int BAR_WIDTH = std::clamp(-f, 0.0f, 2.0f) * MAX_BAR_WIDTH;
          sdlutil::FillRectRGB(screen,
                               LOW_X + TEXTW + BAR_CENTER - BAR_WIDTH,
                               lowy + 2,
                               BAR_WIDTH,
                               font->height - 2,
                               0x88, 0x33, 0x00);
        } else {
          const int BAR_WIDTH = std::clamp(f, 0.0f, 2.0f) * MAX_BAR_WIDTH;
          sdlutil::FillRectRGB(screen,
                               LOW_X + TEXTW + BAR_CENTER,
                               lowy + 2,
                               BAR_WIDTH,
                               font->height - 2,
                               0x33, 0x88, 0x33);
        }
      }

      // XXX bars here too
      for (int i = 0; i < 26; i++) {
        font->draw(UP_X, 80 + i * (font->height + 2),
                   StringPrintf("%c: %+.8f", 'a' + i,
                                res.up_pred[i]));
      }

      gamma_low.Draw();
      gamma_up.Draw();

      // TODO!
      // std::array<float, 26> low_pred;
      // std::array<float, 26> up_pred;
    }
  }

  constexpr int pad_top =
    (SDF_CONFIG.pad_top / (float)SDF_CONFIG.sdf_size) * DRAWING_SIZE;
  constexpr int pad_left =
    (SDF_CONFIG.pad_left / (float)SDF_CONFIG.sdf_size) * DRAWING_SIZE;
  constexpr int pad_bottom =
    (1.0f - SDF_CONFIG.pad_bot / (float)SDF_CONFIG.sdf_size) * DRAWING_SIZE;
  sdlutil::drawline(screen, pad_left + DRAWING_X, DRAWING_Y - 4,
                    pad_left + DRAWING_X, DRAWING_Y + DRAWING_SIZE + 4,
                    0x00, 0x00, 0xFF);
  sdlutil::drawline(screen, DRAWING_X - 4, DRAWING_Y + pad_top,
                    DRAWING_X + DRAWING_SIZE + 4, DRAWING_Y + pad_top,
                    0xFF, 0x00, 0x00);
  sdlutil::drawline(screen, DRAWING_X - 4, DRAWING_Y + pad_bottom,
                    DRAWING_X + DRAWING_SIZE + 4, DRAWING_Y + pad_bottom,
                    0xFF, 0x00, 0x00);
}

void UI::DrawBez() {

  Timer bez_timer;
  auto [cpx, cpy, dist] =
    DistanceFromPointToQuadBezier(
        // The point to test
        bez_px, bez_py,
        // Bezier start point
        bez_startx, bez_starty,
        // Bezier control point
        bez_cx, bez_cy,
        // Bezier end point
        bez_endx, bez_endy);
  double bez_ms = bez_timer.MS();
  printf("Distance %.2f in %.6fs\n", dist, bez_ms / 1000.0);
  int bez_cpx = cpx, bez_cpy = cpy;

  auto DrawPt = [](int xx, int yy, uint8 r, uint8 g, uint8 b) {
      sdlutil::drawclippixel(screen, xx, yy, r, g, b);
      sdlutil::drawclippixel(screen, xx + 1, yy, r, g, b);
      sdlutil::drawclippixel(screen, xx, yy + 1, r, g, b);
      sdlutil::drawclippixel(screen, xx + 1, yy + 1, r, g, b);
    };

  constexpr float sqerr = 1.0f;
  int x = bez_startx;
  int y = bez_starty;
  for (const auto [xx, yy] :
         TesselateQuadraticBezier<double>(
             bez_startx, bez_starty, bez_cx, bez_cy, bez_endx, bez_endy,
             sqerr)) {
    sdlutil::drawclipline(screen, x, y, xx, yy, 0x33, 0x33, 0xFF);
    x = xx;
    y = yy;
  }

  sdlutil::drawclipline(screen,
                        bez_px, bez_py,
                        bez_cpx, bez_cpy, 0xFF, 0xFF, 033);

  DrawPt(bez_px, bez_py, 0xFF, 0xFF, 0xFF);
  DrawPt(bez_cpx, bez_cpy, 0xFF, 0xFF, 0xFF);

  DrawPt(bez_startx, bez_starty, 0x33, 0xFF, 0x33);
  DrawPt(bez_cx, bez_cy, 0x99, 0x99, 0x33);
  DrawPt(bez_endx, bez_endy, 0xFF, 0x33, 0x33);
}

void UI::Draw() {
  switch (mode) {
  case Mode::DRAW:
    DrawDrawing();
    break;

  case Mode::SORTITION:
    DrawSortition();
    break;

  case Mode::BEZDIST:
    DrawBez();
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
      float x = contour.StartX();
      float y = contour.StartY();
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
                 TesselateQuadraticBezier<double>(
                     x, y, p.cx, p.cy, p.x, p.y, sqerr)) {
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

  CHECK((cursor_arrow = Cursor::MakeArrow()));
  CHECK((cursor_bucket = Cursor::MakeBucket()));
  CHECK((cursor_eraser = Cursor::MakeEraser()));

  SDL_SetCursor(cursor_arrow);
  SDL_ShowCursor(SDL_ENABLE);

  UI ui;
  ui.Run();

  SDL_Quit();
  return 0;
}

