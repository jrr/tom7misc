
#include "convert.h"

#include <algorithm>
#include <utility>
#include <tuple>
#include <vector>
#include <string>
#include <cstring>
#include <memory>

#include "arcfour.h"
#include "base/logging.h"
#include "randutil.h"
#include "stb_image.h"

using namespace std;

// The NES NTSC palette, as RGB triplets.
// This is what FCEUX uses, but it's notoriously not that
// accurate (colors are too saturated).
static constexpr uint8 ntsc_palette[256 * 3] = {
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

static constexpr uint8 cart_palettes[4 * 4] = {
  0x1d, 0x16, 0x27, 0x06,
  // 0x00, 0x2d, 0x10, 0x20,
  0x00, 0x26, 0x36, 0x20,
  0x00, 0x06, 0x17, 0x26,
  0x00, 0x08, 0x07, 0x17,
};

// Dense r-g-b triplets.
// TODO: to cc-lib image.h?
struct ImageRGB {
  ImageRGB(std::vector<uint8> rgb, int width, int height) :
    width(width), height(height), rgb(std::move(rgb)) {}
  static ImageRGB *Load(const string &filename);
  ImageRGB *Clone() const {
    return new ImageRGB(rgb, width, height);
  }
  const int width, height;
  // Size width * height * 3.
  std::vector<uint8> rgb;
};

ImageRGB *ImageRGB::Load(const string &filename) {
  vector<uint8> ret;
  int width, height, bpp_unused;
  uint8 *stb_rgb = stbi_load(filename.c_str(),
			     &width, &height, &bpp_unused, 3);
  if (stb_rgb == nullptr) return nullptr;
  const int bytes = width * height * 3;
  ret.resize(bytes);
  memcpy(ret.data(), stb_rgb, bytes);
  return new ImageRGB(std::move(ret), width, height);
}

enum class PaletteMethod {
  // TODO: fixed, etc.
  MOST_COMMON,
  MOST_COMMON_SHUFFLED,
};

// TODO: This can be done much better, as a fancy optimization
// problem:
//  - Should not just be picking the most common colors, but
//    minimizing total error.
//  - Consider that it may still be useful to use a color
//    in more than one palette.
//  - Consider temporal dithering.
void MakePalette(PaletteMethod method, const ImageRGB *img,
		 ArcFour *rc, Screen *screen) {

  // Get the single closest NES color.
  auto ClosestColor = [](int r, int g, int b) -> int {
    // XXX TODO: use LAB DeltaE. But that is much more expensive...
    int best_sqerr = 65536 * 3 + 1;
    int best_i = 0;
    for (int nes_color = 0; nes_color < 256; nes_color++) {
      int rr = ntsc_palette[nes_color * 3 + 0];
      int gg = ntsc_palette[nes_color * 3 + 1];
      int bb = ntsc_palette[nes_color * 3 + 2];
      int dr = r - rr, dg = g - gg, db = b - bb;
      int sqerr = dr * dr + dg * dg + db * db;
      if (sqerr < best_sqerr) {
	best_i = nes_color;
	best_sqerr = sqerr;
      }
    }

    return best_i;
  };

  auto ColorCount = [img, &ClosestColor]() ->
    vector<pair<int, int>> {
    vector<int> was_best;
    for (int i = 0; i < 256; i++) was_best.push_back(0);
    for (int i = 0; i < img->width * img->height; i++) {
      uint8 r = img->rgb[i * 3 + 0];
      uint8 g = img->rgb[i * 3 + 1];
      uint8 b = img->rgb[i * 3 + 2];
      was_best[ClosestColor(r, g, b)]++;
    }

    // Now find the most common best colors.
    // XXX faster ways to do this!
    vector<pair<int, int>> color_count;
    color_count.reserve(256);
    for (int i = 0; i < 256; i++)
      color_count.push_back(make_pair(i, was_best[i]));
    std::sort(color_count.begin(),
	      color_count.end(),
	      [](const pair<int, int> &a,
		 const pair<int, int> &b) {
		// Descending by count.
		return b.second < a.second;
	      });
    return color_count;
  };
  
  switch (method) {
  case PaletteMethod::MOST_COMMON: {
    vector<pair<int, int>> color_count = ColorCount();
    // Put the overall most common colors in separate
    // palettes (is this a good idea??)

    for (int i = 0; i < 256; i++) {
      if (color_count[i].second) {
	printf("%02x: %d\n", color_count[i].first, color_count[i].second);
      }
    }
    printf("\n");

    // XXX There are more cases like this, like when we only have 6 colors, etc.
    if (color_count[4].second == 0) {
      // If we have only 4 colors, make sure they're in the same palette!
      for (int i = 0; i < 4; i++) {
	screen->palette[i +  0] = color_count[i].first;
	screen->palette[i +  4] = color_count[i].first;
	screen->palette[i +  8] = color_count[i].first;
	screen->palette[i + 12] = color_count[i].first;
      }
    } else {
      int rank = 0;
      // 0   1  2  3
      // 4   5  6  7
      // 8   9 10 11
      // 12 13 14 15
      for (int dest : { 0, 15, 11, 7, 3, 14, 10, 6, 2, 13, 9, 5, 1 }) {
	screen->palette[dest] = color_count[rank].first;
	rank++;
      }
    }
    break;
  }
  case PaletteMethod::MOST_COMMON_SHUFFLED: {
    vector<pair<int, int>> color_count = ColorCount();
    // Assign colors to slots at random.
    vector<int> slots = { 0, 15, 11, 7, 3, 14, 10, 6, 2, 13, 9, 5, 1 };
    Shuffle(rc, &slots);
    int rank = 0;
    for (int dest : slots) {
      screen->palette[dest] = color_count[rank].first;
      rank++;
      // Loop around if we didn't have enough colors to fill the
      // palette, though.
      if (rank >= color_count.size() ||
	  color_count[rank].second == 0) rank = 0;
    }
    break;
  }  
  default:
    LOG(FATAL) << "unknown method?";
  }
  
  // For cleanliness, always initialize these...
  screen->palette[4] = 0;
  screen->palette[8] = 0;
  screen->palette[12] = 0;
}

// Screen already has palette; fill in with image
static void FillScreenDithered(ImageRGB *img, Screen *screen) {
  // Each pixel stored as sixteenths.
  vector<int> rgb;
  rgb.reserve(img->rgb.size());
  for (int i = 0; i < img->rgb.size(); i++)
    rgb.push_back(img->rgb[i] * 16);
  
  // The main tricky thing about converting an image is
  // mapping image colors to NES colors. In this first pass,
  // we assume a fixed palette (4 different palettes, each
  // consisting of 1+3 colors). Each 8x1-pixel strip has to
  // use the same palette. So the core routine takes 8 pixels
  // and produces a palette selection (0-3) for the strip,
  // as well as one byte each for the high and low color bits.

  // Two-bit color index within palette #pal that matches the
  // RGB color best.
  auto ClosestColor = [&screen](int pal, int r, int g, int b) ->
    std::tuple<int, int, int> {
    // XXX TODO: use LAB DeltaE. But that is much more expensive...
    int best_sqerr = 0x7FFFFFFE;
    int best_i = 0, best_nes = 0;
    for (int i = 0; i < 4; i++) {
      // Index within the nes color gamut.
      int nes_color = i == 0 ? screen->palette[0] :
	screen->palette[pal * 4 + i];
      int rr = ntsc_palette[nes_color * 3 + 0];
      int gg = ntsc_palette[nes_color * 3 + 1];
      int bb = ntsc_palette[nes_color * 3 + 2];
      int dr = r - rr, dg = g - gg, db = b - bb;
      int sqerr = dr * dr + dg * dg + db * db;
      if (sqerr < best_sqerr) {
	best_i = i;
	best_nes = nes_color;
	best_sqerr = sqerr;
      }
    }

    // TODO: Also keep per-color error to propagate?
    return {best_i, best_nes, best_sqerr};
  };
  
  auto OneStrip = [&ClosestColor](
      // 8x3 bytes, rgb triplets
      const int *rgb,
      int *rgb_right,
      int *rgb_down) -> std::tuple<uint8, uint8, uint8> {

    // Try all four palettes, to minimize this total error.
    int best_totalerror = 0x7FFFFFFE;
    std::tuple<uint8, uint8, uint8> best;
    struct Errors {
      int r = 0, g = 0, b = 0;
    };
    Errors best_down[10];
    Errors best_right;
    auto Squared = [](const Errors &e) {
      return e.r * e.r + e.g * e.g + e.b * e.b;
    };
    
    for (int pal = 0; pal < 4; pal++) {
      int totalerror = 0;
      uint8 lobits = 0, hibits = 0;
      // rightward floyd-steinberg error, in sixteenths
      Errors right;
      Errors down[10];
      for (int x = 0; x < 8; x++) {
	int r = (int)rgb[x * 3 + 0] + right.r;
	int g = (int)rgb[x * 3 + 1] + right.g;
	int b = (int)rgb[x * 3 + 2] + right.b;

	// Each pixel must be one of the four selected colors.
	// So compute the closest.
	int p, nes_color, err;
	std::tie(p, nes_color, err) = ClosestColor(pal, r / 16, g / 16, b / 16);
	// totalerror += err;

	int rr = ntsc_palette[nes_color * 3 + 0];
	int gg = ntsc_palette[nes_color * 3 + 0];
	int bb = ntsc_palette[nes_color * 3 + 0];

	int dr = ((r / 16) - rr);
	int dg = ((g / 16) - gg);
	int db = ((b / 16) - bb);
	
	right.r = (7 * dr);
	right.g = (7 * dg);
	right.b = (7 * db);

	// For performance and quality: Could divide these errors
	// by 16 at the end, rather than eagerly?
	
	// down-left
	down[x].r += (3 * dr);
	down[x].g += (3 * dg);
	down[x].b += (3 * db);
	// down
	down[x + 1].r += (5 * dr);
	down[x + 1].g += (5 * dg);
	down[x + 1].b += (5 * db);
	// down-right
	down[x + 2].r += dr;
	down[x + 2].g += dg;
	down[x + 2].b += db;
	
	lobits <<= 1; lobits |= (p & 1);
	hibits <<= 1; hibits |= ((p >> 1) & 1);
      }
      totalerror = Squared(right);
      for (const Errors &e : down)
	totalerror += Squared(e);
      
      if (pal == 0 || totalerror < best_totalerror) {
	best = {(uint8)pal, lobits, hibits};
	best_totalerror = totalerror;
	for (int i = 0; i < 10; i++)
	  best_down[i] = down[i];

	best_right = right;
      }
    }

    // Now, for the best one, propagate the final error right and down.

    // XXX maybe everything should just be ints
    #if 0
    auto ClampMix = [](uint8 old, int err) -> uint8 {
      int n = (int)old + err;
      if (n < 0) return 0;
      if (n > 255) return 255;
      return (uint8)n;
    };
    #else
    auto ClampMix = [](int old, int err) -> int {
      int n = old + err;
      // if (n < -64) return -64;
      // if (n > 255 * 16) return 255 * 16;
      // if (n < 0) return 0;
      // if (n > 255 * 16) return 255 * 16;
      return n;
    };
    #endif

    rgb_right[0] = ClampMix(rgb_right[0], best_right.r);
    rgb_right[1] = ClampMix(rgb_right[1], best_right.g);
    rgb_right[2] = ClampMix(rgb_right[2], best_right.b);

    #if 1
    for (int i = 0; i < 10; i++) {
      rgb_down[i * 3 + 0] = ClampMix(rgb_down[i * 3 + 0], best_down[i].r);
      rgb_down[i * 3 + 1] = ClampMix(rgb_down[i * 3 + 1], best_down[i].g);
      rgb_down[i * 3 + 2] = ClampMix(rgb_down[i * 3 + 2], best_down[i].b);
    }
    #endif
    
    return best;
  };

  // Different pointers for dummy1 and dummy2 in the hopes that the
  // compiler can infer that these never alias.
  int dummy1[3] = {}, dummy2[10 * 3] = {};
  for (int scanline = 0; scanline < 240; scanline++) {
    for (int col = 0; col < 32; col++) {
      int idx = scanline * 32 + col;
      const int *strip = rgb.data() + idx * 8 * 3;
      int *right = col < 21 ? rgb.data() + (idx + 8) * 8 * 3 : &dummy1[0];
      // 10 pixels, starting below the strip but one pixel to the left, and continuing
      // one pixel after its end.
      // [.][.][s][t][r][i][p][8][8][8][.][.]
      // [.][d][o][w][n][0][0][0][0][0][0][.]
      // Note that this runs off the left edge (back onto the previous scanline), but we
      // don't care about that. It also extends one pixel off the right edge of the screen,
      // which we should fix.
      int *down = scanline < 238 ? rgb.data() + ((idx + 32) * 8 - 1) * 3 : &dummy2[0];
      uint8 pal, lobits, hibits;
      std::tie(pal, lobits, hibits) = OneStrip(strip, right, down);

      // The PPU will only read two bits out of this byte to determine
      // the palette selection. We could place the bits in the correct
      // place, but it's simpler to just fill all of the entries with
      // the same value.
      uint8 attr = pal | (pal << 2);
      attr = attr | (attr << 4);

      screen->attr[idx] = attr;
      screen->color_lo[idx] = lobits;
      screen->color_hi[idx] = hibits;
    }
  }
}

void FillScreenSelective(ImageRGB *img, Screen *screen) {
  
  // The main tricky thing about converting an image is
  // mapping image colors to NES colors. In this first pass,
  // we assume a fixed palette (4 different palettes, each
  // consisting of 1+3 colors). Each 8x1-pixel strip has to
  // use the same palette. So the core routine takes 8 pixels
  // and produces a palette selection (0-3) for the strip,
  // as well as one byte each for the high and low color bits.

  // Two-bit color index within palette #pal that matches the
  // RGB color best.
  auto ClosestColor = [&screen](int pal, int r, int g, int b) ->
    std::tuple<int, int> {
    // XXX TODO: use LAB DeltaE. But that is much more expensive...
    int best_sqerr = 65536 * 3 + 1;
    int best_i = 0;
    for (int i = 0; i < 4; i++) {
      // Index within the nes color gamut.
      int nes_color = screen->palette[pal * 4 + i];
      int rr = ntsc_palette[nes_color * 3 + 0];
      int gg = ntsc_palette[nes_color * 3 + 1];
      int bb = ntsc_palette[nes_color * 3 + 2];
      int dr = r - rr, dg = g - gg, db = b - bb;
      int sqerr = dr * dr + dg * dg + db * db;
      if (sqerr < best_sqerr) {
	best_i = i;
	best_sqerr = sqerr;
      }
    }

    // TODO: Also keep per-color error to propagate?
    return {best_i, best_sqerr};
  };
  
  auto OneStrip = [&ClosestColor](
      // 8x3 bytes, rgb triplets
      const uint8 *rgb) -> std::tuple<uint8, uint8, uint8> {

    // Try all four palettes, to minimize this total error.
    int best_totalerror = 0x7FFFFFFE;
    std::tuple<uint8, uint8, uint8> best;
    for (int pal = 0; pal < 4; pal++) {
      int totalerror = 0;
      uint8 lobits = 0, hibits = 0;
      for (int x = 0; x < 8; x++) {
	uint8 r = rgb[x * 3 + 0];
	uint8 g = rgb[x * 3 + 1];
	uint8 b = rgb[x * 3 + 2];

	// Each pixel must be one of the four selected colors.
	// So compute the closest.
	int p, err;
	std::tie(p, err) = ClosestColor(pal, r, g, b);
	totalerror += err;
	
	// TODO: Consider propagating some error, e.g.
	// Floyd-Steinbeg.

	lobits <<= 1; lobits |= (p & 1);
	hibits <<= 1; hibits |= ((p >> 1) & 1);
      }

      if (pal == 0 || totalerror < best_totalerror) {
	best = {(uint8)pal, lobits, hibits};
	best_totalerror = totalerror;
      }
    }
      
    return best;
  };

  for (int scanline = 0; scanline < 240; scanline++) {
    for (int col = 0; col < 32; col++) {
      int idx = scanline * 32 + col;
      const uint8 *strip = img->rgb.data() + idx * 8 * 3;
      uint8 pal, lobits, hibits;
      std::tie(pal, lobits, hibits) = OneStrip(strip);

      // The PPU will only read two bits out of this byte to determine
      // the palette selection. We could place the bits in the correct
      // place, but it's simpler to just fill all of the entries with
      // the same value.
      uint8 attr = pal | (pal << 2);
      attr = attr | (attr << 4);

      screen->attr[idx] = attr;
      screen->color_lo[idx] = lobits;
      screen->color_hi[idx] = hibits;
    }
  }
}

static std::unique_ptr<ImageRGB> Load(const string &filename) {
  std::unique_ptr<ImageRGB> img(ImageRGB::Load(filename));
  CHECK(img != nullptr) << filename;
  CHECK_EQ(img->width, 256) << filename << ": " << img->width;
  CHECK_EQ(img->height, 240) << filename << ": " << img->height;
  printf("ScreenFromFile %s\n", filename.c_str());
  return std::move(img);
}

Screen ScreenFromFileDithered(const string &filename) {
  std::unique_ptr<ImageRGB> img = Load(filename);
  Screen screen;
  ArcFour rc("sffd");
  MakePalette(PaletteMethod::MOST_COMMON_SHUFFLED, img.get(), &rc, &screen);
  FillScreenDithered(img.get(), &screen);
  return screen;
}


Screen ScreenFromFile(const string &filename) {
  std::unique_ptr<ImageRGB> img = Load(filename);
  Screen screen;
  ArcFour rc("sff");
  MakePalette(PaletteMethod::MOST_COMMON, img.get(), &rc, &screen);
  FillScreenSelective(img.get(), &screen);
  return screen;
}

vector<Screen> MultiScreenFromFile(const string &filename) {
  std::unique_ptr<ImageRGB> original_img = Load(filename);
  ArcFour rc("msff");
  vector<Screen> screens;
  screens.reserve(8);
  for (int i = 0; i < 8; i++) {
    Screen screen;
    std::unique_ptr<ImageRGB> img(original_img->Clone());
    MakePalette(PaletteMethod::MOST_COMMON_SHUFFLED, img.get(), &rc, &screen);
    FillScreenSelective(img.get(), &screen);
    screens.push_back(screen);
  }

  return screens;
}

