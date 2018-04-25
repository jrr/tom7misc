
#include "convert.h"
#include "stb_image.h"

#include <algorithm>
#include <utility>
#include <tuple>
#include <vector>
#include <string>
#include <cstring>
#include "base/logging.h"

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

// Dense r-g-b triplets.
// TODO: to cc-lib image.h?
struct ImageRGB {
  ImageRGB(std::vector<uint8> rgb, int width, int height) :
    width(width), height(height), rgb(std::move(rgb)) {}
  static ImageRGB *Load(const string &filename);
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

void MakePalette(const ImageRGB *img, Screen *screen) {
  // TODO: This can be done much better, as a fancy optimization
  // problem:
  //  - Should not just be picking the most common colors, but
  //    minimizing total error.
  //  - Consider that it may still be useful to use a color
  //    in more than one palette.
  //  - Consider temporal dithering.

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
  
  screen->palette[4] = 0;
  // For cleanliness, initialize these...
  screen->palette[8] = 0;
  screen->palette[12] = 0;
}

Screen ScreenFromFile(const string &filename) {
  Screen screen;
  ImageRGB *img = ImageRGB::Load(filename);
  CHECK(img != nullptr) << filename;
  CHECK_EQ(img->width, 256) << filename << ": " << img->width;
  CHECK_EQ(img->height, 240) << filename << ": " << img->height;
  printf("ScreenFromFile %s\n", filename.c_str());
  
  MakePalette(img, &screen);

  vector<int> rgb;
  rgb.reserve(img->rgb.size());
  for (int i = 0; i < img->rgb.size(); i++)
    rgb.push_back(img->rgb[i]);
  
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
      int nes_color = screen.palette[pal * 4 + i];
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
    struct DownErrs {
      int r[10] = {}, g[10] = {}, b[10] = {};
    };
    DownErrs best_down;
    int best_right_r = 0, best_right_g = 0, best_right_b = 0;
    
    for (int pal = 0; pal < 4; pal++) {
      int totalerror = 0;
      uint8 lobits = 0, hibits = 0;
      // rightward floyd-steinberg error
      int dither_next_r = 0, dither_next_g = 0, dither_next_b = 0;
      DownErrs down_errs;
      for (int x = 0; x < 8; x++) {
	int r = (int)rgb[x * 3 + 0] + dither_next_r;
	int g = (int)rgb[x * 3 + 1] + dither_next_g;
	int b = (int)rgb[x * 3 + 2] + dither_next_b;

	// Each pixel must be one of the four selected colors.
	// So compute the closest.
	int p, nes_color, err;
	std::tie(p, nes_color, err) = ClosestColor(pal, r, g, b);
	totalerror += err;

	int rr = ntsc_palette[nes_color * 3 + 0];
	int gg = ntsc_palette[nes_color * 3 + 0];
	int bb = ntsc_palette[nes_color * 3 + 0];

	int dr = (r - rr);
	int dg = (g - gg);
	int db = (b - bb);
	
	dither_next_r = (7 * dr) / 16;
	dither_next_g = (7 * dg) / 16;
	dither_next_b = (7 * db) / 16;

	// For performance and quality: Could divide these errors
	// by 16 at the end, rather than eagerly?
	
	// down-left
	down_errs.r[x] += (3 * dr) / 16;
	down_errs.g[x] += (3 * dg) / 16;
	down_errs.b[x] += (3 * db) / 16;
	// down
	down_errs.r[x + 1] += (5 * dr) / 16;
	down_errs.g[x + 1] += (5 * dg) / 16;
	down_errs.b[x + 1] += (5 * db) / 16;
	// down-right
	down_errs.r[x + 2] += dr / 16;
	down_errs.g[x + 2] += dg / 16;
	down_errs.b[x + 2] += db / 16;
	
	lobits <<= 1; lobits |= (p & 1);
	hibits <<= 1; hibits |= ((p >> 1) & 1);
      }

      if (pal == 0 || totalerror < best_totalerror) {
	best = {(uint8)pal, lobits, hibits};
	best_totalerror = totalerror;
	best_down = down_errs;
	
	best_right_r = dither_next_r;
	best_right_g = dither_next_g;
	best_right_b = dither_next_b;
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
      // if (n < 0) return 0;
      // if (n > 255) return 255;
      return n;
    };
    #endif

    rgb_right[0] = ClampMix(rgb_right[0], best_right_r);
    rgb_right[1] = ClampMix(rgb_right[1], best_right_g);
    rgb_right[2] = ClampMix(rgb_right[2], best_right_b);

    #if 1
    for (int i = 0; i < 10; i++) {
      rgb_down[i * 3 + 0] = ClampMix(rgb_down[i * 3 + 0], best_down.r[i]);
      rgb_down[i * 3 + 1] = ClampMix(rgb_down[i * 3 + 1], best_down.g[i]);
      rgb_down[i * 3 + 2] = ClampMix(rgb_down[i * 3 + 2], best_down.b[i]);
    }
    #endif
    
    return best;
  };

  // Different pointers for dummy1 and dummy2 in the hopes that the
  // compiler can infer that these never alias.
  int dummy1[3] = {}, dummy2[10 * 3] = {};
  for (int scanline = 0; scanline < 240; scanline++) {
    printf("%d\n", scanline);
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

      screen.attr[idx] = attr;
      screen.color_lo[idx] = lobits;
      screen.color_hi[idx] = hibits;
    }
  }
  return screen;
}
