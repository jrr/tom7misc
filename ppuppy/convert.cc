
#include "convert.h"
#include "stb_image.h"

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

// The four background palettes installed into PPU internal memory.
// Currently fixed (from Ice Hockey) player select screen (and
// this is a poor palette because it has many repeated colors).
static constexpr uint8 cart_palettes[4 * 4] = {
  0x0F, 0x00, 0x05, 0x27,
  0x0F, 0x15, 0x05, 0x00,
  0x0F, 0x25, 0x15, 0x20,
  0x0F, 0x31, 0x30, 0x22,
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

Screen ScreenFromFile(const string &filename) {
  Screen screen;
  ImageRGB *img = ImageRGB::Load(filename);
  CHECK(img != nullptr) << filename;
  CHECK_EQ(img->width, 256) << filename << ": " << img->width;
  CHECK_EQ(img->height, 240) << filename << ": " << img->height;

  // The main tricky thing about converting an image is
  // mapping image colors to NES colors. In this first pass,
  // we assume a fixed palette (4 different palettes, each
  // consisting of 1+3 colors). Each 8x1-pixel strip has to
  // use the same palette. So the core routine takes 8 pixels
  // and produces a palette selection (0-3) for the strip,
  // as well as one byte each for the high and low color bits.

  // Two-bit color index within palette #pal that matches the
  // RGB color best.
  auto ClosestColor = [](int pal, int r, int g, int b) ->
    std::tuple<int, int> {
    // XXX TODO: use LAB DeltaE. But that is much more expensive...
    int best_sqerr = 65536 * 3 + 1;
    int best_i = 0;
    for (int i = 0; i < 4; i++) {
      // Index within the nes color gamut.
      int nes_color = cart_palettes[pal * 4 + i];
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

      screen.attr[idx] = attr;
      screen.color_lo[idx] = lobits;
      screen.color_hi[idx] = hibits;
    }
  }
  return screen;
}
