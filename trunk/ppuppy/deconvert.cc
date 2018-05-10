// Round-trip converts a graphic into Screen format, then back to RGBA
// PNG, for debugging.

#include <string>
#include "../cc-lib/image.h"
#include "ppuppy.h"
#include "screen.h"
#include "convert.h"
#include "base/stringprintf.h"
#include "util.h"

using namespace std;

// The NES NTSC palette, as RGB triplets.
// This is what FCEUX uses, but it's notoriously not that
// accurate (colors are too saturated).
static constexpr uint8 ntsc_palette[16 * 4 * 3] = {
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

ImageRGBA Deconvert(const Screen &screen) {
  vector<uint8> rgba;
  rgba.reserve(256 * 256 * 4);
  for (int i = 0; i < 256 * 256; i++) {
    rgba.push_back(0x00);
    rgba.push_back(0x00);
    rgba.push_back(0x00);
    rgba.push_back(0xFF);    
  }

  const uint8 scroll_x = screen.palette[4];
  printf("scroll_x: %02x\n", scroll_x);
  
  for (int y = 0; y < 240; y++) {
    for (int col = 0; col < 32; col++) {
      // Note: This doesn't read the same color bits that the PPU
      // would read, but they are all the same if produced by Convert.
      const uint8 attr = screen.attr[y * 32 + col] & 3;
      const uint8 color_lo = screen.color_lo[y * 32 + col];
      const uint8 color_hi = screen.color_hi[y * 32 + col];
      
      for (int bit = 0; bit < 8; bit++) {
	const int x = col * 8 + bit;
	const uint8 value =
	  ((color_lo >> (7 - bit)) & 1) |
	  (((color_hi >> (7 - bit)) & 1) << 1);

	const int nes_color =
	  value == 0 ?
	  screen.palette[0] :
	  screen.palette[attr * 4 + value];

	if (x >= scroll_x) {
	  const int idx = (y * 256 + x - scroll_x) * 4;
	  rgba[idx + 0] = ntsc_palette[nes_color * 3 + 0];
	  rgba[idx + 1] = ntsc_palette[nes_color * 3 + 1];
	  rgba[idx + 2] = ntsc_palette[nes_color * 3 + 2];
	}
      }
    }
  }

  auto IsBlack = [](int nes_color) {
    switch (nes_color) {
    case 0x0d:
    case 0x1d:
    case 0x0e:
    case 0x1e:
    case 0x2e:
    case 0x3e:
    case 0x0f:
    case 0x1f:
    case 0x2f:
    case 0x3f:
      return true;
    default:
      return false;
    }
  };
  
  // Show palettes at the bottom.
  for (int p = 0; p < 4; p ++) {
    const int yy = 256 - 14;
    for (int i = 0; i < 4; i++) {
      const int nes_color =
	i == 0 ?
	screen.palette[0] :
	screen.palette[p * 4 + i];
      bool is_black = IsBlack(nes_color);
      uint8 r = ntsc_palette[nes_color * 3 + 0];
      uint8 g = ntsc_palette[nes_color * 3 + 1];
      uint8 b = ntsc_palette[nes_color * 3 + 2];

      const int xx = 2 + p * 67 + i * 13;

      for (int y = 0; y < 12; y++) {
	for (int x = 0; x < 12; x++) {
	  int idx = ((yy + y) * 256 + xx + x) * 4;
	  if (is_black && (y == 0 || x == 0 || y == 11 || x == 11)) {
	    rgba[idx + 0] = 0xFF;
	    rgba[idx + 1] = 0xFF;
	    rgba[idx + 2] = 0xFF;
	  } else {
	    rgba[idx + 0] = r;
	    rgba[idx + 1] = g;
	    rgba[idx + 2] = b;
	  }
	}
      }

      // If the color is very dark, draw a border
      // around it
      
    }
  }

  return ImageRGBA(std::move(rgba), 256, 256);
}

// Generate palette swatches image.
ImageRGBA Swatches() {
  vector<uint8> rgba;
  rgba.resize(256 * 256 * 4);
  for (int y = 0; y < 4 * 16; y++) {
    int yy = y >> 4;
    for (int x = 0; x < 256; x++) {
      int xx = x >> 4;
      int nes_color = yy * 16 + xx;
      uint8 r = ntsc_palette[nes_color * 3 + 0];
      uint8 g = ntsc_palette[nes_color * 3 + 1];
      uint8 b = ntsc_palette[nes_color * 3 + 2];
      int idx = (y * 256 + x) * 4;
      rgba[idx + 0] = r;
      rgba[idx + 1] = g;
      rgba[idx + 2] = b;
      rgba[idx + 3] = 0xFF;
    }
  }
  return ImageRGBA(std::move(rgba), 256, 256);
}

static void Save(const ImageRGBA &img,
		 const string &outfile) {
  img.Save(outfile);
  fprintf(stderr, "Wrote %s\n", outfile.c_str());
}



int main(int argc, char **argv) {
  // fprintf(stderr, "Byte size of screen: %d\n", (int)sizeof(Screen));
  const char *USAGE = "./deconvert [-multi] input.ext out.png\n";
  CHECK(argc >= 2) << USAGE;
  bool multi = false;

  int argi = 1;
  if (0 == strcmp(argv[argi], "-multi")) {
    multi = true;
    argi++;
  }

  CHECK(argi < argc) << USAGE;
  string infile = argv[argi++];
  CHECK(argi < argc) << USAGE;
  string base = argv[argi++];
  
  CHECK(Util::endswith(base, ".png")) << base;
  base.resize(base.size() - 4);

  if (multi) {
    vector<Screen> screens = MultiScreenFromFile(infile);
    for (int i = 0; i < screens.size(); i++) {
      const Screen &screen = screens[i];
      const string outfile = StringPrintf("%s-%d.png", base.c_str(), i);
      ImageRGBA img = Deconvert(screen);
      Save(img, outfile);
    }
  } else {
    Screen screen = ScreenFromFile(infile);
    ImageRGBA img = Deconvert(screen);
    // ImageRGBA img = Swatches();
    Save(img, base + ".png");
  }
  return 0;
}
