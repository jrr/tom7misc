
#include "convert565.h"

#include <algorithm>
#include <utility>
#include <tuple>
#include <vector>
#include <string>
#include <cstring>
#include <memory>
#include <unordered_map>

// #include "arcfour.h"
#include "base/logging.h"
#include "randutil.h"
#include "stb_image.h"
// #include "gtl/top_n.h"
#include "color-util.h"

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

static constexpr float ntsc_lab[16 * 4 * 3] = {
  53.585022, -5.036026, -3.45214605,
  29.6371155, 23.5529995, -64.6291656,
  22.1817055, 51.3155174, -83.1460953,
  21.6919403, 51.9620781, -67.5401611,
  34.7772865, 56.5841637, -9.21894932,
  41.6881104, 61.8053665, 36.4953918,
  38.8516502, 57.5118523, 51.9031296,
  29.8871269, 42.671814, 42.0424538,
  24.6677055, 14.0535984, 34.0312424,
  24.8467712, -33.0162354, 31.6361389,
  26.5329819, -36.912693, 32.9891815,
  25.8723679, -29.6362038, 7.73772001,
  25.9787941, -5.94152498, -29.6009541,
  0, 0, 0,
  1.3708744, -0.292673707, -0.209954381,
  1.3708744, -0.292673707, -0.209954381,
  80.242836, -6.96530914, -4.77465391,
  52.2979355, 19.0739632, -80.3804016,
  44.2866783, 41.6147118, -93.452034,
  44.7721748, 64.4654007, -89.550148,
  55.1095657, 72.6896515, -31.6253185,
  55.5146027, 70.555954, 31.0264759,
  54.4606781, 70.1910553, 66.0955734,
  47.7291031, 55.7880402, 58.6518517,
  52.3695831, 29.0082703, 59.2190552,
  47.2285843, -46.9250374, 49.4496765,
  51.4687653, -59.6319427, 52.325016,
  50.4893646, -49.0196342, 16.9233494,
  59.1028366, -20.5771923, -40.2964363,
  12.7400093, -2.07997847, -1.42580569,
  2.46757317, -0.526808202, -0.37792027,
  2.46757317, -0.526808202, -0.37792027,
  100, -8.39516544, -5.75482845,
  79.5664062, -35.8837852, -37.3016472,
  66.5662918, 2.38218904, -57.3839912,
  67.0186462, 46.5845757, -56.1123734,
  63.3363342, 78.6822739, -54.7360306,
  63.0771408, 56.4725418, 5.66347837,
  68.833313, 32.8926735, 60.0371552,
  72.9486847, 21.8501987, 73.0660019,
  79.8405075, 3.08528543, 74.9064636,
  83.0937271, -53.1411476, 77.272377,
  83.487999, -84.017128, 68.1383362,
  84.5216599, -71.0992203, 19.2921867,
  89.9704437, -53.1529198, -21.5552559,
  39.9040565, -4.04590368, -2.77342796,
  3.63551331, -0.776149333, -0.556796789,
  3.63551331, -0.776149333, -0.556796789,
  100, -8.39516544, -5.75482845,
  93.8587341, -32.5998077, -15.3859377,
  90.2255249, -21.1581593, -20.7108269,
  76.0879745, 21.1688271, -30.7985191,
  79.6751785, 36.4523811, -32.7660217,
  78.2541122, 24.4877644, 4.16406393,
  87.2370834, 3.41114402, 18.3364506,
  94.2046661, -12.8216152, 32.9156532,
  96.0729752, -17.9438, 40.4084816,
  89.0784988, -25.8738098, 34.4339981,
  87.8568878, -41.1373062, 18.6791782,
  89.9726181, -36.701889, -1.31937265,
  94.0189896, -37.879406, -13.6184578,
  88.115448, -7.53507042, -5.16521931,
  5.0633297, -1.08097494, -0.77547431,
  5.0633297, -1.08097494, -0.77547431,
};

static constexpr inline float ByteFloat(uint8 ch) {
  constexpr float inv255 = 1.0f / 255.0f;
  return ch * inv255;
}

// Slow! Just used to fill in the table at start.
static uint8 ClosestColorRGB(int r, int g, int b) {
  float sl, sa, sb;
  ColorUtil::RGBToLAB(ByteFloat(r), ByteFloat(g), ByteFloat(b),
		      &sl, &sa, &sb);
  // int best_sqerr = 65536 * 3 + 1;
  float best_delta_e = 65536.0;
  int best_i = 0;
  for (int nes_color = 0; nes_color < 64; nes_color++) {
    // Don't even consider 1d, 0e, 1e, 2e, 3e, 0f, 1f, 2f, 3f,
    // since these are all black (we use 0d for that).
    // Also, 20 and 30 are basically identical whites.
    const int cm = nes_color & 0x0f;
    if (cm == 0x0e || cm == 0x0f || nes_color == 0x1d || nes_color == 0x30)
      continue;

    
    float nl = ntsc_lab[nes_color * 3 + 0];
    float na = ntsc_lab[nes_color * 3 + 1];
    float nb = ntsc_lab[nes_color * 3 + 2];
    float delta_e = ColorUtil::DeltaE(sl, sa, sb, nl, na, nb);
    if (delta_e < best_delta_e) {
      best_i = nes_color;
      best_delta_e = delta_e;
    }
  }

  return best_i;
};

// nes_delta_e[src * 64 + dst] = DeltaE(lab(src), lab(dst));
uint8 nes_delta_e[64 * 64] = {};

// The closest NES color for a given NES color. This is almost
// the identity, except that we normalize colors that are
// equal: ?e and ?f and 1d become 0d (perfect blacks)
// and 30 becomes 20 (perfect whites).
static uint8 closest_color_nes[64] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0D, 0x0D,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x0D, 0x0D, 0x0D,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x0D, 0x0D,
  0x20, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x0D, 0x0D,
};

// In 565 color space, there are only 2^16 colors.
// We can just use a table lookup
static uint8 closest_color565[65536] = {0};
void InitClosestColors565() {
  for (int c = 0; c < 65536; c++) {
    uint16 packed = c;
    uint8 b = (packed & 31) << 3;
    uint8 g = ((packed >> 5) & 63) << 2;
    uint8 r = ((packed >> 11) & 31) << 3;
    closest_color565[c] = ClosestColorRGB(r, g, b);
  }

  // DeltaE for NTSC palette and itself.
  float max = 0;
  for (int s = 0; s < 64; s++) {
    float sl = ntsc_lab[s * 3 + 0];
    float sa = ntsc_lab[s * 3 + 1];
    float sb = ntsc_lab[s * 3 + 2];
    for (int d = 0; d < 64; d++) {
      float dl = ntsc_lab[d * 3 + 0];
      float da = ntsc_lab[d * 3 + 1];
      float db = ntsc_lab[d * 3 + 2];
      // Stretched to [0,255].
      float delta_e8 =
	ColorUtil::DeltaE(sl, sa, sb,
			  dl, da, db) * 1.8;
      if (delta_e8 > max) max = delta_e8;
      // printf("0x%02x, ", (uint8)delta_e8);
      nes_delta_e[s * 64 + d] = (uint8)delta_e8;
    }
  }
  // printf("Max was %.4f\n", max);
}
  
static inline int BigramKey(uint8 c1, uint8 c2) {
  if (c1 <= c2) {
    return c1 * 64 + c2;
  } else {
    return c2 * 64 + c1;
  }
}

static inline pair<uint8, uint8> UnBigramKey(int i) {
  return make_pair(i / 64, i % 64);
}

void MakePaletteFromBigrams(const vector<int> &was_best_bigram,
			    Screen *screen) {
  // Now find the most common bigrams.
  // PERF faster ways to do this!
  // (like for example TopN. We could never use more than
  // 13^2 bigrams, so we need only look at that many. The
  // actual bound is probably lower than that, even.)
  vector<pair<pair<uint8, uint8>, int>> bigram_count;
  bigram_count.reserve(64);
  for (int i = 0; i < was_best_bigram.size(); i++)
    bigram_count.push_back(make_pair(UnBigramKey(i),
				     was_best_bigram[i]));
  std::sort(bigram_count.begin(),
	    bigram_count.end(),
	    [](const pair<pair<uint8, uint8>, int> &a,
	       const pair<pair<uint8, uint8>, int> &b) {
	      // Descending by count.
	      return b.second < a.second;
	    });

  // Get the overall most common color, which should be the background
  // color. (This is not always optimal, but a good policy..)
  int was_best[64] = {0};
  for (const pair<pair<uint8, uint8>, int> &p : bigram_count) {
    was_best[p.first.first] += p.second;
    was_best[p.first.second] += p.second;
  }

  // Find the single best with a linear scan;
  int best_count = was_best[0];
  int besti = 0;
  for (int i = 1; i < 64; i++) {
    if (was_best[i] > best_count) {
      best_count = was_best[i];
      besti = i;
    }
  }

  // For each color index, in which palettes does it already appear?
  vector<vector<int>> palettes;
  palettes.resize(64);

  // Initialize each palette to contain just the background color.
  const int bg = besti;
  screen->palette[0] = bg;
  screen->palette[4] = bg;
  screen->palette[8] = bg;
  screen->palette[12] = bg;

  palettes[bg] = {0, 1, 2, 3};

  // Loop until we've used up all our slots.
  int colors_left = 4 * 3;
  // Next free index in each palette.
  int nextp[4] = {1, 1, 1, 1};

  // Now populate the palette. For each bigram, if there's space in
  // a palette that already has one of the two colors, add it there.
  // Otherwise, insert both colors in a palette with space.
  for (int row_idx = 0; row_idx < bigram_count.size(); row_idx++) {
    const auto &row = bigram_count[row_idx];
    if (row.second == 0) {
      break;
    }
    if (colors_left == 0) {
      break;
    }

    auto PlaceSingleton = [screen, &nextp, &palettes, &colors_left](
	uint8 c) {
      for (int p = 0; p < 4; p++) {
	if (nextp[p] != 4) {
	  screen->palette[p * 4 + nextp[p]] = c;
	  nextp[p]++;
	  palettes[c].push_back(p);
	  colors_left--;
	  return true;
	}
      }
      return false;
    };

    // We don't care about the count any more, just the colors.
    const uint8 c1 = row.first.first;
    const uint8 c2 = row.first.second;
    if (c1 == c2) {
      // Here we're just inserting a single color.
      if (!palettes[c1].empty()) {
	// Already present.
	goto next_bigram;
      }

      // XXX would be a little better to allocate singletons at the
      // end (but reserve space for them), since we might later see
      // a bigram that involves that singleton (and prefer to place
      // them together).
      // Otherwise, put it in some palette.
      // Will succeed since colors_left > 0.
      (void)PlaceSingleton(c1);

    } else {
      // Otherwise, prefer to place one color in a palette that already
      // contains the other.
      auto CanFriend = [screen, &nextp, &palettes, &colors_left](
	  uint8 c1, uint8 c2) -> bool {
	for (int p : palettes[c1]) {
	  if (nextp[p] != 4) {
	    screen->palette[p * 4 + nextp[p]] = c2;
	    nextp[p]++;
	    palettes[c2].push_back(p);
	    colors_left--;
	    return true;
	  }
	}
	return false;
      };

      auto Friends = [&palettes](uint8 c1, uint8 c2) {
	for (int p1 : palettes[c1])
	  for (int p2 : palettes[c2])
	    if (p1 == p2) return true;
	return false;
      };

      // XXX they may already be in the same palette. if so, we're done.
      if (Friends(c1, c2)) goto next_bigram;

      if (CanFriend(c1, c2)) goto next_bigram;
      if (CanFriend(c2, c1)) goto next_bigram;

      // Otherwise, try to place both in the same palette.
      for (int p = 0; p < 4; p++) {
	if (nextp[p] < 3) {
	  screen->palette[p * 4 + nextp[p]] = c1;
	  nextp[p]++;
	  palettes[c1].push_back(p);
	  screen->palette[p * 4 + nextp[p]] = c2;
	  nextp[p]++;
	  palettes[c2].push_back(p);
	  colors_left -= 2;
	  goto next_bigram;
	}
      }

      // Finally, place as singletons.
      if (PlaceSingleton(c1))
	PlaceSingleton(c2);
    }

  next_bigram:;
  }

  // For cleanliness, always initialize these...
  screen->palette[4] = 0;
  screen->palette[8] = 0;
  screen->palette[12] = 0;
}

void MakePalette565(const void *data,
		    int width, int height, int pitch,
		    bool offset, Screen *screen) {

  // The bigrams approach is high quality, and not too
  // slow if the screen already consists of a low number
  // of colors (typical on SNES).

  // Most common bigrams, in descending order by frequency.
  // Key is pair of colors (c1, c2) cooccurring in an
  // 8x1 strip (not necessarily adjacent!), with c1 <= c2.  
  vector<int> was_best_bigram;
  was_best_bigram.reserve(64 * 64);
  for (int i = 0; i < 64 * 64; i++) was_best_bigram.push_back(0);

  const int off = offset ? 4 : 0;
  for (int y = 0; y < height; y ++) {
    uint16 *line = (uint16*)&((uint8 *)data)[y * pitch];
    for (int x = offset; x < width; x += 8) {
      // Closest color for a pixel in the strip.

      uint8 cs[8];
      for (int i = 0; i < 8; i++) {
	uint16 packed = line[x];
	cs[i] = closest_color565[packed];
      }

      // Now, insert all pairs.
      for (int i = 0; i < 7; i++) {
	for (int j = i + 1; j < 8; j++) {
	  was_best_bigram[BigramKey(cs[i], cs[j])]++;
	}
      }
    }
  }

  MakePaletteFromBigrams(was_best_bigram, screen);
}


// As above, but this is even easier because we know the best NES
// color entry is just the color itself (after normalization).
void MakePaletteNES(const uint8 *data, Screen *screen) {
  vector<int> was_best_bigram;
  was_best_bigram.reserve(64 * 64);
  for (int i = 0; i < 64 * 64; i++) was_best_bigram.push_back(0);
	 
  for (int y = 0; y < 240; y++) {
    const int yy = y * 256;
    for (int x = 0; x < 256; x += 8) {
      uint8 cs[8];
      for (int i = 0; i < 8; i++) {
	uint8 color = data[yy + x + i] & 63;
	cs[i] = closest_color_nes[color];
      }
      
      // Now, insert all pairs.
      for (int i = 0; i < 7; i++) {
	for (int j = i + 1; j < 8; j++) {
	  was_best_bigram[BigramKey(cs[i], cs[j])]++;
	}
      }
    }
  }

  MakePaletteFromBigrams(was_best_bigram, screen);
}

// Given a palette index and a NES color, return the best
// entry in that palette for it, along with the 8-bit
// delta-e error.
std::tuple<int, int> ClosestEntry(const uint8 *palette,
				  uint8 bg_err,
				  int pal, uint8 best_nes) {
  uint8 best_err = bg_err;
  int best_i = 0;
  for (int i = 1; i < 4; i++) {
    // Index within the nes color gamut.
    int nes_color = palette[pal * 4 + i];
    uint8 err = nes_delta_e[best_nes * 64 + nes_color];
    if (err < best_err) {
      best_i = i;
      best_err = err;
    }
  }

  return {best_i, (int)best_err};
}

void FillScreenFast565(const void *data,
		       int width, int height, int pitch,
		       bool offset, Screen *screen) {

  auto OneStrip = [screen](
      // Eight 16-bit pixels RGB 565 format
      const uint16 *rgb565) -> std::tuple<uint8, uint8, uint8> {
    
    // Try all four palettes, to minimize this total error.
    // Worst error for a single pixel is 255, so...
    int best_totalerror = 256 * 8;
    std::tuple<uint8, uint8, uint8> best;
    uint8 bg_err[8];
    uint8 best_nes[8];
    const uint8 bgcolor = screen->palette[0];
    for (int x = 0; x < 8; x++) {
      uint16 packed = rgb565[x];
      // As a simplification, look up the closest NES color,
      // and use that as our source for error.
      uint8 best = closest_color565[packed];
      best_nes[x] = best;
      bg_err[x] = nes_delta_e[64 * best + bgcolor];
    }
    
    for (int pal = 0; pal < 4; pal++) {
      int totalerror = 0;
      uint8 lobits = 0, hibits = 0;
      for (int x = 0; x < 8; x++) {
	// uint16 packed = rgb565[x];
	// As a simplification, look up the closest NES color,
	// and use that as our source for error.
	// int best_nes = closest_color565[packed];
	uint8 best = best_nes[x];

	// Each pixel must be one of the four selected colors.
	// So compute the closest.
	int p, err;
	std::tie(p, err) = ClosestEntry(screen->palette,
					bg_err[x], pal, best);
	totalerror += err;

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

  const int off = offset ? 4 : 0;
  for (int scanline = 0; scanline < 240; scanline++) {
    uint16 *line = (uint16*)&((uint8 *)data)[scanline * pitch];
    for (int col = 0; col < 32; col++) {
      int idx = scanline * 32 + col;
      const uint16 *strip = &line[col * 8 - off];
      
      uint8 pal, lobits, hibits;
      std::tie(pal, lobits, hibits) = OneStrip(strip);

      uint8 attr = pal | (pal << 2);
      attr = attr | (attr << 4);

      screen->attr[idx] = attr;
      screen->color_lo[idx] = lobits;
      screen->color_hi[idx] = hibits;
    }
  }

  screen->palette[4] = off;
}

void FillScreenFastNES(const uint8 *data,
		       Screen *screen) {
  auto OneStrip = [screen](
      // Eight 8-bit pixels, as NES colors (must be masked & 63)
      const uint8 *nespixels) -> std::tuple<uint8, uint8, uint8> {
    
    // Try all four palettes, to minimize this total error.
    // Worst error for a single pixel is 255, so...
    int best_totalerror = 256 * 8;
    std::tuple<uint8, uint8, uint8> best;
    uint8 bg_err[8];
    uint8 best_nes[8];
    const uint8 bgcolor = screen->palette[0];
    for (int x = 0; x < 8; x++) {
      // Here we don't need to normalize with closest_color_nes,
      // since nes_delta_e will give us 0 for all variants.
      uint8 best = nespixels[x] & 63;
      best_nes[x] = best;
      bg_err[x] = nes_delta_e[64 * best + bgcolor];
    }
    
    for (int pal = 0; pal < 4; pal++) {
      int totalerror = 0;
      uint8 lobits = 0, hibits = 0;
      for (int x = 0; x < 8; x++) {
	uint8 best = best_nes[x];

	// Each pixel must be one of the four selected colors.
	// So compute the closest.
	int p, err;
	std::tie(p, err) = ClosestEntry(screen->palette,
					bg_err[x], pal, best);
	totalerror += err;

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
    int yy = scanline * 256;
    for (int col = 0; col < 32; col++) {
      int idx = scanline * 32 + col;
      const uint8 *strip = &data[yy + col * 8];
      
      uint8 pal, lobits, hibits;
      std::tie(pal, lobits, hibits) = OneStrip(strip);

      uint8 attr = pal | (pal << 2);
      attr = attr | (attr << 4);

      screen->attr[idx] = attr;
      screen->color_lo[idx] = lobits;
      screen->color_hi[idx] = hibits;
    }
  }
}
