
#include "convert.h"

#include <algorithm>
#include <utility>
#include <tuple>
#include <vector>
#include <string>
#include <cstring>
#include <memory>
#include <unordered_map>

#include "arcfour.h"
#include "base/logging.h"
#include "randutil.h"
#include "stb_image.h"

#include "color-util.h"

using namespace std;

#define DEBUG_CONVERT false

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

// XXX optimize this
static constexpr uint8 greyscale_palettes[4 * 4] = {
  0x1d, 0x00, 0x2d, 0x30,
  0x00, 0x10, 0x2d, 0x3d,
  0x00, 0x00, 0x10, 0x3d,
  0x00, 0x2d, 0x3d, 0x30,
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

static constexpr inline float ByteFloat(uint8 ch) {
  constexpr float inv255 = 1.0f / 255.0f;
  return ch * inv255;
}

// TODO: This can be done much better, as a fancy optimization
// problem:
//  - Should not just be picking the most common colors, but
//    minimizing total error.
//  - Consider that it may still be useful to use a color
//    in more than one palette.
//  - Consider temporal dithering.
//
// Better way to do this is to collect 8-pixel strips (perhaps
// quantize and count them somehow) and then do k-means etc.
// in order to solve for the least lossy global palette. We
// fail to put a common color in multiple palettes too often,
// which creates more error than we need to.
// We also suffer from using two colors that are actually
// quite close to one another (slight difference in greys),
// because this "most popular color" code treats all distinct
// nes colors as equally important.
void MakePalette(PaletteMethod method, const ImageRGB *img,
		 ArcFour *rc, Screen *screen) {

  // Initialize the table. XXX just make this constexpr.
#if 0
  float ntsc_lab[16 * 4 * 3];
  for (int nes_color = 0; nes_color < 16 * 4; nes_color++) {
    int rr = ntsc_palette[nes_color * 3 + 0];
    int gg = ntsc_palette[nes_color * 3 + 1];
    int bb = ntsc_palette[nes_color * 3 + 2];
    float il, ia, ib;
    ColorUtil::RGBToLAB(ByteFloat(rr), ByteFloat(gg), ByteFloat(bb),
			&il, &ia, &ib);
    ntsc_lab[nes_color * 3 + 0] = il;
    ntsc_lab[nes_color * 3 + 1] = ia;
    ntsc_lab[nes_color * 3 + 2] = ib;
  }

  for (int nes_color = 0; nes_color < 16 * 4; nes_color++) {
    printf("%.9g, %.9g, %.9g,  \n",
	   ntsc_lab[nes_color * 3 + 0],
	   ntsc_lab[nes_color * 3 + 1],
	   ntsc_lab[nes_color * 3 + 2]);
  }
#endif
  
  // Get the single closest NES color.
  auto ClosestColor = [](int r, int g, int b) -> uint8 {
    float sl, sa, sb;
    ColorUtil::RGBToLAB(ByteFloat(r), ByteFloat(g), ByteFloat(b),
			&sl, &sa, &sb);
    // int best_sqerr = 65536 * 3 + 1;
    float best_delta_e = 65536.0;
    int best_i = 0;
    for (int nes_color = 0; nes_color < 64; nes_color++) {
      // Don't even consider 1d, 0e, 1e, 2e, 3e, 0f, 1f, 2f, 3f,
      // since these are all black (we use 0d for that).
      const int cm = nes_color & 0x0f;
      if (cm == 0x0e || cm == 0x0f || nes_color == 0x1d)
	continue;

      float nl = ntsc_lab[nes_color * 3 + 0];
      float na = ntsc_lab[nes_color * 3 + 1];
      float nb = ntsc_lab[nes_color * 3 + 2];
      float delta_e = ColorUtil::DeltaE(sl, sa, sb, nl, na, nb);
      if (delta_e < best_delta_e) {
	best_i = nes_color;
	best_delta_e = delta_e;
      }
      
      /*
      int rr = ntsc_palette[nes_color * 3 + 0];
      int gg = ntsc_palette[nes_color * 3 + 1];
      int bb = ntsc_palette[nes_color * 3 + 2];
      int dr = r - rr, dg = g - gg, db = b - bb;
      int sqerr = dr * dr + dg * dg + db * db;
      if (sqerr < best_sqerr) {
	best_i = nes_color;
	best_sqerr = sqerr;
      }
      */
    }

    return best_i;
  };

  auto ColorCount = [img, &ClosestColor]() ->
    vector<pair<int, int>> {
    vector<int> was_best;
    was_best.reserve(64);
    for (int i = 0; i < 64; i++) was_best.push_back(0);
    for (int i = 0; i < img->width * img->height; i++) {
      uint8 r = img->rgb[i * 3 + 0];
      uint8 g = img->rgb[i * 3 + 1];
      uint8 b = img->rgb[i * 3 + 2];
      was_best[ClosestColor(r, g, b)]++;
    }

    // Now find the most common best colors.
    // XXX faster ways to do this!
    vector<pair<int, int>> color_count;
    color_count.reserve(64);
    for (int i = 0; i < 64; i++)
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

  // XXX: But imagine that we have rainbow strips with 8 distinct
  // colors, repeated often. These will overtake our bigram count, but
  // there's no point in putting all the colors in our palette (unless
  // maybe doing dithering), since we can use at most four at a time.
  // Perhaps better is to just make palettes of 3 or 4 (most occurring
  // or least-loss colors for a strip), and count those.
  CHECK(img->width % 8 == 0);
  auto BigramCount = [img, &ClosestColor]() ->
    // Most common bigrams, in descending order by frequency.
    // Key is pair of colors (c1, c2) cooccurring in an
    // 8x1 strip (not necessarily adjacent!), with c1 <= c2.
    vector<pair<pair<uint8, uint8>, int>> {
    auto Key = [](uint8 c1, uint8 c2) -> int {
      if (c1 <= c2) {
	return c1 * 64 + c2;
      } else {
	return c2 * 64 + c1;
      }
    };
    auto UnKey = [](int i) -> pair<uint8, uint8> {
      return make_pair(i / 64, i % 64);
    };

    vector<int> was_best;
    was_best.reserve(64 * 64);
    for (int i = 0; i < 64 * 64; i++) was_best.push_back(0);
    
    for (int strip = 0; strip < img->width * img->height; strip += 8) {
      // Closest color for a pixel in the strip.
      auto CC = [strip, img, &ClosestColor](int i) -> uint8 {
	const uint8 r = img->rgb[(strip + i) * 3 + 0];
	const uint8 g = img->rgb[(strip + i) * 3 + 1];
	const uint8 b = img->rgb[(strip + i) * 3 + 2];
	return ClosestColor(r, g, b);
      };

      uint8 cs[8];
      for (int i = 0; i < 8; i++)
	cs[i] = CC(i);

      // Now, insert all pairs.
      for (int i = 0; i < 7; i++) {
	for (int j = i + 1; j < 8; j++) {
	  was_best[Key(cs[i], cs[j])]++;
	}
      }
    }

    // Now find the most common bigrams.
    // PERF faster ways to do this!
    vector<pair<pair<uint8, uint8>, int>> color_count;
    color_count.reserve(64);
    for (int i = 0; i < was_best.size(); i++)
      color_count.push_back(make_pair(UnKey(i), was_best[i]));
    std::sort(color_count.begin(),
	      color_count.end(),
	      [](const pair<pair<uint8, uint8>, int> &a,
		 const pair<pair<uint8, uint8>, int> &b) {
		// Descending by count.
		return b.second < a.second;
	      });
    return color_count;
  };

  
  
  switch (method) {
  case PaletteMethod::GREYSCALE: {
    memcpy(screen->palette, greyscale_palettes, 4 * 4);
    break;
  }
  case PaletteMethod::GREEDY_BIGRAMS: {
    vector<pair<pair<uint8, uint8>, int>> bigram_count = BigramCount();

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
      
    #if 0
    for (int i = 0; i < bigram_count.size(); i++) {
      if (bigram_count[i].second) {
	printf("%02x/%02x: %d\n", bigram_count[i].first.first,
	       bigram_count[i].first.second, bigram_count[i].second);
      }
    }
    printf("Overall most common: %02x\n", besti);
    printf("\n");
    #endif

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
      #if 0
      for (int p = 0; p < 4; p++) {
	CHECK(nextp[p] <= 4);
	CHECK(nextp[p] >= 0);
	for (int i = 0; i < 4; i++) {
	  if (i < nextp[p])
	    printf("%02x ", screen->palette[p * 4 + i]);
	  else
	    printf("_ ");
	}
	printf(" | ");
      }
      printf("\n");
      #endif

      const auto &row = bigram_count[row_idx];
      if (row.second == 0) {
	#if 0
	printf("*** All bigrams done.\n");
	#endif
	break;
      }
      if (colors_left == 0) {
	#if 0
	printf("\n*** Bigrams left:\n");
	for (int i = row_idx; i < bigram_count.size(); i++) {
	  if (bigram_count[i].second) {
	    printf("* %02x/%02x: %d\n", bigram_count[i].first.first,
		   bigram_count[i].first.second, bigram_count[i].second);
	  }
	}
	#endif
	break;
      }
      
      auto PlaceSingleton = [screen, &nextp, &palettes, &colors_left](
	  uint8 c) {
	for (int p = 0; p < 4; p++) {
	  if (nextp[p] != 4) {
	    screen->palette[p * 4 + nextp[p]] = c;
	    nextp[p]++;
	    palettes[c].push_back(p);
	    if (DEBUG_CONVERT) printf("Placed singleton %02x in %d\n", c, p);
	    colors_left--;
	    return true;
	  }
	}
	return false;
      };
	
      // We don't care about the count any more, just the colors.
      const uint8 c1 = row.first.first;
      const uint8 c2 = row.first.second;
      if (DEBUG_CONVERT) printf("Next: %02x %02x\n", c1, c2);
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
	      if (DEBUG_CONVERT) printf("Friended %02x to %02x in %d\n", c2, c1, p);
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
	    if (DEBUG_CONVERT) printf("Placed new pair %02x and %02x in %d\n", c1, c2, p);
	    goto next_bigram;
	  }
	}

	// Finally, place as singletons.
	if (PlaceSingleton(c1))
	  PlaceSingleton(c2);
      }

    next_bigram:;
    }
    break;
  }
  case PaletteMethod::MOST_COMMON: {
    vector<pair<int, int>> color_count = ColorCount();
    // Put the overall most common colors in separate
    // palettes (is this a good idea??)

    #if 0
    for (int i = 0; i < 64; i++) {
      if (color_count[i].second) {
	printf("%02x: %d\n", color_count[i].first, color_count[i].second);
      }
    }
    printf("\n");
    #endif

    // XXX There are more cases like this, like when we only have 6
    // colors, etc.
    if (color_count[4].second == 0) {
      // If we have only 4 colors, make sure they're in the same
      // palette! Then we just repeat that same palette 4 times. (No
      // need for this, but it gives us a little robustness against
      // noise when transmitting attribute bits, at least.)
      for (int i = 0; i < 4; i++) {
	screen->palette[i +  0] = color_count[i].first;
	screen->palette[i +  4] = color_count[i].first;
	screen->palette[i +  8] = color_count[i].first;
	screen->palette[i + 12] = color_count[i].first;
      }
    } else if (color_count[7].second == 0) {
      // With 7 or fewer colors, fill two palettes, and then repeat but shuffled.
      int dst = 0;
      for (int src : {0, 1, 2, 3,
	    /* 0 */      4, 5, 6,
	    /* 0 */      4, 2, 3,
	    /* 0 */      1, 5, 6}) {
	screen->palette[dst] = color_count[src].first;
	dst++;
      }
    } else {
      int rank = 0;
      // 0   1  2  3
      // 4   5  6  7
      // 8   9 10 11
      // 12 13 14 15
      // for (int dest : { 0, 15, 11, 7, 3, 14, 10, 6, 2, 13, 9, 5, 1 }) {
      for (int dest : { 0, 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15 }) {
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
  printf("Dithered mode!\n");
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
	int gg = ntsc_palette[nes_color * 3 + 1];
	int bb = ntsc_palette[nes_color * 3 + 2];

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
      int nes_color = i == 0 ? screen->palette[0] :
	screen->palette[pal * 4 + i];
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

  // std::unordered_map<string, std::tuple<uint8, uint8, uint8>> memo;
  
  auto OneStrip = [&ClosestColor /* , &memo */](
      // 8x3 bytes, rgb triplets
      const uint8 *rgb) -> std::tuple<uint8, uint8, uint8> {

    /*
    string key{(const char *)rgb, 8 * 3};
    auto it = memo.find(key);
    if (it != memo.end()) {
      return it->second;
    }
    */
    
    // Try all four palettes, to minimize this total error.
    int best_totalerror = 0x7FFFFFFE;
    std::tuple<uint8, uint8, uint8> best;
    for (int pal = 0; pal < 4 /* XXX */; pal++) {
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

	lobits <<= 1; lobits |= (p & 1);
	hibits <<= 1; hibits |= ((p >> 1) & 1);
      }

      if (pal == 0 || totalerror < best_totalerror) {
	best = {(uint8)pal, lobits, hibits};
	best_totalerror = totalerror;
      }
    }

    // memo[key] = best;
    
    /*
    string key{(const char *)rgb, 8 * 3};
    auto it = memo.find(key);
    if (it != memo.end()) {
      if (best != it->second) {
	printf("Inconsistent!\n"
	       "RGB: %s\n"
	       "Old result: %02x, %02x, %02x\n"
	       "New result: %02x, %02x, %02x\n",
	       "??",
	       std::get<0>(it->second),
	       std::get<1>(it->second),
	       std::get<2>(it->second),
	       std::get<0>(best),
	       std::get<1>(best),
	       std::get<2>(best));
      }
    } else {
      memo[key] = best;
    }
    */

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

// This version is for real-time mapping of screen images, e.g.
// from the SNES emulator.
void FillScreenFast(ImageRGB *img, Screen *screen) {
  // Two-bit color index within palette #pal that matches the
  // RGB color best.
  auto ClosestColor = [&screen](int pal, int r, int g, int b) ->
    std::tuple<int, int> {
    int best_sqerr = 65536 * 3 + 1;
    int best_i = 0;
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
	best_sqerr = sqerr;
      }
    }

    // TODO: Also keep per-color error to propagate?
    return {best_i, best_sqerr};
  };

  // Any way to avoid searching all four palettes?
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
  MakePalette(PaletteMethod::GREEDY_BIGRAMS, img.get(), &rc, &screen);
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

