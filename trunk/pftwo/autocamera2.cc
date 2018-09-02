// TODO: Give higher scores to linkages when the x/y memory locations
// are close together.
// TODO: Penalize linkages in the typical OAMDMA src region.

#include "autocamera2.h"

#include <vector>
#include <utility>
#include <functional>

#include <string.h>
#include <stdlib.h>

#include "../fceulib/ppu.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/hashing.h"
#include "../cc-lib/gtl/top_n.h"
#include "../fceulib/simplefm2.h"

static constexpr int NUM_SPRITES = 64;
// Absolute difference allowed between memory location and sprite
// location along one dimension. 0 would mean exact match, 1 means
// the three pixels: exact, 1 pixel left, 1 pixel right.
static constexpr int LINKAGE_MARGIN = 2;
static constexpr int MIN_RANDOM_DISTANCE = 8;
static_assert(MIN_RANDOM_DISTANCE > (LINKAGE_MARGIN >> 1),
	      "precondition");
// When moving a sprite by changing its memory location, we allow
// the sprite's vector to differ by up to this amount in each component.
static constexpr int DELTA_SLOP = 3;

static constexpr int NUM_LINKAGES = 32;

using Linkage = AutoCamera2::Linkage;
using XLoc = AutoCamera2::XLoc;

namespace {
struct OAM {
  // Copies from emu, so emu can be modified after construction.
  OAM(Emulator *emu) : mem(emu->GetFC()->ppu->SPRAM,
			   emu->GetFC()->ppu->SPRAM + 256) {}
  inline uint8 X(int sprite_idx) const { return mem[sprite_idx * 4 + 3]; }
  inline uint8 Y(int sprite_idx) const { return mem[sprite_idx * 4 + 0]; }
  inline uint8 Byte(int byte_idx) const { return mem[byte_idx]; }

  bool Equals(const OAM &other) const {
    return 0 == memcmp(mem.data(), other.mem.data(), 256);
  }
  
private:
  // Copy of OAM region.
  const vector<uint8> mem;
};

// Set of eight-bit integers.
struct EightSet {
  EightSet() : bits(256, false) {}
  inline void Insert(uint8 idx) {
    bits[idx] = true;
  }
  inline bool Contains(uint8 idx) const {
    return bits[idx];
  }
  inline void InsertRegion(uint8 idx, uint8 margin) {
    Insert(idx);
    int x = idx;
    for (uint8 off = 0; off <= margin; off++) {
      int xx = x - off;
      if (xx >= 0) Insert(xx);
      int xxx = x + off;
      if (xxx < 256) Insert(xxx);
    }
  }
  
private:
  vector<bool> bits;
};

struct BetterLinkage {
  bool operator() (const Linkage &a, const Linkage &b) const {
    return a.score > b.score;
  }
};

struct BetterXLoc {
  bool operator() (const XLoc &a, const XLoc &b) const {
    return a.score > b.score;
  }
};
}

using ScoredLocationMap = 
  std::unordered_map<pair<int, int>, float,
		     Hashing<pair<int, int>>>;


static vector<Linkage> GetBestLinkages(
    const ScoredLocationMap &scores) {
  gtl::TopN<Linkage, BetterLinkage> topn(NUM_LINKAGES);
  for (const auto &p : scores) {
    topn.push(Linkage(p.first.first, p.first.second, p.second));
  }
  std::unique_ptr<vector<Linkage>> best(topn.Extract());
  return std::move(*best);
}

AutoCamera2::AutoCamera2(const string &game) : rc(game) {
  emu.reset(Emulator::Create(game));
  lemu.reset(Emulator::Create(game));
  remu.reset(Emulator::Create(game));
  CHECK(emu.get() != nullptr) << game;
  CHECK(lemu.get() != nullptr) << game;
  CHECK(remu.get() != nullptr) << game;
}

AutoCamera2::~AutoCamera2() {}

// Looks like we generally need two frames between modifying memory
// and seeing it reflected in OAM. (With one frame, we generally just
// find the OAMDMA region in RAM.)
static void StepEmu(Emulator *emu) {
  emu->StepFull(0U, 0U);
  emu->StepFull(0U, 0U);
}

vector<Linkage> AutoCamera2::FindLinkages(
    const vector<uint8> &save,
    const std::function<void(string)> &report) {
  // This is parallelizable, but probably simpler to organize the
  // parallelism across samples rather than within one.
  // (But should we take emu as an argument, then?)
  emu->LoadUncompressed(save);

  vector<uint8> orig_mem = emu->GetMemory();
  StepEmu(emu.get());
  OAM orig_oam{emu.get()};

  // This is the scroll at the end of the frame.
  const uint32 orig_xscroll = emu->GetXScroll();
  const uint32 orig_yscroll = emu->GetYScroll();
  (void)orig_xscroll; (void)orig_yscroll;

  const PPU *ppu = emu->GetFC()->ppu;
  
  // First, create a set of coordinates that could correspond to
  // a sprite. x and y separate. This allows us to quickly filter
  // memory locations.
  EightSet xset, yset;
  for (int i = 0; i < NUM_SPRITES; i++) {
    const uint8 sprite_y = orig_oam.Y(i);
    xset.InsertRegion(orig_oam.X(i), LINKAGE_MARGIN);
    yset.InsertRegion(sprite_y, LINKAGE_MARGIN);
    // Some games (Contra) store the actual screen coordinate but
    // others (Mario) store the value in absolute level coordinates
    // (i.e., prior to scrolling). If the sprite's location is the
    // memory location offset by the PPU scroll position, also accept
    // this as a potential linkage. (TODO: We should report this
    // back in the interface, so that uses of the location can
    // also compensate.)

    // Use the scroll position from the scanline that the sprite was
    // rendered on.
    uint8 xscroll = ppu->interframe_x[sprite_y];
    uint8 yscroll = ppu->interframe_x[sprite_y];
    
    // xset.InsertRegion(orig_oam.X(i) - orig_xscroll, LINKAGE_MARGIN);
    // yset.InsertRegion(orig_oam.Y(i) - orig_yscroll, LINKAGE_MARGIN);
    xset.InsertRegion(orig_oam.X(i) - xscroll, LINKAGE_MARGIN);
    yset.InsertRegion(orig_oam.Y(i) - yscroll, LINKAGE_MARGIN);
  }

  // Any memory location whose value is close to a sprite's coordinate.
  vector<int> xcand, ycand;
  for (int i = 0; i < orig_mem.size(); i++) {
    uint8 val = orig_mem[i];
    if (xset.Contains(val)) xcand.push_back(i);
    if (yset.Contains(val)) ycand.push_back(i);
  }

  if (report)
    report(StringPrintf("%lld x potential locations and %lld y",
			xcand.size(), ycand.size()));

  // get new x,y that are not close to oldx,oldy
  // XXX also newx should not be close to newy, and dx should not
  // be close to dy
  // The value must also be between low and high, inclusive. Make
  // sure this interval is wide enough, or it may not terminate!
  auto GetNewCoord =
    [this](uint8 old, uint8 low, uint8 high) {
      for (;;) {
	const uint8 z = rc.Byte();
	if (z >= low && z <= high) {
	  if (z < old) {
	    if (old - z >= MIN_RANDOM_DISTANCE) return z;
	  } else if (old < z) {
	    if (z - old >= MIN_RANDOM_DISTANCE) return z;
	  }
	}
      }
    };

  // Keep away from the edges of the screen. For y coordinate,
  // don't draw outside the visible scanlines, either.
  static constexpr int X_LOW = 9, X_HIGH = 247;
  static constexpr int Y_LOW = 9, Y_HIGH = 232;
  
  // Filter xcands and ycands that don't affect any sprite.
  int filtered = 0;
  {
    // Memoize, since there can be overlap between x and y candidates.
    std::unordered_map<int, bool> coord_memo;
    vector<int> xcand_new, ycand_new;
    auto NoEffect =
      [this, &coord_memo, &save, &orig_mem, &orig_oam, &GetNewCoord](
	  int loc, uint8 low, uint8 high) {
	auto it = coord_memo.find(loc);
	if (it != coord_memo.end()) return it->second;
	emu->LoadUncompressed(save);
	// Mem is orig_mem at this point.
	const uint8 oldv = orig_mem[loc];
	const uint8 newv = GetNewCoord(oldv, low, high);

	uint8 *ram = emu->GetFC()->fceu->RAM;
	ram[loc] = newv;
	StepEmu(emu.get());
	OAM new_oam{emu.get()};
	const bool no_effect = new_oam.Equals(orig_oam);
	coord_memo[loc] = no_effect;
	return no_effect;
      };
    for (int i : xcand) {
      if (NoEffect(i, X_LOW, X_HIGH)) {
	filtered++;
      } else {
	xcand_new.push_back(i);
      }
    }
    for (int i : ycand) {
      if (NoEffect(i, Y_LOW, Y_HIGH)) {
	filtered++;
      } else {
	ycand_new.push_back(i);
      }
    }
    xcand = std::move(xcand_new);
    ycand = std::move(ycand_new);
  }

  if (report)
    report(StringPrintf(
	       "[filtered] %lld x potential locations and %lld y",
	       xcand.size(), ycand.size()));

  ScoredLocationMap scores;

  int tried = 0, rejected_all_equal = 0;
  for (int xc : xcand) {
    for (int yc : ycand) {
      // The memory locations must be distinct!
      if (xc == yc)
	continue;

      tried++;

      // XXX: Actually, should check that the *same sprites* are moving
      // on each iteration, right?
      for (int iters = 6; iters--;) {
	// We'll restore before frame, then modify the candidate memory
	// locations, then execute a frame.
	emu->LoadUncompressed(save);
	// Mem is orig_mem at this point.
	const uint8 oldx = orig_mem[xc], oldy = orig_mem[yc];
	const uint8 newx = GetNewCoord(oldx, X_LOW, X_HIGH);
	const uint8 newy = GetNewCoord(oldy, Y_LOW, Y_HIGH);

	uint8 *ram = emu->GetFC()->fceu->RAM;
	ram[xc] = newx;
	ram[yc] = newy;
	StepEmu(emu.get());
	OAM new_oam{emu.get()};

	const int dx = (int)newx - (int)oldx, dy = (int)newy - (int)oldy;
	// Otherwise, for each changed sprite, compute the delta.
	// TODO: Since warping might update the scroll, we should
	// be including changes to (scanline-specific) scroll positions
	// here.
	// TODO: Consider wraparound (unsigned distance?) here, since
	// x usually will overflow from 255 to 0, for example. This
	// can be mid-screen when scroll is nonzero.
	for (int s = 0; s < 64; s++) {
	  const int dsx = (int)new_oam.X(s) - (int)orig_oam.X(s);
	  const int dsy = (int)new_oam.Y(s) - (int)orig_oam.Y(s);
	  if (abs(dsx - dx) <= DELTA_SLOP &&
	      abs(dsy - dy) <= DELTA_SLOP) {
	    // Penalize when not exactly equal?
	    scores[make_pair(xc, yc)] += 1.0f;
	  }
	}
      }
    }
  }

  const int working_pairs = (int)scores.size();

  vector<Linkage> best = GetBestLinkages(scores);

  (void)tried;
  (void)filtered;
  (void)rejected_all_equal;
  (void)working_pairs;
  
  return best;
}

vector<Linkage> AutoCamera2::MergeLinkages(
    const vector<vector<Linkage>> &samples) {
  ScoredLocationMap scores;
  for (const vector<Linkage> &linkages : samples)
    for (const Linkage &linkage : linkages)
      scores[make_pair(linkage.xloc, linkage.yloc)] += linkage.score;
  return GetBestLinkages(scores);
}


// Wait this many frames pressing nothing in order to remove velocity.
static constexpr int COOLDOWN_FRAMES = 6;
static constexpr int TEST_FRAMES = 20;
// Might want to also update the switch below in ModularLess.
static constexpr int MAX_DIST_PER_FRAME = 5;

// True if the value 'now' is strictly lower than 'prev'. Writes a
// score in [0, 1] to the score argument. Lower is treated modularly,
// and must be no more than the supplied maximum distance.
static bool ModularLess(uint8 maxdist, uint8 prev, uint8 now,
			float *score) {
  *score = 0.0f;
  auto D =
    [maxdist, score](int d) {
      if (d < maxdist) {
	switch (d) {
	case 1: *score = 1.0f; break;
	case 2: *score = 0.8f; break;
	case 3: *score = 0.5f; break;
	case 4: *score = 0.25f; break;
	default:
	  *score = 0.10f;
	  break;
	}
	return true;
      } else {
	return false;
      }
    };
  if (now == prev) return false;
  else if (now < prev) {
    // Normal.
    return D(prev - now);
  } else {
    // For example (now = 254, prev = 1).
    return D((int)prev + 256 - now);
  }
}

static inline bool ModularGreater(uint8 maxdist, uint8 prev, uint8 now,
				  float *score) {
  return ModularLess(maxdist, now, prev, score);
}


static vector<XLoc> GetBestXLocs(const vector<float> &scores) {
  gtl::TopN<XLoc, BetterXLoc> topn(16);
  for (int idx = 0; idx < 2048; idx++) {
    if (scores[idx] > 0.0f) {
      topn.push(XLoc(idx, scores[idx]));
    }
  }
  std::unique_ptr<vector<XLoc>> best(topn.Extract());
  return std::move(*best);
}

vector<XLoc> AutoCamera2::MergeXLocs(const vector<vector<XLoc>> &xlocs) {
  vector<float> scores(2048, 0.0f);
  for (const vector<XLoc> &v : xlocs)
    for (const XLoc &x : v)
      scores[x.xloc] += x.score;
  return GetBestXLocs(scores);
}

vector<XLoc> AutoCamera2::FindXLocs(
    const vector<uint8> &save,
    bool player_two,
    const std::function<void(string)> &report) {

  auto MakePlayer = [player_two](uint8 inputs) {
    return player_two ? ((uint16)inputs << 8) : (uint16)inputs;
  };
  
  // Basically follows the autocamera approach of splitting the world
  // into hold-left, hold-nothing, and hold-right. But:
  //  - We look for memory locations, not sprites
  //  - We specifically look for small per-frame increments
  //  - We treat coordinates modularly, so that e.g. 1 is considered
  //    slightly to the right of 255.

  // Original position.
  emu->LoadUncompressed(save);
  for (int i = 0; i < COOLDOWN_FRAMES; i++) {
    emu->StepFull(0, 0);
  }

  vector<uint8> cooled = emu->SaveUncompressed();
  lemu->LoadUncompressed(cooled);
  remu->LoadUncompressed(cooled);
  
  vector<uint8> orig_mem = emu->GetMemory();
  vector<uint8> lmem = orig_mem;
  vector<uint8> rmem = orig_mem;

  vector<float> scores(2048, 0.0f);

  for (int i = 0; i < TEST_FRAMES; i++) {
    emu->StepFull16(0U);
    lemu->StepFull16(MakePlayer(INPUT_L));
    remu->StepFull16(MakePlayer(INPUT_R));

    vector<uint8> now_nmem = emu->GetMemory();
    vector<uint8> now_lmem = lemu->GetMemory();
    vector<uint8> now_rmem = remu->GetMemory();
    for (int idx = 0; idx < 2048; idx++) {
      // Only consider the memory location if it didn't change in the
      // original.
      if (now_nmem[idx] == orig_mem[idx]) {
	float lscore = 0.0f, rscore = 0.0f;
	bool lok = ModularLess(MAX_DIST_PER_FRAME, lmem[idx], now_lmem[idx],
			       &lscore);
	bool rok = ModularGreater(MAX_DIST_PER_FRAME,
				  rmem[idx], now_rmem[idx],
				  &rscore);
	float score = lscore + rscore;
	// Penalize if we didn't see both move.
	if (!lok || !rok) score *= 0.5f;
	scores[idx] += score;
      }
    }
    // Shift memories. No need to shift nmem, since we always test
    // equality on it.
    lmem.swap(now_lmem);
    rmem.swap(now_rmem);
  }
  
  return GetBestXLocs(scores);
}
