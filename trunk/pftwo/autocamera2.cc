#include "autocamera2.h"

#include <vector>
#include <utility>

#include <string.h>
#include <stdlib.h>

#include "../fceulib/ppu.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/hashing.h"
#include "../cc-lib/gtl/top_n.h"

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

}

AutoCamera2::AutoCamera2(const string &game) {
  emu.reset(Emulator::Create(game));
  CHECK(emu.get() != nullptr) << game;
}

AutoCamera2::~AutoCamera2() {}

// Looks like we generally need two frames between modifying memory
// and seeing it reflected in OAM. (With one frame, we generally just
// find the OAMDMA region in RAM.)
static void StepEmu(Emulator *emu) {
  emu->StepFull(0U, 0U);
  emu->StepFull(0U, 0U);
}

void AutoCamera2::FindLinkages(const vector<uint8> &save) {
  // This is parallelizable, but probably simpler to organize the
  // parallelism across samples rather than within one.
  // (But should we take emu as an argument, then?)
  emu->LoadUncompressed(save);

  // Generate a different random stream for each sample.
  ArcFour rc{save};
  
  vector<uint8> orig_mem = emu->GetMemory();
  StepEmu(emu.get());
  OAM orig_oam{emu.get()};

  // First, create a set of coordinates that could correspond to
  // a sprite. x and y separate. This allows us to quickly filter
  // memory locations.
  EightSet xset, yset;
  for (int i = 0; i < NUM_SPRITES; i++) {
    xset.InsertRegion(orig_oam.X(i), LINKAGE_MARGIN);
    yset.InsertRegion(orig_oam.Y(i), LINKAGE_MARGIN);
  }

  // Any memory location whose value is close to a sprite's coordinate.
  vector<int> xcand, ycand;
  for (int i = 0; i < orig_mem.size(); i++) {
    uint8 val = orig_mem[i];
    if (xset.Contains(val)) xcand.push_back(i);
    if (yset.Contains(val)) ycand.push_back(i);
  }

  printf("%lld x potential locations and %lld y\n",
	 xcand.size(), ycand.size());

  // get new x,y that are not close to oldx,oldy
  // XXX also newx should not be close to newy, and dx should not
  // be close to dy
  auto GetNewCoord =
    [&rc](uint8 old) {
      for (;;) {
	const uint8 z = rc.Byte();
	if (z < old) {
	  if (old - z >= MIN_RANDOM_DISTANCE) return z;
	} else if (old < z) {
	  if (z - old >= MIN_RANDOM_DISTANCE) return z;
	}
      }
    };

  // Filter xcands and ycands that don't affect any sprite.
  int filtered = 0;
  {
    // Memoize, since there can be overlap between x and y candidates.
    std::unordered_map<int, bool> coord_memo;
    vector<int> xcand_new, ycand_new;
    auto NoEffect =
      [this, &coord_memo, &save, &orig_mem, &orig_oam, &GetNewCoord](int loc) {
	auto it = coord_memo.find(loc);
	if (it != coord_memo.end()) return it->second;
	emu->LoadUncompressed(save);
	// Mem is orig_mem at this point.
	const uint8 oldv = orig_mem[loc];
	const uint8 newv = GetNewCoord(oldv);

	uint8 *ram = emu->GetFC()->fceu->RAM;
	ram[loc] = newv;
	StepEmu(emu.get());
	OAM new_oam{emu.get()};
	const bool no_effect = new_oam.Equals(orig_oam);
	coord_memo[loc] = no_effect;
	return no_effect;
      };
    for (int i : xcand) {
      if (NoEffect(i)) {
	filtered++;
      } else {
	xcand_new.push_back(i);
      }
    }
    for (int i : ycand) {
      if (NoEffect(i)) {
	filtered++;
      } else {
	ycand_new.push_back(i);
      }
    }
    xcand = std::move(xcand_new);
    ycand = std::move(ycand_new);
  }

  printf("[filtered] %lld x potential locations and %lld y\n",
	 xcand.size(), ycand.size());

  std::unordered_map<pair<int, int>, float,
		     Hashing<pair<int, int>>> scores;

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
	const uint8 newx = GetNewCoord(oldx), newy = GetNewCoord(oldy);

	uint8 *ram = emu->GetFC()->fceu->RAM;
	ram[xc] = newx;
	ram[yc] = newy;
	StepEmu(emu.get());
	OAM new_oam{emu.get()};

	const int dx = (int)newx - (int)oldx, dy = (int)newy - (int)oldy;
	// Otherwise, for each changed sprite, compute the delta.
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

  using P = pair<pair<int, int>, float>;
  
  struct BetterPair {
    bool operator() (const P &a, const P &b) const {
      return a.second > b.second;
    }
  };

  const int working_pairs = (int)scores.size();
  gtl::TopN<P, BetterPair> topn(16);
  for (const auto &p : scores) {
    topn.push(make_pair(p.first, p.second));
  }
  std::unique_ptr<vector<P>> best(topn.Extract());
  
  printf("%d filtered, %d pairs tried, %d all equal, "
	 "%d nonzero score\nBest:\n",
	 filtered, tried, rejected_all_equal, working_pairs);
  for (const P &p : *best) {
    printf("  %.1f:  %d,%d\n", p.second, p.first.first, p.first.second);
  }
}
