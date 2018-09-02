
#include "autolives.h"

#include <string>
#include <vector>
#include <unordered_map>

#include "../cc-lib/arcfour.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/fc.h"
#include "../fceulib/fceu.h"

// How to set?
static constexpr int TEST_CONTROL_FRAMES = 180;

static constexpr int TRY_TO_DIE_FRAMES = 360;

static constexpr int FINDLIVES_NUM_EXPERIMENTS = 10;

// ? Probably better with game-specific markov model?
namespace {
// Random inputs intended to demonstrate that the player
// has control, so they are fairly "jiggly."
struct InputGenerator {
  uint8 Next(ArcFour *rc) {
    uint8 b = rc->Byte();

    auto Randomize =
      [this, &b](int L, int R) {
	// Switch 1/4 of the time.
	const bool do_switch = (b & 3) == 0;
	b >>= 2;
	if (do_switch) {
	  if (last & (L | R)) {
	    // Currently going. Either swap or stop.
	    if (b & 1) {
	      if (last & L) {
		last &= ~L;
		last |= R;
	      } else {
		last &= ~R;
		last |= L;
	      }
	    } else {
	      last &= ~(L | R);
	    }
	    b >>= 1;
	  } else {
	    // Not going. Go one way or the other.
	    last |= (b & 1) ? L : R;
	    b >>= 1;
	  }
	}
      };

    Randomize(INPUT_L, INPUT_R);
    Randomize(INPUT_U, INPUT_D);

    if ((b & 3) == 0) last ^= INPUT_A;
    b >>= 2;
    if ((b & 3) == 0) last ^= INPUT_B;
    b >>= 2;

    // Never generate select or start, since these often have
    // special meaning.
    return last;
  }
 private:
  uint8 last = INPUT_A;
};
}

AutoLives::AutoLives(
    const string &game,
    NMarkovController nmarkov) : rc(game),
				 nmarkov(std::move(nmarkov)) {
  emu.reset(Emulator::Create(game));
  lemu.reset(Emulator::Create(game));
  remu.reset(Emulator::Create(game));
  memu.reset(Emulator::Create(game));
  CHECK(emu.get() != nullptr) << game;
  CHECK(lemu.get() != nullptr) << game;
  CHECK(remu.get() != nullptr) << game;
  CHECK(memu.get() != nullptr) << game;
}

AutoLives::~AutoLives() {}

// PERF it would be possible to do both players at once
// without emulating twice. (As one simple example, the
// no-buttons emu does the same thing either way.)
float AutoLives::IsInControl(const vector<uint8> &save,
			     int xloc, int yloc,
			     bool player_two) {

  // Initialize them all to the same state.
  emu->LoadUncompressed(save);
  lemu->LoadUncompressed(save);
  remu->LoadUncompressed(save);
  memu->LoadUncompressed(save);

  auto MakePlayer = [player_two](uint8 inputs) {
    return player_two ? ((uint16)inputs << 8) : (uint16)inputs;
  };
  
  InputGenerator gen;
  int success = 0;
  for (int i = 0; i < TEST_CONTROL_FRAMES; i++) {
    // emu gets no inputs
    emu->Step16(0U);
    // lemu holds left,
    lemu->Step16(MakePlayer(INPUT_L));
    // remu holds right,
    remu->Step16(MakePlayer(INPUT_R));
    // mash emu gets random inputs
    const uint8 input = gen.Next(&rc);
    memu->Step16(MakePlayer(input));
    // XXX use nmarkov now that we have it?
    
    // Now, has xloc changed? We're not looking for change relative to
    // the start state (the player may be moving while a death
    // animation plays, for example), but rather whether our inputs
    // are having any effect on the progression of the state. So,
    // increment the score whenever any pair of the above emulators
    // disagree on theplayer's location.

    // TODO: Sometimes "moving" the player just means moving the
    // scroll, so we could consider adding the x and y scroll to the
    // player location before comparing.
    
    // TODO: Instead of just using inequality, we could consider the
    // distance traveled.
    const uint8 *ram = emu->GetFC()->fceu->RAM;
    const uint8 *lram = lemu->GetFC()->fceu->RAM;
    const uint8 *rram = remu->GetFC()->fceu->RAM;
    const uint8 *mram = memu->GetFC()->fceu->RAM;
    if (!(ram[xloc] == lram[xloc] &&
	  ram[xloc] == rram[xloc] &&
	  ram[xloc] == mram[xloc]) ||
	!(ram[yloc] == lram[yloc] &&
	  ram[yloc] == rram[yloc] &&
	  ram[yloc] == mram[yloc])) {
      success++;
    }
  }

  return success / (float)TEST_CONTROL_FRAMES;
}

namespace {
// Example frame right before a memory location is decremented,
// paired with the inputs so that we can repeat exactly.
struct Frame {
  uint16 inputs = 0;
  std::vector<uint8> save;
  Frame(uint16 inputs, vector<uint8> save) : inputs(inputs),
					     save(std::move(save)) {}
};

// Information about a memory location from random play, below.
struct Info {
  // Number of times it was the same.
  int same = 0;
  // Times incremented or decremented.
  int incremented = 0;
  int decremented = 0;
  // If random play causes the memory location to change by
  // a large magnitude on any frame, it is disqualified and
  // removed from the map, so no need to store it here.

  // Example frames (save states) that cause decrements.
  vector<Frame> decremented_saves;
};
}

// True if decremented by more than 0 but no more than maxdist.
// Treated modularly.
static bool Decremented(uint8 maxdist, uint8 prev, uint8 now) {
  if (now == prev) return false;
  else if (now < prev) {
    // Normal.
    return (prev - now) <= maxdist;
  } else {
    // For example (now = 254, prev = 1).
    return ((int)prev + 256 - now) <= maxdist;
  }
}

static constexpr int MAX_DECREMENT = 1;

// PERF: Allow passing a whitelist or blacklist of locations
// to consider.
void AutoLives::FindLives(const vector<uint8> &save,
			  bool player_two) {

  auto MakePlayer = [player_two](uint8 inputs) {
    return player_two ? ((uint16)inputs << 8) : (uint16)inputs;
  };
  
  // All candidate memory locations. Absence is treated as
  // having been disqualified, so we start by adding every
  // location.
  std::unordered_map<int, Info> locs;
  for (int i = 0; i < 2048; i++) locs[i] = Info();

  // We'll run a number of experiments starting from this
  // same save spot. We're trying to disqualify locations
  // (for changing too often) and, especially, to find
  // a frame where we die.

  // Share this across experiments to increase entropy.
  NMarkovController::History nhist = nmarkov.HistoryInDomain();
  for (int expt = 0; expt < FINDLIVES_NUM_EXPERIMENTS; expt++) {
    // Start back at the beginning.
    emu->LoadUncompressed(save);

    printf("[%d/%d] Starting experiment with %d locs left.\n",
	   expt, FINDLIVES_NUM_EXPERIMENTS, (int)locs.size());
    
    vector<uint8> mem_prev = emu->GetMemory();
    for (int f = 0; f < TRY_TO_DIE_FRAMES; f++) {
      // No candidates left.
      if (locs.empty())
	break;

      
      const uint8 input = nmarkov.RandomNext(nhist, &rc);
      nhist = nmarkov.Push(nhist, input);
      const uint16 input16 = MakePlayer(input);
      emu->Step16(input16);
      vector<uint8> mem_now = emu->GetMemory();

      // Now, for any memory location that's still a candidate...
      for (auto it = locs.begin(); it != locs.end(); /* in loop */) {
	const int loc = it->first;
	Info *info = &it->second;
	const uint8 prev = mem_prev[loc];
	const uint8 now = mem_now[loc];
	if (prev == now) {
	  info->same++;
	} else if (Decremented(MAX_DECREMENT, prev, now)) {
	  info->decremented++;
	  info->decremented_saves.emplace_back(input16,
					       emu->SaveUncompressed());
	} else if (Decremented(MAX_DECREMENT, now, prev)) {
	  info->incremented++;
	} else {
	  // Change is too big. Eliminate it.
	  auto next = it; ++next;
	  locs.erase(it);
	  printf("[%d/%d] Disqualified %04x whose value was "
		 "%02x -> %02x. (%d left)\n",
		 expt, FINDLIVES_NUM_EXPERIMENTS,
		 loc, prev, now, (int)locs.size());
	  it = next;
	  continue;
	}
	++it;
      }

      mem_prev = std::move(mem_now);
    }
  }

  printf("After experiments, %d locations remain. These are all the same:\n",
	 (int)locs.size());
  for (auto it = locs.begin(); it != locs.end(); /* in loop */) {
    const Info &info = it->second;
    if (info.incremented == 0 && info.decremented == 0) {
      printf("%04x, ", it->first);
      auto next = it; ++next;
      locs.erase(it);
      it = next;
      continue;
    }
    ++it;
  }
  printf("\nAnd the rest (%d):\n", (int)locs.size());
  for (auto it = locs.begin(); it != locs.end(); ++it) {
    const Info &info = it->second;
    printf("%04x %d= %d+ %d-\n", it->first,
	   info.same, info.incremented, info.decremented);
  }
}
