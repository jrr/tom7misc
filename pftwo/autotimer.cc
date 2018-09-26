#include "autotimer.h"

#include <math.h>
#include <algorithm>
#include <string>
#include <vector>
#include <unordered_map>

#include "../cc-lib/arcfour.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/fc.h"
#include "../fceulib/fceu.h"
#include "autoutil.h"

static constexpr bool VERBOSE = false;

AutoTimer::AutoTimer(
    const string &game,
    NMarkovController nmarkov) : random_pool(game),
				 emulator_pool(game, 4),
				 nmarkov(std::move(nmarkov)) {
}

AutoTimer::~AutoTimer() {}


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

namespace {
// A candidate timer, once we have seen any decrement.
struct TimerInfo {
  // If true, then we've already eliminated this location (for example
  // because it is not incrementing or decrementing consistently.)
  bool disqualified = false;
  // Frame offset at which it last changed. Default is -1,
  // meaning we haven't seen a change yet.
  int last_change = -1;

  bool incrementing = false;
  
  // These are only meaningful once we've observed a second
  // change.
  float average_delta = 0.0f;
  int num_deltas = 0;
};
}

// To find timers, we just execute some frames. We're looking for the
// location to be decremented (or incremented) at least 3 times,
// because we are likely in the midst of an interval already (so we
// can't measure its length) and then want to see at least two
// intervals of the same length! Since timers are usually one-per-second
// at the slowest, four seconds should suffice.
//
// There's danger in this running for two long, because if we die, it
// often stops timers.
static constexpr int EXPERIMENT_FRAMES = 4 * 60;

vector<AutoTimer::TimerLoc> AutoTimer::FindTimers(const vector<uint8> &save) {
  Emulator *emu = emulator_pool.Acquire();
  // ArcFour *rc = random_pool.Acquire();

  // A location only becomes a candidate once it changes.
  std::unordered_map<int, TimerInfo> candidates;

  // Start back at the beginning.
  emu->LoadUncompressed(save);
  vector<uint8> mem_prev = emu->GetMemory();
  for (int f = 0; f < EXPERIMENT_FRAMES; f++) {
    // The "safest" thing is often just to stay still, so that's what
    // we do. TODO: It would also be pretty reasonable to compare what
    // happens in the training movie if we have one.
    emu->Step16(0);
    vector<uint8> mem_now = emu->GetMemory();

    for (int i = 0; i < 2048; i++) {
      if (mem_now[i] != mem_prev[i]) {
	TimerInfo *info = &candidates[i];
	if (info->disqualified) continue;

	if (VERBOSE)
	  printf("%04x @%d %02x -> %02x",
		 i, f, mem_prev[i], mem_now[i]);
	
	bool incrementing = false;
	if (Decremented(1, mem_prev[i], mem_now[i])) {
	  incrementing = false;
	} else if (Decremented(1, mem_now[i], mem_prev[i])) {
	  incrementing = true;
	} else {
	  info->disqualified = true;
	  if (VERBOSE)
	    printf("  not inc/dec. DQ.\n");
	  continue;
	}

	// First change?
	if (info->last_change == -1) {
	  info->last_change = f;
	  info->incrementing = incrementing;

	  if (VERBOSE)
	    printf("  ok first %c\n",
		   incrementing ? '+' : '-');
	  continue;
	} else {
	  if (info->incrementing != incrementing) {
	    // Change in direction not allowed.
	    info->disqualified = true;
	    if (VERBOSE)
	      printf("  was %c now %c. DQ.\n",
		     info->incrementing ? '+' : '-',
		     incrementing ? '+' : '-');
	    continue;
	  }
	  
	  const int delta = f - info->last_change;
	  info->last_change = f;
	  // Second change? Then this will always succeed.
	  if (info->num_deltas == 0) {
	    info->num_deltas = 1;
	    info->average_delta = (float)delta;
	    if (VERBOSE)
	      printf("  ok %c delta %d.\n",
		     incrementing ? '+' : '-',
		     delta);
	    continue;
	  } else {
	    if (delta < info->average_delta - 1.01f ||
		delta > info->average_delta + 1.01f) {
	      // We allow one frame of jitter, but here the delta was
	      // not within that range.
	      info->disqualified = true;
	      if (VERBOSE)
		printf("  delta %.3f now %d. DQ\n",
		       info->average_delta,
		       delta);
	      continue;
	    } else {
	      info->average_delta =
		((info->average_delta * info->num_deltas) + (float)delta) /
		(info->num_deltas + 1);
	      info->num_deltas++;
	      if (VERBOSE)
		printf(" ok x %d now %.3f\n", info->num_deltas,
		       info->average_delta);
	    }
	  }
	}
      }
    }
    mem_prev = std::move(mem_now);
  }

  // Now that we've reached the end of the experiment, disqualify or
  // downweight anything that hasn't decremented but should have!
  for (auto &p : candidates) {
    TimerInfo *info = &p.second;
    if (info->disqualified) continue;
    if (info->last_change == -1 || info->num_deltas == 0) {
      // Didn't see enough changes to establish a repeating pattern.
      // We eliminate it, although note that it could just be a
      // really slow counter.
      info->disqualified = true;
      if (VERBOSE)
	printf("%04x  not enough changes. DQ\n", p.first);
      continue;
    }
    const int last_delta = EXPERIMENT_FRAMES - info->last_change;
    if (last_delta > info->average_delta + 2.01f) {
      info->disqualified = true;
      if (VERBOSE)
	printf("%04x  last delta %d (want %.3f). DQ\n", p.first,
	       last_delta, info->average_delta);
      continue;
    }
  }
  
  // TODO: Could make more passes here, starting from the beginning.
  // Would want a different state other than last_change = -1 meaning
  // like "I know what the delta and direction should be, but I don't
  // know where I am within an interval."

  vector<TimerLoc> timers;
  // Now, get the best scoring candidates.
  for (const auto &p : candidates) {
    const TimerInfo *info = &p.second;
    if (info->disqualified ||
	info->last_change == -1 ||
	info->num_deltas < 2)
      continue;

    TimerLoc timer;
    timer.loc = p.first;
    timer.period = info->average_delta;
    timer.incrementing = info->incrementing;
    
    // Not that many notions of score here in a single pass..
    
    // How far are we from an integer? A number in [0, 0.5) with 0
    // being the most integral.
    const float fpart =
      fabsf(info->average_delta - roundf(info->average_delta));

    // Since very fast timers get lots of repetitions, we shouldn't
    // let that affect the score that much.
    timer.score = std::min(info->num_deltas, 3) * 0.5f + fpart;
    
    timers.push_back(timer);
  }

  if (VERBOSE) {
    printf("%d Remaining:\n", (int)timers.size());
    for (const TimerLoc &t : timers) {
      printf("%04x %c @%.3f: %.3f\n",
	     t.loc, (t.incrementing ? '+' : '-'), t.period, t.score);
    }
  }
  
  emulator_pool.Release(emu);
  return timers;
}

// static
vector<AutoTimer::TimerLoc> AutoTimer::MergeTimers(
    const vector<vector<TimerLoc>> &lv) {
  return MergeAndBest<AutoTimer::TimerLoc>(lv, 0.0f, 16);
}
