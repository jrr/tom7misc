// Ideas:
// - Contra finds the same top value 0xFF for both players; this
//   should result in discounting it.
// - TODO: Filter out locations detected by autotimer.

#include "autolives.h"

#include <math.h>
#include <algorithm>
#include <string>
#include <vector>
#include <unordered_map>

#include "../cc-lib/arcfour.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/fc.h"
#include "../fceulib/fceu.h"

// How to set?
static constexpr int TEST_CONTROL_FRAMES = 3 * 60;

static constexpr int TRY_TO_DIE_FRAMES = 6 * 60;

static constexpr int FINDLIVES_NUM_EXPERIMENTS = 10;

static constexpr bool VERBOSE = true;

AutoLives::AutoLives(
    const string &game,
    NMarkovController nmarkov) : random_pool(game),
				 emulator_pool(game, 4),
				 nmarkov(std::move(nmarkov)) {
}

AutoLives::~AutoLives() {}

// PERF it would be possible to do both players at once
// without emulating twice. (As one simple example, the
// no-buttons emu does the same thing either way.)
float AutoLives::IsInControl(const vector<uint8> &save,
			     int xloc, int yloc,
			     bool player_two) {

  Emulator *emu = emulator_pool.Acquire();
  Emulator *lemu = emulator_pool.Acquire();
  Emulator *remu = emulator_pool.Acquire();
  Emulator *memu = emulator_pool.Acquire();
  ArcFour *rc = random_pool.Acquire();
  
  // Initialize them all to the same state.
  emu->LoadUncompressed(save);
  lemu->LoadUncompressed(save);
  remu->LoadUncompressed(save);
  memu->LoadUncompressed(save);

  auto MakePlayer = [player_two](uint8 inputs) {
    return player_two ? ((uint16)inputs << 8) : (uint16)inputs;
  };

  // XXX: Maybe should discard inputs for a while?
  // (And again below?)
  NMarkovController::History nhist = nmarkov.HistoryInDomain();
  int success = 0;
  for (int i = 0; i < TEST_CONTROL_FRAMES; i++) {
    // emu gets no inputs
    emu->Step16(0U);
    // lemu holds left,
    lemu->Step16(MakePlayer(INPUT_L));
    // remu holds right,
    remu->Step16(MakePlayer(INPUT_R));
    // mash emu gets random inputs
    const uint8 input = nmarkov.RandomNext(nhist, rc);
    nhist = nmarkov.Push(nhist, input);
    memu->Step16(MakePlayer(input));
    
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

  random_pool.Release(rc);
  emulator_pool.Release(emu);
  emulator_pool.Release(lemu);
  emulator_pool.Release(remu);
  emulator_pool.Release(memu);
  return success / (float)TEST_CONTROL_FRAMES;
}

namespace {
// Example frame right before a memory location is decremented,
// paired with the inputs so that we can repeat exactly.
struct Frame {
  uint16 inputs = 0;
  std::vector<uint8> save;
  uint8 value_before = 0, value_after = 0;
  Frame(uint16 inputs, vector<uint8> save,
	uint8 value_before, uint8 value_after) :
    inputs(inputs), save(std::move(save)),
    value_before(value_before), value_after(value_after) {}
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

  // Collection of experiments, each of which contains the (ascending
  // order) frame offsets (from the initial save) when decrements
  // happened. Used for timer detection.
  vector<vector<int>> offsets;
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

template<class T>
auto EraseIt(T &ty, typename T::iterator &it) -> typename T::iterator {
  auto next = it; ++next;
  ty.erase(it);
  return next;
}

static float ScoreOneBeforeValue(uint8 v) {
  // Many games allow zero lives, but many others kill you when reaching
  // zero. Also, this is a very common value for other stuff.
  if (v == 0) return 0.75f;
    
  // These are the most canonical number of lives, and we should never
  // be dead with this number of lives.
  if (v >= 1 && v <= 3) return 1.0f;
  // Reasonable lives, common health.
  if (v <= 10) return 0.8f;
  // Rare but possible lives, reasonable health.
  if (v <= 32) return 0.7f;
  // Sometimes health.
  if (v <= 64) return 0.5f;
  // Many games test death by seeing if the value is negative, and if
  // it's treated as unsigned then this is an absurd number of lives.
  // (XXX wait this should be slightly nonzero right?)
  if (v <= 127) return 0.0f;
  return 0.0f;
}

// Score the location based on the values it has at each point of
// decrement.
static float ScoreValues(const vector<Frame> &frames) {
  // Minimum "before" value. Just one observation with a crazy
  // value should downweight a lot, so we take the minimum over
  // all decrements.
  float minbefore = 1.0f;
  for (const Frame &f : frames) {
    const float scorebefore = ScoreOneBeforeValue(f.value_before);
    if (minbefore < scorebefore) minbefore = scorebefore;
  }

  // Next, decrements of exactly 1 are considered better.
  // Everything in here is between 1 and MAX_DECREMENT, so
  // that's all we look for.
  bool decrementone = true;
  for (const Frame &f : frames) {
    if (!Decremented(1, f.value_before, f.value_after)) {
      decrementone = false;
      break;
    }
  }

  // Usually we want to see 3 -> 2 -> 1, or like 15 -> 13 -> 12.
  // This is currently covered by the IncrementsPenalty below.

  if (decrementone) return minbefore;
  else return minbefore * 0.5f;
}

// Gaining extra lives or health is certainly possible, but
// should be less common than dying or taking damage in random
// play.
static float IncrementsPenalty(int increments, int decrements) {
  if (increments == 0) return 1.0f;
  if (increments > decrements) return 0.25f;
  else return 0.5f;
}

// PERF: Allow passing a whitelist or blacklist of locations
// to consider.
vector<AutoLives::LivesLoc> AutoLives::FindLives(const vector<uint8> &save,
						 int xloc, int yloc,
						 bool player_two) {

  Emulator *emu = emulator_pool.Acquire();
  ArcFour *rc = random_pool.Acquire();
  
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

    if (VERBOSE)
      printf("[%d/%d] Starting experiment with %d locs left.\n",
	     expt, FINDLIVES_NUM_EXPERIMENTS, (int)locs.size());
    
    vector<uint8> mem_prev = emu->GetMemory();
    for (int f = 0; f < TRY_TO_DIE_FRAMES; f++) {
      // No candidates left.
      if (locs.empty())
	break;

      vector<uint8> save_before_inputs = emu->SaveUncompressed();
      
      const uint8 input = nmarkov.RandomNext(nhist, rc);
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
					       save_before_inputs,
					       prev, now);
	  if (info->offsets.empty()) {
	    info->offsets.resize(FINDLIVES_NUM_EXPERIMENTS);
	  }
	  CHECK(expt < FINDLIVES_NUM_EXPERIMENTS);
	  info->offsets[expt].push_back(f);

	} else if (Decremented(MAX_DECREMENT, now, prev)) {
	  info->incremented++;
	} else {
	  // Change is too big. Eliminate it.
	  if (VERBOSE)
	    printf("[%d/%d] Disqualified %04x whose value was "
		   "%02x -> %02x. (%d left)\n",
		   expt, FINDLIVES_NUM_EXPERIMENTS,
		   loc, prev, now, (int)locs.size());
	  it = EraseIt(locs, it);
	  continue;
	}
	++it;
      }

      mem_prev = std::move(mem_now);
    }
  }

  if (VERBOSE)
    printf("After experiments, %d locations remain. These never changed:\n",
	   (int)locs.size());
  for (auto it = locs.begin(); it != locs.end(); /* in loop */) {
    const Info &info = it->second;
    if (info.incremented == 0 && info.decremented == 0) {
      if (VERBOSE)
	printf("%04x, ", it->first);
      it = EraseIt(locs, it);
      continue;
    }
    ++it;
  }
  if (VERBOSE) printf("\n");
  
  // Filter out anything that incremented/decremented too often.
  // Note that for something like health, this is probably much
  // too conservative.
  // (At this point we could probably be scoring these, rather
  // than filtering...)

#if 0 // XXXX
  static constexpr int MAX_INCDEC = 2 * FINDLIVES_NUM_EXPERIMENTS;
  if (VERBOSE)
    printf("%d remain. Filtered because they changed too often:\n",
	   (int)locs.size());
  for (auto it = locs.begin(); it != locs.end(); /* in loop */) {
    const Info &info = it->second;
    if (info.incremented > MAX_INCDEC ||
	info.decremented > MAX_INCDEC) {
      if (VERBOSE)
	printf("%04x %d= %d+ %d-\n", it->first,
	       info.same, info.incremented, info.decremented);
      it = EraseIt(locs, it);
      continue;
    }
    ++it;
  }
#endif
  
  // XXX Bugz: I wrote this thinking that all of the decrement_saves
  // are sequential within the same experiment. This is not what
  // happens -- we run many different experiments and put all the
  // saves into one set.
  #if 1
  // Filter out timers here. Timers just decrement at regular or
  // nearly-regular intervals, and never increment.
  static constexpr int MIN_COUNTER_REPETITIONS = 2;
  static_assert(MIN_COUNTER_REPETITIONS >= 2,
		"need 2 to find an interval!");
  if (VERBOSE)
    printf("%d remain. Filter out potential counters:\n", (int)locs.size());
  for (auto it = locs.begin(); it != locs.end(); /* in loop */) {
    const Info &info = it->second;
    if (info.incremented == 0) {
      if (VERBOSE) printf("%04x: ", it->first);
      // Returns the number of reps that are consistently repeating,
      // or -1 if it is actually inconsistent.
      auto GetConsistentDelta =
	[](const vector<int> &offsets, int *delta) -> int {
	  // Obviously, no consistent decrement if it's empty. But we
	  // could be anywhere in the period when we start
	  // experimenting, so we also can't do anything if we only
	  // have a single decrement.
	  if (offsets.size() < 2) return 0;
	  const int d = offsets[1] - offsets[0];
	  *delta = d;
	  int count = 1;
	  for (int s = 1; s < offsets.size() - 1; s++) {
	    const int d2 = offsets[s + 1] - offsets[s];
	    // Inconsistent.
	    // TODO: This can easily have false negatives because
	    // of lag frames. This may be happening in ninja gaiden.
	    // Allow for some slop?
	    if (d != d2)
	      return -1;
	    count++;
	  }
	  return count;
	};


      // Make sure all the deltas are the same. -1 means no delta
      // yet.
      int same_delta = -1;
      // Count of experiments that were inconsistent.
      int inconsistent = 0;
      // Total number of reps that were consistent.
      int consistent = 0;
      for (const vector<int> &offsets : info.offsets) {
	int delta = 0;
	const int reps = GetConsistentDelta(offsets, &delta);
	if (reps == -1) {
	  inconsistent++;
	  if (VERBOSE) printf(" i");
	} else if (reps >= MIN_COUNTER_REPETITIONS) {
	  if (VERBOSE) printf(" %dx%d", delta, reps);
	  consistent += reps;
	  if (same_delta == -1) same_delta = delta;
	  if (delta != same_delta) {
	    // Exit early, but also make sure we don't accept
	    // it somehow.
	    if (VERBOSE) printf(" %d != %d <exit>", delta, same_delta);
	    same_delta = -2;
	    break;
	  }
	} else {
	  if (VERBOSE) printf(" (%d)", reps);
	}
      }

      if (VERBOSE) printf("\n");
      
      if (same_delta > 0 &&
	  inconsistent < (0.10f * FINDLIVES_NUM_EXPERIMENTS) &&
	  consistent > (0.50f * FINDLIVES_NUM_EXPERIMENTS)) {

	if (VERBOSE)
	  printf("%04x is probably timer; delta %d, inconsistent %d times\n"
		 "     and consistent reps %d\n",
		 it->first, same_delta, inconsistent, consistent);

	it = EraseIt(locs, it);
	continue;
      }
    }
    ++it;
  }
  #endif
  
  if (VERBOSE)
    printf("\nAnd the rest are candidates (%d):\n", (int)locs.size());
  for (auto it = locs.begin(); it != locs.end(); ++it) {
    const Info &info = it->second;
    if (VERBOSE)
      printf("%04x %d= %d+ %d-\n", it->first,
	     info.same, info.incremented, info.decremented);
  }

  // Now, replay candidate "death" frames. When the "lives" location
  // is set to 1 or 0, we should see much worse "In Control" score.

  vector<LivesLoc> results;
  for (auto it = locs.begin(); it != locs.end(); ++it) {
    const int loc = it->first;
    const Info &info = it->second;

    if (VERBOSE) printf("%04x:\n", loc);
    int tests_with_zero = 0, tests_with_one = 0;
    float score_with_zero = 0.0f, score_with_one = 0.0f;

    auto AnalyzeFrames =
      [& /* XXX */]() {
	for (const Frame &frame : info.decremented_saves) {

	  // Re-execute the base frame (whatever happened before)
	  // to get the base "in control" fraction.
	  emu->LoadUncompressed(frame.save);
	  const uint8 base_value_before = emu->ReadRAM(loc);
	  // If it already contains 0, we won't do any tests. So save
	  // us from doing the expensive IsInControl call for base.
	  if (base_value_before == 0)
	    continue;
	  // Execute the inputs with the normal number of lives.
	  emu->Step16(frame.inputs);
	  const uint8 base_value_after = emu->ReadRAM(loc);
	  const float base_control = IsInControl(emu->SaveUncompressed(),
						 xloc, yloc,
						 player_two);

	  // TODO: We could increase our confidence by doing this for
	  // many more values.
	  
	  // Now, do the same for some other small but different value.
	  // We expect that our value is decremented the same amount,
	  // and that we retain control.
	  const uint8 alt_value_before = base_value_before + 1;
	  emu->LoadUncompressed(frame.save);
	  emu->SetRAM(loc, alt_value_before);
	  emu->Step16(frame.inputs);
	  const uint8 alt_value_after = emu->ReadRAM(loc);
	  const uint8 dbase = base_value_before - base_value_after;
	  const uint8 dalt = alt_value_before - alt_value_after;
	  // We expect to decrement by the same amount.
	  // Two cases where this may be too strict:
	  //   - this frame is actually our last life. In this case,
	  //     though, we can't conduct the experiment anyway.
	  //     (but would be better if we 'continue;')
	  //   - We are trying to increment health or something past
	  //     its maximum, and somehow there's a check for that
	  //     before the player takes damage. I think this is
	  //     probably very rare?
	  if (dbase != dalt) {
	    if (VERBOSE)
	      printf("%04x failed alt test: base %d->%d alt %d->%d\n",
		     loc, base_value_before, base_value_after,
		     alt_value_before, alt_value_after);
	    return false;
	  }
	  const float alt_control = IsInControl(emu->SaveUncompressed(),
						xloc, yloc,
						player_two);
	  
	  // If we aren't able to set this to some other "alive"
	  // value and retain control, something is wrong.
	  if (fabs(base_control - alt_control) > 0.05) {
	    if (VERBOSE)
	      printf("%04x inconsistent control:\n"
		     "     base %d->%d (ctl %.4f) vs alt %d->%d (ctl %.4f)\n",
		     loc, base_value_before, base_value_after, base_control,
		     alt_value_before, alt_value_after, alt_control);
	    return false;
	  }

	  // XXX skip if not enough control in base (absolute threshold)?
	  
	  // Test with the lives value set to set_to, if applicable.
	  auto Test =
	    [this, emu, rc, xloc, yloc, player_two, loc, &frame,
	     base_control, base_value_before, base_value_after](
		 uint8 set_to, int *tests, float *score) {
	      emu->LoadUncompressed(frame.save);
	      uint8 *ram = emu->GetFC()->fceu->RAM;
	      const uint8 expt_value_before = ram[loc];
	      CHECK(expt_value_before == base_value_before);
	      if (VERBOSE)
		printf(" test ram[%04x] base (%02x->%02x) ctl %.3f  "
		       "expt (%02x->",
		       loc, base_value_before, base_value_after,
		       base_control, set_to);
	      if (expt_value_before > set_to) {
		// Modify memory.
		ram[loc] = set_to;
		emu->Step16(frame.inputs);
		ram = emu->GetFC()->fceu->RAM;
		const uint8 expt_value_after = ram[loc];
		const float expt_control = IsInControl(emu->SaveUncompressed(),
						       xloc, yloc, player_two);
		if (VERBOSE)
		  printf("%02x) ctl %.3f",
			 expt_value_after,
			 expt_control);

		// XXX we expect the value to decrement as before, though
		// it's also reasonable for the death routine to be like
		// void Die() {
		//   if (lives == 0) goto GameOver();
		//   lives--;
		// }
		// But we could penalize the location for doing something
		// nonsensical here.
		
		// Increase score when base_control > expt_control
		// (that is, we have less control now).
		++*tests;
		*score += (base_control - expt_control);
	      } else {
		if (VERBOSE) printf("skip)");
	      }
	      if (VERBOSE) printf("\n");
	    };

	  Test(0, &tests_with_zero, &score_with_zero);
	  Test(1, &tests_with_one, &score_with_one);
	}
	return true;
      };

    // If we're unable to change the value of the memory location
    // (to some small value that's not 1 or 0), then we just filter
    // it out.
    if (!AnalyzeFrames())
      continue;
      
    // Inspect the actual values that the memory location takes on.
    const float valuescore = ScoreValues(info.decremented_saves);
    // Discount the score if we saw increments.
    const float incpenalty = IncrementsPenalty(info.incremented,
					       info.decremented);
    
    // Use median maybe? We do expect this to be rather consistent.
    const float controlscore =
      [&]() {
	if (tests_with_zero > 0 && tests_with_one > 0) {
	  return std::max(score_with_zero / (float)tests_with_zero,
			  score_with_one / (float)tests_with_one);
	} else if (tests_with_zero > 0) {
	  return score_with_zero / (float)tests_with_zero;
	} else if (tests_with_one > 0) {
	  return score_with_one / (float)tests_with_one;
	} else {
	  return -1.0f;
	}
      }();

    // Should probably just filter anything with negative combined score...
    const float combinedscore =
      controlscore <= 0.0f ? controlscore :
      (controlscore * incpenalty) + valuescore;
    
    if (VERBOSE)
      printf("%04x, %d fs ctl %.4f: wz %.2f/%d, wo %.2f/%d, value %.2f inc %.2f = %.4f\n",
	     loc,
	     (int)info.decremented_saves.size(),
	     controlscore,
	     score_with_zero, tests_with_zero,
	     score_with_one, tests_with_one,
	     valuescore, incpenalty,
	     combinedscore);

    results.emplace_back(loc, combinedscore);
  }
  if (VERBOSE)
    printf("\n");
  
  std::sort(results.begin(), results.end(),
	    [](const LivesLoc &a, const LivesLoc &b) {
	      return a.score > b.score;
	    });

  if (VERBOSE) {
    printf("\n%d final candidates:\n", (int)results.size());
    for (const LivesLoc &ll : results) {
      printf("%04x %.6f\n", ll.loc, ll.score);
    }
  }

  random_pool.Release(rc);
  emulator_pool.Release(emu);
  
  return results;
}

// static
vector<AutoLives::LivesLoc> AutoLives::MergeLives(
    const vector<vector<LivesLoc>> &lv) {
  std::unordered_map<int, LivesLoc> sums;
  for (const auto &vec : lv) {
    for (const LivesLoc &l : vec) {
      LivesLoc *s = &sums[l.loc];
      s->loc = l.loc;
      s->score += l.score;
    }
  }

  // XXX topn?
  vector<LivesLoc> results;
  results.reserve(sums.size());
  for (const auto &p : sums)
    results.push_back(p.second);

  std::sort(results.begin(), results.end(),
	    [](const LivesLoc &a, const LivesLoc &b) {
	      return a.score > b.score;
	    });

  return results;
}
