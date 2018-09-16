// Like autocamera, but for finding memory locations to protect
// the values of, like a location that stores the number of
// lives the player has. (This could also be stuff like player
// health.)
//
// This first cut is based on the following idea.
//   - On most frames, it does not change.
//   - A value containing lives should often be at risk of
//     decrementing (i.e., random play does decrement it on
//     "many" frames).
//   - When the value is decremented to zero (or below zero),
//     the player loses control "a lot more" than if the value
//     is 1 or more.
//
// Since training data probably doesn't contain a lot of deaths,
// we need to also search inputs to find examples.

// Ideas TODO:
//  - Can filter out memory locations that are write-before-read
//    (i.e, temporaries)

#ifndef __AUTOLIVES_H
#define __AUTOLIVES_H

#include "pftwo.h"

#include <functional>
#include <memory>
#include <string>
#include <vector>
#include <functional>

#include "../fceulib/emulator.h"
#include "../cc-lib/arcfour.h"
#include "n-markov-controller.h"

#include "random-pool.h"
#include "emulator-pool.h"

// As with autocamera, focus is on quality and debuggability, not
// performance.
struct AutoLives {
  // Creates some private emulator instances that it can reuse.
  // Wants a markov model for generating inputs.
  AutoLives(const string &game,
	    NMarkovController nmarkov);
  ~AutoLives();

  // Returns a score in [0, 1] indicating whether we seem to be
  // able to control the player now, where 0 is the least control.
  // Has to simulate a bunch of frames, so this is fairly slow.
  // xloc and yloc must be memory locations that represent the
  // player's location.
  float IsInControl(const vector<uint8> &save,
		    int xloc, int yloc,
		    bool player_two);

  struct LivesLoc {
    int loc = 0;
    float score = 0.0f;
    LivesLoc() {}
    LivesLoc(int loc, float score) : loc(loc), score(score) {}
  };
  
  // Find and score memory locations that might be lives, starting
  // from the given state. This is expensive since among other things,
  // it needs to play randomly in order to find inputs that "kill" the
  // player.
  vector<LivesLoc> FindLives(const vector<uint8> &save,
			     int xloc, int yloc,
			     bool player_two);

  // Merge and sort by summing scores.
  static vector<LivesLoc> MergeLives(const vector<vector<LivesLoc>> &lv);
  
private:
  RandomPool random_pool;
  EmulatorPool emulator_pool;
  const NMarkovController nmarkov;
};

#endif
