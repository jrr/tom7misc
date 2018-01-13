#ifndef __N_MARKOV_CONTROLLER_H
#define __N_MARKOV_CONTROLLER_H

#include "pftwo.h"

#include <unordered_map>
#include "../cc-lib/arcfour.h"

// A Markov generator for NES controller inputs (8-bit bytes) that keeps
// n (in 0..8) inputs of history. Allows sampling from the successors of a
// state, and has a compact state representation (uint64). 
struct NMarkovController {
  using History = uint64;
  
  History HistoryInDomain() const;
  // Given some recent history, sample a next input. If the history has
  // no precedent, returns zeroes (which often, but not always, gets
  // you back to a state that does have precedent. XXX - should fix this!)
  uint8 RandomNext(History current, ArcFour *rc) const;

  // Add the input 'next' into the history, and remove the oldest entry
  // so that there are exactly n. (A shift, bit mask, and bitwise-or).
  History Push(History h, uint8 next) const;
  
  // The inputs are treated cyclically.
  // Only n up to 8 is supported (input sequences are stored in 64-bit
  // words).
  NMarkovController(const vector<uint8> &inputs, int n);

  // Log stats to console.
  void Stats() const;
  
 private:
  const int n;
  // Any input history that's known to be in the domain.
  History history_in_domain = 0ULL;
  const uint64 history_bitmask;
  // For a history in the domain, all the inputs that could follow it,
  // along with the associated nonzero probability mass. The uint32s
  // in a row all add to 2^32-1.
  unordered_map<History, vector<pair<uint32, uint8>>> matrix;
};

#endif
