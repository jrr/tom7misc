#ifndef _GAMESTATS_H
#define _GAMESTATS_H

#include <shared_mutex>
#include <cstdint>

#include "base/logging.h"
#include "chess.h"
#include "fates.h"

// Whole purpose of running over games is to collect these stats. So
// that we can estimate variance, we also randomly assign players to
// one of G groups (by hash of username) and accumulate a given game
// into the white player's bucket. So there are actually G Stats
// objects.
static constexpr int NUM_BUCKETS = 32;
static_assert(! (NUM_BUCKETS & (NUM_BUCKETS - 1)),
              "Must be a power of two");
static constexpr int64_t NUM_BUCKETS_MASK = NUM_BUCKETS - 1;

struct PieceStats {
  int64_t died_on[64] = {};
  int64_t survived_on[64] = {};
};

struct Stats {
  std::shared_mutex m;
  int64_t num_games = 0LL;
  PieceStats pieces[32] = {};
  void AddGame(const Fates &gs) {
    m.lock();
    num_games++;
    for (int i = 0; i < 32; i++) {
      const uint8_t fate = gs.fates[i];
      if (fate & Fates::DIED) {
        pieces[i].died_on[fate & Fates::POS_MASK]++;
      } else {
        pieces[i].survived_on[fate & Fates::POS_MASK]++;
      }
    }
    m.unlock();
  }
};


#endif
