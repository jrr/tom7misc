#ifndef __GAMESTATS_H
#define __GAMESTATS_H

#include <shared_mutex>
#include <cstdint>

// Whole purpose of running over games is to collect these stats. So
// that we can estimate variance, we also randomly assign players to
// one of G groups (by hash of username) and accumulate a given game
// into the white player's bucket. So there are actually G Stats
// objects.
static constexpr int NUM_BUCKETS = 32;
static_assert(! (NUM_BUCKETS & (NUM_BUCKETS - 1)),
	      "Must be a power of two");
static constexpr int64_t NUM_BUCKETS_MASK = NUM_BUCKETS - 1;


// Results of a single game.
struct GameStats {
  static constexpr uint8_t DIED = 0b10000000;
  static constexpr uint8_t POS_MASK = 0b00111111;
  // Initialized to their start squares (alive) in standard position.
  uint8_t fates[32] = { 0,  1,  2,  3,  4,  5,  6,  7,
		        8,  9, 10, 11, 12, 13, 14, 15,
		       48, 49, 50, 51, 52, 53, 54, 55,
		       56, 57, 58, 59, 60, 61, 62, 63, };
};

struct PieceStats {
  int64_t died_on[64] = {};
  int64_t survived_on[64] = {};
};

struct Stats {
  std::shared_mutex m;
  int64_t num_games = 0LL;
  PieceStats pieces[32] = {};
  void AddGame(const GameStats &gs) {
    m.lock();
    num_games++;
    for (int i = 0; i < 32; i++) {
      const uint8_t fate = gs.fates[i];
      if (fate & GameStats::DIED) {
	pieces[i].died_on[fate & GameStats::POS_MASK]++;
      } else {
	pieces[i].survived_on[fate & GameStats::POS_MASK]++;
      }
    }
    m.unlock();
  }
};


#endif
