
#ifndef __PLAYER_UTIL_H
#define __PLAYER_UTIL_H

#include <vector>
#include <cstdint>
#include <string>

#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"

#include "chess.h"
#include "player.h"

struct PlayerUtil {
  static std::string GetSeed();

  static bool ParseLongMove(const std::string &move_s,
			    bool black, Position::Move *m);
  
  template<class T, class F>
  static const T &GetBest(const std::vector<T> &v, F f) {
    CHECK(!v.empty());
    int best_i = 0;
    for (int i = 1; i < v.size(); i++) {
      // f is <, so this means a strict improvement
      if (f(v[i], v[best_i])) {
	best_i = i;
      }
    }
    return v[best_i];
  }
};

// Base class for a player orders by some metric on the board
// state after the move, and breaks ties at random.
struct EvalResultPlayer : public Player {
  EvalResultPlayer();
  
  // With smaller scores being better.
  virtual int64_t PositionPenalty(Position *p) = 0;

  Position::Move MakeMove(const Position &orig_pos) override;
  
  struct LabeledMove {
    Position::Move m;
    int64_t penalty = 0.0;
    uint32_t r = 0u;
  };
  
  ArcFour rc;
};


#endif
