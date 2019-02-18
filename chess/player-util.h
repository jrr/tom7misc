
#ifndef __PLAYER_UTIL_H
#define __PLAYER_UTIL_H

#include <vector>
#include <cstdint>
#include <string>
#include <memory>

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
struct EvalResultPlayer : public StatelessPlayer {
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

template<class P, class ...Args>
struct MakeStateless : public Player {
  // Same constructor.
  MakeStateless(Args... args) : player(new P(args...)) {}
  
  struct MSGame : public PlayerGame {
    explicit MSGame(P *player) : player(player) {}

    void ForceMove(Position::Move move) override { }
    // Get a move for the current player in the current position.
    Position::Move GetMove(const Position &pos) override {
      return player->MakeMove(pos);
    }

    // Owned by parent object.
    P *player;
  };

  MSGame *CreateGame() override {
    return new MSGame(player.get());
  }
  
  std::string Name() const override { return player->Name(); }
  std::string Desc() const override { return player->Desc(); }
  std::unique_ptr<P> player;
};

#endif
