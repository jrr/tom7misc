
#ifndef __PLAYER_UTIL_H
#define __PLAYER_UTIL_H

#include <vector>
#include <cstdint>
#include <string>
#include <memory>

#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "chess.h"
#include "player.h"

struct PlayerUtil {
  static std::string GetSeed();

  static bool ParseLongMove(const std::string &move_s,
			    bool black, Position::Move *m);

  // Get the best item from the non-empty vector f, where
  // f(a, b) returns true if a is better than b.
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

  Position::Move MakeMove(const Position &orig_pos,
			  Explainer *explainer) override;
  
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

    void ForceMove(const Position &pos, Position::Move move) override { }
    // Get a move for the current player in the current position.
    Position::Move GetMove(const Position &pos,
			   Explainer *explainer) override {
      return player->MakeMove(pos, explainer);
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

// Blend a player with random play. The move is random if
// a random 16-bit number is less than the threshold.
template<uint16_t THRESH>
struct BlendRandom : public Player {
  static_assert(THRESH > 0 && THRESH < 65535, "0 < THRESH < 65535");
  BlendRandom(Player *player) : player(player),
				rc(PlayerUtil::GetSeed()) {}
  struct BGame : public PlayerGame {
    BGame(ArcFour *rc, PlayerGame *pgame) : rc(rc), pgame(pgame) {}
    void ForceMove(const Position &pos, Position::Move move) override {
      pgame->ForceMove(pos, move);
    }

    Position::Move GetMove(const Position &orig_pos,
			   Explainer *explainer) override {
      const uint16 r = Rand16(rc);
      if (r < THRESH) {
	// Random move.
	Position pos = orig_pos;
	std::vector<Position::Move> legal = pos.GetLegalMoves();
	CHECK(!legal.empty());
	return legal[RandTo32(rc, legal.size())];
      } else {
	return pgame->GetMove(orig_pos);
      }
    }
    
    ArcFour *rc = nullptr;
    PlayerGame *pgame = nullptr;
  };
  
  PlayerGame *CreateGame() override {
    return new BGame(&rc, player->CreateGame());
  }

  // As above.
  std::string Name() const override {
    return StringPrintf("%s_r%d", player->Name().c_str(), (int)THRESH);
  }
  std::string Desc() const override {
    return StringPrintf("Blend: %d random + %d (%s)",
			(int)THRESH, (int)(65535 - THRESH),
			player->Desc().c_str());
  }

  virtual ~BlendRandom() {}
  std::unique_ptr<Player> player;
  ArcFour rc;
};


#endif
