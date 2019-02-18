
#include "fate-player.h"

#include <string>

#include "all-fate-data.h"
#include "gamestats.h"
#include "player-util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"

#include "chess.h"

using Move = Position::Move;
using namespace std;

namespace {
struct FatePlayer : public Player {
  FatePlayer() : rc(PlayerUtil::GetSeed()) {}
  
  struct FGame : public PlayerGame {
    explicit FGame(FatePlayer *parent) : parent(parent) {
    }

    // Higher score better.
    struct LabeledMove {
      Position::Move m;
      double score = 0.0;
      uint32_t r = 0u;
    };
    
    void ForceMove(Position::Move move) override {
      gamestats.Update(pos, move);
      pos.ApplyMove(move);
    }
    // Get a move for the current player in the current position.
    Position::Move GetMove(const Position &orig_pos) override {
      Position pos = orig_pos;
      std::vector<LabeledMove> labeled;
      for (const Move &m : pos.GetLegalMoves()) {
	LabeledMove lm;
	lm.m = m;
	lm.r = Rand32(&parent->rc);
	GameStats gs_copy = gamestats;
	gs_copy.Update(pos, m);
	pos.MoveExcursion(m,
			  [this, &pos, &lm, &gs_copy]() {
			    lm.score = parent->Eval(&pos, gs_copy);
			    return 0;
			  });
	labeled.push_back(lm);
      }
      CHECK(!labeled.empty());
      
      return PlayerUtil::GetBest(
	  labeled,
	  [](const LabeledMove &a,
	     const LabeledMove &b) {
	    if (a.score != b.score)
	      return a.score > b.score;
	    
	    return a.r < b.r;
	  }).m;
    }

    Position pos;
    // Owned by parent object.
    FatePlayer *parent;
    // Locations of the 32 players.
    GameStats gamestats;
  };

  // Return a higher score for a more preferable move.
  // Note that the position represents the state after the move,
  // so it is the opponent's turn now.
  virtual double Eval(Position *pos, const GameStats &gs) = 0;
  
  FGame *CreateGame() override {
    return new FGame(this);
  }
  
  ArcFour rc;
};

struct SafePlayer : public FatePlayer {
  string Name() const override { return "safe"; }
  string Desc() const override {
    return "Move pieces towards squares where they are more likely "
      "to survive (according to statistics computed for all games "
      "in the lichess database through Nov 2018).";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 0.0;
    const bool black = !pos->BlackMove();
    // Sum over all of my own living pieces.
    for (int i = black ? 0 : 16; i < (black ? 16 : 32); i++) {
      if ((gs.fates[i] & GameStats::DIED) == 0) {
	const LivedDied &ld =
	  all_fate_data[i][gs.fates[i] & GameStats::POS_MASK];
	score += ld.lived;
      }
    }
    return score;
  }
};

struct DangerousPlayer : public FatePlayer {
  string Name() const override { return "dangerous"; }
  string Desc() const override {
    return "Move pieces towards squares where they are more likely "
      "to die (according to statistics computed for all games "
      "in the lichess database through Nov 2018).";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 0.0;
    const bool black = !pos->BlackMove();
    for (int i = black ? 0 : 16; i < (black ? 16 : 32); i++) {
      if ((gs.fates[i] & GameStats::DIED) == 0) {
	const LivedDied &ld =
	  all_fate_data[i][gs.fates[i] & GameStats::POS_MASK];
	score += ld.died;
      }
    }
    return score;
  }
};

}  // namespace

Player *Safe() {
  return new SafePlayer;
}

Player *Dangerous() {
  return new DangerousPlayer;
}
