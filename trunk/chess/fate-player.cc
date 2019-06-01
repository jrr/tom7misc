
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

static constexpr bool VERBOSE = false;

namespace {
template<bool NORM>
struct FatePlayer : public Player {
  FatePlayer() : rc(PlayerUtil::GetSeed()) {}
  
  struct FGame : public PlayerGame {
    explicit FGame(FatePlayer *parent) : parent(parent) {
    }

    // Higher score better.
    struct LabeledMove {
      Position::Move m;
      double score = 0.0;
    };
    
    void ForceMove(const Position &pos, Position::Move move) override {
      gamestats.Update(pos, move);
    }
    
    // Get a move for the current player in the current position.
    Position::Move GetMove(const Position &orig_pos,
			   Explainer *explainer) override {
      Position pos = orig_pos;
      std::vector<LabeledMove> labeled;
      double min_score = 99999.0, max_score = 0.0;
      if (VERBOSE) printf("\n------\n");
      for (const Move &m : pos.GetLegalMoves()) {
	LabeledMove lm;
	lm.m = m;
	GameStats gs_copy = gamestats;
	gs_copy.Update(pos, m);
	pos.MoveExcursion(m,
			  [this, &pos, &lm, &gs_copy]() {
			    lm.score = parent->Eval(&pos, gs_copy);
			    return 0;
			  });

	min_score = std::min(lm.score, min_score);
	max_score = std::max(lm.score, max_score);
	labeled.push_back(lm);
	if (VERBOSE)
	  printf("%s ~ %.6f\n", pos.ShortMoveString(lm.m).c_str(), lm.score);
      }
      CHECK(!labeled.empty());

      double total_score = 0.0;
      if (NORM) {
	// Prevent singularities.
	if (labeled.size() == 1)
	  return labeled[0].m;
	if (min_score >= max_score)
	  return labeled[RandTo32(&parent->rc, labeled.size())].m;

	if (VERBOSE) printf("norm:\n");
	const double onorm = 1.0 / (max_score - min_score);
	double total_score = 0.0;
	for (LabeledMove &lm : labeled) {
	  lm.score = (lm.score - min_score) * onorm;
	  total_score += lm.score;
	  if (VERBOSE) printf("%s -> %.6f\n",
			      pos.ShortMoveString(lm.m).c_str(), lm.score);
	}

	if (VERBOSE)
	  printf("min %.6f max %.6f onorm %.6f total %.6f\n",
		 min_score, max_score, onorm, total_score);
      } else {
	for (const LabeledMove &lm : labeled)
	  total_score += lm.score;
      }
      // Sample, weighted by score. Index into the stacked
      // probability mass.
      double idx = RandDouble(&parent->rc) * total_score;
      if (VERBOSE) printf("\nIndex %.6f / %.6f\n", idx, total_score);
      
      for (int i = 0; i < labeled.size(); i++) {
	idx -= labeled[i].score;
	if (VERBOSE) printf("  Move %d idx %.6f\n", i, idx);
	if (idx < 0.0) {
	  if (VERBOSE) fflush(stdout);
	  return labeled[i].m;
	}
      }
      if (VERBOSE) fflush(stdout);
      
      // Could get all the way to the end in unlucky rounding situations.
      // Return the last move in this case.
      return labeled[labeled.size() - 1].m;
    }

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

struct SafePlayer : public FatePlayer<true> {
  string Name() const override { return "safe"; }
  string Desc() const override {
    return "Move pieces towards squares where they are more likely "
      "to survive (according to statistics computed for all games "
      "in the lichess database through Nov 2018).";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 1.0;
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

struct DangerousPlayer : public FatePlayer<true> {
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

struct PopularPlayer : public FatePlayer<true> {
  string Name() const override { return "popular"; }
  string Desc() const override {
    return "Move pieces towards squares where they are more likely "
      "to be at the end of the game.";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 0.0;
    const bool black = !pos->BlackMove();
    for (int i = black ? 0 : 16; i < (black ? 16 : 32); i++) {
      if ((gs.fates[i] & GameStats::DIED) == 0) {
	const LivedDied &ld =
	  all_fate_data[i][gs.fates[i] & GameStats::POS_MASK];
	score += ld.died + ld.lived;
      }
    }
    return score;
  }
};

struct RarePlayer : public FatePlayer<true> {
  string Name() const override { return "rare"; }
  string Desc() const override {
    return "Move pieces towards squares where they are less likely "
      "to be at the end of the game.";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 0.0;
    const bool black = !pos->BlackMove();
    for (int i = black ? 0 : 16; i < (black ? 16 : 32); i++) {
      if ((gs.fates[i] & GameStats::DIED) == 0) {
	const LivedDied &ld =
	  all_fate_data[i][gs.fates[i] & GameStats::POS_MASK];
	score += 1.0 - (ld.died + ld.lived);
      }
    }
    return score;
  }
};

struct SurvivalistPlayer : public FatePlayer<false> {
  string Name() const override { return "survivalist"; }
  string Desc() const override {
    return "Move pieces towards squares with higher survival:death "
      "ratios.";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 0.0;
    const bool black = !pos->BlackMove();
    for (int i = black ? 0 : 16; i < (black ? 16 : 32); i++) {
      if ((gs.fates[i] & GameStats::DIED) == 0) {
	const LivedDied &ld =
	  all_fate_data[i][gs.fates[i] & GameStats::POS_MASK];
	if (ld.died > 0.0) {
	  score += (ld.lived / ld.died);
	} else {
	  // This constant doesn't matter much, since 
	  score += 1e6;
	}
      }
    }
    return score;
  }
};

struct FatalistPlayer : public FatePlayer<false> {
  string Name() const override { return "fatalist"; }
  string Desc() const override {
    return "Move pieces towards squares with higher death:survival "
      "ratios.";
  }
  double Eval(Position *pos, const GameStats &gs) override {
    double score = 0.0;
    const bool black = !pos->BlackMove();
    for (int i = black ? 0 : 16; i < (black ? 16 : 32); i++) {
      if ((gs.fates[i] & GameStats::DIED) == 0) {
	const LivedDied &ld =
	  all_fate_data[i][gs.fates[i] & GameStats::POS_MASK];
	if (ld.lived > 0.0) {
	  score += (ld.died / ld.lived);
	} else {
	  score += 1e6;
	}
      }
    }
    return score;
  }
};

struct EqualizerPlayer : public Player {
  EqualizerPlayer() : rc(PlayerUtil::GetSeed()) {}
  
  struct EQGame : public PlayerGame {
    EQGame(EqualizerPlayer *parent) : parent(parent) {
      for (int i = 0; i < 32; i++)
	moved[i] = 0;
      for (int i = 0; i < 64; i++)
	visited_black[i] = visited_white[i] = 0;
      // Starting position counts as "visited."
      for (int i = 0; i < 16; i++) visited_black[i] = 1;
      for (int i = 48; i < 64; i++) visited_white[i] = 1;
    }

    // Higher score better.
    struct LabeledMove {
      Position::Move m;
      int piece_moved = 0;
      int square_visited = 0;
      uint32 r = 0;
    };
    
    void ForceMove(const Position &pos, Position::Move m) override {
      const int srcidx = gamestats.PieceIndexAt(m.src_row, m.src_col);
      CHECK(srcidx >= 0) << srcidx;
      moved[srcidx]++;
      const bool black = pos.BlackMove();
      int *visited = black ? &visited_black[0] : &visited_white[0];
      int dst_pos = (int)m.dst_row * 8 + (int)m.dst_col;
      visited[dst_pos]++;

      // But also, if this is a castling move, increment the destination
      // of the rook and the movement of the rook.
      const int src_pos = (int)m.src_row * 8 + (int)m.src_col;
      if (src_pos == 4 && pos.PieceAt(0, 4) ==
	  (Position::BLACK | Position::KING)) {
	if (dst_pos == 2) {
	  moved[0]++;
	  visited[3]++;
	} else if (dst_pos == 6) {
	  moved[7]++;
	  visited[5]++;
	}
      } else if (src_pos == 60 && pos.PieceAt(7, 4) ==
		 (Position::WHITE | Position::KING)) {
	if (dst_pos == 58) {
	  moved[24]++;
	  visited[59]++;
	} else if (dst_pos == 62) {
	  moved[31]++;
	  visited[61]++;
	}
      }
	  
      gamestats.Update(pos, m);
    }

    Position::Move GetMove(const Position &orig_pos,
			   Explainer *explainer) override {
      Position pos = orig_pos;
      std::vector<LabeledMove> labeled;
      const bool black = pos.BlackMove();
      
      for (const Move &m : pos.GetLegalMoves()) {
	LabeledMove lm;
	lm.m = m;
	lm.r = Rand32(&parent->rc);
	// a piece index
	const int srcidx = gamestats.PieceIndexAt(m.src_row, m.src_col);
	CHECK(srcidx >= 0) << srcidx;
	lm.piece_moved = moved[srcidx];
	// a board index
	const int dstidx = (int)m.dst_row * 8 + (int)m.dst_col;
	lm.square_visited =
	  black ? visited_black[dstidx] : visited_white[dstidx];

	labeled.push_back(lm);
      }
      CHECK(!labeled.empty());

      return PlayerUtil::GetBest(
	  labeled,
	  [](const LabeledMove &a,
	     const LabeledMove &b) {
	    // First, prefer a piece that's moved more rarely.
	    if (a.piece_moved != b.piece_moved)
	      return a.piece_moved < b.piece_moved;
	    // Next, to a less-visited destination.
	    if (a.square_visited != b.square_visited)
	      return a.square_visited < b.square_visited;
	    // Break ties randomly.
	    return a.r < b.r;
	  }).m;
    }
    
    GameStats gamestats;
    // Number of times the piece has moved.
    int moved[32] = {};
    // Number of times the square was visited by white, black.
    int visited_white[64] = {};
    int visited_black[64] = {};
    EqualizerPlayer *parent;
  };

  EQGame *CreateGame() override {
    return new EQGame(this);
  }
  
  string Name() const override { return "equalizer"; }
  string Desc() const override {
    // Due to "Anonymous"
    return "Move the piece that has moved the fewest number of "
      "times to the square that has received the fewest of my "
      "pieces, breaking ties randomly.";
  }

  ArcFour rc;
};


}  // namespace

Player *Safe() {
  return new SafePlayer;
}

Player *Dangerous() {
  return new DangerousPlayer;
}

Player *Popular() {
  return new PopularPlayer;
}

Player *Rare() {
  return new RarePlayer;
}

Player *Survivalist() {
  return new SurvivalistPlayer;
}

Player *Fatalist() {
  return new FatalistPlayer;
}


Player *Equalizer() {
  return new EqualizerPlayer;
}
