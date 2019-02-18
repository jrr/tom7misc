
#include <string>
#include <memory>
#include <cstdint>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "player.h"
#include "stockfish.h"
#include "chess.h"
#include "player-util.h"
#include "subprocess.h"

using int64 = int64_t;

using Move = Position::Move;
using namespace std;

namespace {
struct StockfishPlayer : public StatelessPlayer {

  StockfishPlayer(int level, int nodes, const string &name) :
    level(level), nodes(nodes), name(name) {
    fish.reset(new Stockfish(level, nodes));
    CHECK(fish.get());
  }
  
  Move MakeMove(const Position &orig_pos) override {
    // XXX: In endgames, the move clock matters. Should
    // perhaps be providing this.
    string fen = orig_pos.ToFEN(10, 10);
    string move_s;
    Stockfish::Score score;
    fish->GetMove(fen, &move_s, &score);
    Move m;
    CHECK(PlayerUtil::ParseLongMove(move_s, orig_pos.BlackMove(), &m))
      << orig_pos.BoardString()
      << "\n" << fen
      << "\n[" << move_s << "]";
    {
      Position pos = orig_pos;
      CHECK(pos.IsLegal(m));
    }

    return m;
  }
  
  string Name() const override {
    return name;
  }
  
  string Desc() const override {
    return StringPrintf("Stockfish engine, no tables, level %d, nodes %lld",
			level, nodes);
  }
  
  const int level;
  const int64 nodes;
  const string name;
  std::unique_ptr<Stockfish> fish;
};

struct WorstfishPlayer : public EvalResultPlayer {

  WorstfishPlayer(int level) : EvalResultPlayer(), level(level) {
    fish.reset(new Stockfish(level, 0));
    CHECK(fish.get());
  }

  // Should be bigger than any possible centipawn loss and any possible
  // mating sequence length. Mate in 5 is represented as (MATE - 5).
  static constexpr int64 MATE = 0xFFFF'FFFF'FFFF;

  // Say that Worstfish is playing as white. Then we enumerate all of
  // white's moves, apply them, and call PositionPenalty as black.
  // Stockfish searches for black's best move and evaluates the position
  // from black's point of view. If the position is good for black,
  // then we should return low scores.
  int64 PositionPenalty(Position *p) override {
    string fen = p->ToFEN(10, 10);
    string move_s;
    Stockfish::Score score;
    fish->GetMove(fen, &move_s, &score);

    // There's no reason to parse the actual move; we just care about
    // the score. In fact there may be no move (stockfish returns
    // "(none)") because we may be mated.
    // 
    // In the case that the player checkmated, we see "mate 0",
    // and in the case of stalemate we see "cp 0".

    if (score.is_mate) {
      // Consider any "mate in x" to be lexicographically more important
      // than centipawn loss, using MATE as like "infinity".

      if (score.value == 0) {
	// Black is checkmated. Then this is white's best possible move,
	// so it gets the highest penalty.
	return MATE;
      } else if (score.value > 0) {
	// Black can mate in x moves. This is bad for white, so this gets
	// a very low (negative) penalty so that we select such moves.
	// The closer mate is (smaller x), the worse for white.
	return -MATE + score.value;
      } else {
	// White can mate in x moves. This is good for white, so the
	// penalty must be high. Value is negative, so add it to
	// MATE; a mate in -1 is better for white than a mate in -7.
	return MATE + score.value;
      }
    } else {
      // If centipawns is positive here, then "black" is winning.
      return -score.value;
    }
  }
  
  string Name() const override {
    return "worstfish";
  }
  
  string Desc() const override {
    return StringPrintf("Make the worst legal move according to Stockfish "
			"(no tables, level %d).",
			level);
  }

  const int level;
  std::unique_ptr<Stockfish> fish;
};

}  // namespace


StatelessPlayer *CreateStockfish0() {
  return new StockfishPlayer(0, 0, "stockfish0");
}

StatelessPlayer *CreateStockfish5() {
  return new StockfishPlayer(5, 0, "stockfish5");
}

StatelessPlayer *CreateStockfish10() {
  return new StockfishPlayer(10, 0, "stockfish10");
}

StatelessPlayer *CreateStockfish15() {
  return new StockfishPlayer(15, 0, "stockfish15");
}

StatelessPlayer *CreateStockfish20() {
  return new StockfishPlayer(20, 0, "stockfish20");
}

StatelessPlayer *CreateStockfish1M() {
  return new StockfishPlayer(20, 1000000, "stockfish1m");
}


StatelessPlayer *CreateWorstfish() {
  return new WorstfishPlayer(20);
}
