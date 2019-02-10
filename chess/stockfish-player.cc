
#include <string>
#include <memory>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "player.h"
#include "stockfish.h"
#include "chess.h"
#include "player-util.h"
#include "subprocess.h"

using Move = Position::Move;
using namespace std;

namespace {
struct StockfishPlayer : public Player {

  StockfishPlayer(int level) : level(level) {
    fish.reset(new Stockfish(level));
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
    return StringPrintf("stockfish%d", level);
  }
  
  string Desc() const override {
    return StringPrintf("Stockfish engine, no tables, level %d",
			level);
  }
  
  const int level;
  std::unique_ptr<Stockfish> fish;
};

struct WorstfishPlayer : public EvalResultPlayer {

  WorstfishPlayer(int level) : EvalResultPlayer(), level(level) {
    fish.reset(new Stockfish(level));
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


Player *CreateStockfish0() {
  return new StockfishPlayer(0);
}

Player *CreateStockfish5() {
  return new StockfishPlayer(5);
}

Player *CreateStockfish10() {
  return new StockfishPlayer(10);
}

Player *CreateStockfish15() {
  return new StockfishPlayer(15);
}

Player *CreateStockfish20() {
  return new StockfishPlayer(20);
}


Player *CreateWorstfish() {
  return new WorstfishPlayer(20);
}
