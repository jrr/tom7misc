
#include "player.h"

#include <string>
#include <mutex>
#include <cstdint>

#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

using namespace std;
using int64 = int64_t;
using Move = Position::Move;

namespace {

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

static int TypeValue(uint8 t) {
  switch (t) {
  case Position::PAWN:
    return 1;

  case Position::BISHOP:
  case Position::KNIGHT:
    return 3;

  case Position::ROOK:
  case Position::C_ROOK:
    return 5;

  case Position::QUEEN:
    return 9;

  default:
    // Note, this includes king.
    return 0;
  }
}


#if 0
static bool IsMating(Position *pos, const Move &m) {
  return pos->MoveExcursion(m, [pos]() { return pos->IsMated(); });
}

static bool IsChecking(Position *pos, const Move &m) {
  return pos->MoveExcursion(m, [pos]() { return pos->IsInCheck(); });
}
#endif

struct FirstMovePlayer : public Player {

  static int WhiteCode(const Move &m) {
    int src = (int)m.src_row * 8 + (int)m.src_col;
    int dst = (int)m.dst_row * 8 + (int)m.dst_col;
    return (src * 64 + dst) * 8 + (int)m.promote_to;
  }

  static int BlackCode(const Move &m) {
    int src = (int)(7 - m.src_row) * 8 + (int)m.src_col;
    int dst = (int)(7 - m.dst_row) * 8 + (int)m.dst_col;
    return (src * 64 + dst) * 8 + (int)m.promote_to;
  }
  
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    std::vector<Move> legal = pos.GetLegalMoves();

    if (pos.BlackMove()) {
      return GetBest(legal, 
		     [](const Move &a, const Move &b) {
		       return BlackCode(a) < BlackCode(b);
		     });
    } else {
      return GetBest(legal, 
		     [](const Move &a, const Move &b) {
		       return WhiteCode(a) < WhiteCode(b);
		     });
    }
  }
  
  const char *Name() const override { return "first_move"; }
  const char *Desc() const override {
    return "Makes the lexicographically first legal move.";
  }
};

static std::mutex seed_m;
static string GetSeed() {
  static int64 counter = 0LL;
  int64 c = 0LL;
  seed_m.lock();
  c = counter++;
  seed_m.unlock();

  int64 t = time(nullptr);
  return StringPrintf("s%lld.%lld", c, t);
}

struct RandomPlayer : public Player {
  RandomPlayer() : rc(GetSeed()) {
    rc.Discard(800);
  }
    
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    std::vector<Move> legal = pos.GetLegalMoves();
    CHECK(!legal.empty());

    return legal[RandTo32(&rc, legal.size())];
  }
  
  const char *Name() const override { return "random_move"; }
  const char *Desc() const override {
    return "Choose a legal move, uniformly at random.";
  }

  ArcFour rc;
};

struct CCCPPlayer : public Player {

  struct LabeledMove {
    Move m;
    bool is_checkmate = false;
    bool is_check = false;
    // Or EMPTY if not capturing.
    uint8 captured = Position::EMPTY;
  };

  // Here, lower is better.
  static int CenterDistance(uint8 col) {
    switch (col) {
    default:
    case 0: return 3;
    case 1: return 2;
    case 2: return 1;
    case 3: return 0;
    case 4: return 0;
    case 5: return 1;
    case 6: return 2;
    case 7: return 3;
    }      
  }

  // This is just used to make a total order where we do
  // not have a preference. (XXX this produces pretty
  // different behavior between black and white...)
  static int MoveCode(const Move &m) {
    int src = (int)m.src_row * 8 + (int)m.src_col;
    int dst = (int)m.dst_row * 8 + (int)m.dst_col;
    return (src * 64 + dst) * 8 + (int)m.promote_to;
  }
  
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    bool black = pos.BlackMove();
    std::vector<LabeledMove> labeled;
    for (const Move &m : pos.GetLegalMoves()) {
      LabeledMove lm;
      lm.m = m;
      pos.MoveExcursion(m,
			[&pos, &lm]() {
			  lm.is_checkmate = pos.IsMated();
			  lm.is_check = pos.IsInCheck();
			  return 0;
			});
      lm.captured = pos.IsEnPassant(m) ? Position::PAWN :
	pos.PieceAt(m.dst_row, m.dst_col) & Position::TYPE_MASK;
      labeled.push_back(lm);
    }
    CHECK(!labeled.empty());

    return GetBest(labeled,
		   [black](const LabeledMove &a,
			   const LabeledMove &b) {
		     if (a.is_checkmate != b.is_checkmate)
		       return a.is_checkmate;

		     if (a.is_check != b.is_check)
		       return a.is_check;

		     // If capturing, prefer larger value!
		     // (XXX If multiple captures are available, use the
		     // lowest-value capturing piece?)
		     if (a.captured != b.captured)
		       return TypeValue(b.captured) < TypeValue(a.captured);

		     // Otherwise, prefer move depth.
		     if (a.m.dst_row != b.m.dst_row) {
		       if (black) {
			 // Prefer moving to larger rows
			 return b.m.dst_row < a.m.dst_row;
		       } else {
			 return a.m.dst_row < b.m.dst_row;
		       }
		     }

		     // Otherwise, prefer moving towards the
		     // center.
		     if (a.m.dst_col != b.m.dst_col) {
		       int acs = CenterDistance(a.m.dst_col);
		       int bcs = CenterDistance(b.m.dst_col);
		       if (acs != bcs)
			 return acs < bcs;
		     }

		     // Promote to the better piece.
		     if (a.m.promote_to != b.m.promote_to)
		       return TypeValue(b.m.promote_to & Position::TYPE_MASK) <
			 TypeValue(a.m.promote_to & Position::TYPE_MASK);

		     // Otherwise, we don't express a preference.
		     return MoveCode(a.m) < MoveCode(b.m);
		   }).m;
  }
  
  const char *Name() const override { return "cccp"; }
  const char *Desc() const override {
    return "Checkmate, check, capture, push.";
  }
};

// Base class for a player orders by some metric on the board
// state after the move, and breaks ties at random.
struct EvalResultPlayer : public Player {
  EvalResultPlayer() : rc(GetSeed()) {
    rc.Discard(800);
  }

  // With smaller scores being better.
  virtual int64 PositionPenalty(Position *p) = 0;
  
  struct LabeledMove {
    Move m;
    int64 penalty = 0.0;
    uint32 r = 0u;
  };

  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    std::vector<LabeledMove> labeled;
    for (const Move &m : pos.GetLegalMoves()) {
      LabeledMove lm;
      lm.m = m;
      lm.r = Rand32(&rc);
      pos.MoveExcursion(m,
			[this, &pos, &lm]() {
			  lm.penalty = PositionPenalty(&pos);
			  return 0;
			});
      labeled.push_back(lm);
    }
    CHECK(!labeled.empty());

    return GetBest(labeled,
		   [](const LabeledMove &a,
		      const LabeledMove &b) {
		     if (a.penalty != b.penalty)
		       return a.penalty < b.penalty;

		     return a.r < b.r;
		   }).m;
  }
  
  ArcFour rc;
};

struct MinOpponentMovesPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    return p->NumLegalMoves();
  }
  
  const char *Name() const override { return "min_opponent_moves"; }
  const char *Desc() const override {
    return "Take a random move that minimizes the opponent's number "
      "of legal moves.";
  }
};

struct SuicideKingPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    // Distance is symmetric, so we don't care which is "my" king.
    int br, bc, wr, wc;
    std::tie(br, bc) = p->GetKing(true);
    std::tie(wr, wc) = p->GetKing(false);
    // "Chebyshev distance" which is like Manhattan distance but
    // with diagonal moves.
    return std::max(std::abs(br - wr), std::abs(bc - wc));
  }
  
  const char *Name() const override { return "suicide_king"; }
  const char *Desc() const override {
    return "Take a random move that minimizes the distance "
      "between the two kings.";
  }
};

// Return the distance between the two given squares in Knight's
// moves, on an infinite chessboard, with no obstructions.
// (Note that unlike other metrics, the borders really do change
// the meaning here. The distance from (0,0) to (1,1) on a
// real chessboard is 4 (I think) but this function returns 2
// by allowing you to momentarily jump off the board.)
static int KnightDistance(int r1, int c1, int r2, int c2) {
  int r = std::abs(r2 - r1);
  int c = std::abs(c2 - c1);

  static constexpr int kDistance[64] = {
    0, 3, 2, 3, 2, 3, 4, 5,  
    3, 2, 1, 2, 3, 4, 3, 4, 
    2, 1, 4, 3, 2, 3, 4, 5, 
    3, 2, 3, 2, 3, 4, 3, 4, 
    2, 3, 2, 3, 4, 3, 4, 5, 
    3, 4, 3, 4, 3, 4, 5, 4, 
    4, 3, 4, 3, 4, 5, 4, 5, 
    5, 4, 5, 4, 5, 4, 5, 6, 
  };
  return kDistance[r * 8 + c];
}

struct ReverseStartingPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    int64 dist = 0LL;
    // We're evaluating after making the move, so my pieces are
    // the opposite color of the current move.
    const bool black = !p->BlackMove();
    const int my_mask = black ? Position::BLACK : Position::WHITE;
    const int pawn_row = black ? 6 : 1;
    const int back_row = black ? 7 : 0;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	uint8 piece = p->PieceAt(r, c);
	if ((piece & Position::COLOR_MASK) == my_mask) {
	  switch (piece & Position::TYPE_MASK) {
	  default:
	  case Position::EMPTY:
	    break;
	  case Position::PAWN:
	    dist += std::abs(r - pawn_row);
	    break;
	  case Position::C_ROOK:
	  case Position::ROOK:
	    dist += std::abs(r - back_row);
	    dist += std::min(std::abs(c - 0),
			     std::abs(c - 7));
	    break;
	    
	  case Position::KNIGHT:
	    dist += std::min(KnightDistance(r, c,
					    back_row, 1),
			     KnightDistance(r, c,
					    back_row, 6));
	    break;

	  case Position::QUEEN:
	    dist += std::max(std::abs(r - back_row),
			     std::abs(c - 3));
	    break;

	  case Position::KING:
	    dist += std::max(std::abs(r - back_row),
			     std::abs(c - 4));
	    break;

	  case Position::BISHOP: {
	    // Bishops can't leave their color, so we say that the
	    // black bishop is trying to get to c1 (which also mirrors
	    // horizontally).
	    const bool black_bishop =
	      Position::IsBlackSquare(r, c);
	    const int dest_col = black_bishop ?
	      (black ? 2 : 5) :
	      (black ? 5 : 2);

	    // Distance along row and column.
	    const int dr = std::abs(r - back_row);
	    const int dc = std::abs(c - dest_col);

	    // Assuming the square is reachable (which will be true by
	    // construction above), it follows a very regular pattern,
	    // with shells that simply increase by 1.
	    //
	    // 0  -  2  -  4  -
	    //
	    // -  1  -  3  -  5
	    //
	    // 2  -  2  -  4  -
	    //
	    // -  3  -  3  -  5
	    //
	    // 4  -  4  -  4  -
	    //
	    // -  5  -  5  -  5
				    
	    dist += std::max(dr, dc);
	    break;
	  }
	  }
	}
      }
    }
    return dist;
  }
  
  const char *Name() const override { return "reverse_starting"; }
  const char *Desc() const override {
    return "Try to move pieces such that they mirror the starting "
      "position, but in the opponent's camp.";
  }
};

struct GenerousPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    // This will be increasingly negative (better), the more material
    // we can capture.
    int64 dist = 0LL;
    // From the opponent's perspective. Of the current legal moves,
    // how many capture pieces? Repeatedly count capturing the same
    // piece, since this is even more generous to random players.
    for (const Move &m : p->GetLegalMoves()) {
      uint8 pd = p->PieceAt(m.dst_row, m.dst_col);
      if (pd == Position::EMPTY) {
	if (p->IsEnPassant(m)) dist--;
      } else {
	dist -= TypeValue(pd & Position::TYPE_MASK);
      }
    }
    
    return dist;
  }
  
  const char *Name() const override { return "generous"; }
  const char *Desc() const override {
    return "Try to move pieces so that they can be legally captured "
      "by the opponent.";
  }
};



// TODO:
//  - Stockfish, with and without opening book / endgame tablebases
//  - Maximize net control of squares
//  - Try to move your pieces to certain squares:
//       - put your pieces on squares of your color, or opponent color
//       - try to get all pieces close to the center
//  - attack the opponent's piece of maximum value with your piece
//    of minimum value

}  // namespace

Player *CreateFirstMove() {
  return new FirstMovePlayer;
}

Player *CreateRandom() {
  return new RandomPlayer;
}

Player *CreateCCCP() {
  return new CCCPPlayer;
}

Player *CreateMinOpponentMoves() {
  return new MinOpponentMovesPlayer;
}

Player *CreateSuicideKing() {
  return new SuicideKingPlayer;
}

Player *CreateReverseStarting() {
  return new ReverseStartingPlayer;
}

Player *CreateGenerous() {
  return new GenerousPlayer;
}
