
#include "player.h"

#include <string>
#include <mutex>
#include <cstdint>
#include <memory>

#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "player-util.h"

using namespace std;
using int64 = int64_t;
using Move = Position::Move;

namespace {

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

struct FirstMovePlayer : public StatelessPlayer {

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
      return PlayerUtil::GetBest(
	  legal, 
	  [](const Move &a, const Move &b) {
	    return BlackCode(a) < BlackCode(b);
	  });
    } else {
      return PlayerUtil::GetBest(
	  legal, 
	  [](const Move &a, const Move &b) {
	    return WhiteCode(a) < WhiteCode(b);
	  });
    }
  }
  
  string Name() const override { return "first_move"; }
  string Desc() const override {
    return "Makes the lexicographically first legal move.";
  }
};

struct RandomPlayer : public StatelessPlayer {
  RandomPlayer() : rc(PlayerUtil::GetSeed()) {
    rc.Discard(800);
  }
    
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    std::vector<Move> legal = pos.GetLegalMoves();
    CHECK(!legal.empty());

    return legal[RandTo32(&rc, legal.size())];
  }
  
  string Name() const override { return "random_move"; }
  string Desc() const override {
    return "Choose a legal move, uniformly at random.";
  }

  ArcFour rc;
};

struct CCCPPlayer : public StatelessPlayer {

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

    return PlayerUtil::GetBest(
	labeled,
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
  
  string Name() const override { return "cccp"; }
  string Desc() const override {
    return "Checkmate, check, capture, push.";
  }
};

struct AlphabeticalPlayer : public StatelessPlayer {

  struct LabeledMove {
    Move m;
    string move_string;
  };
  
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    std::vector<LabeledMove> labeled;
    for (const Move &m : pos.GetLegalMoves()) {
      LabeledMove lm;
      lm.m = m;
      lm.move_string = pos.ShortMoveString(m);
      labeled.push_back(lm);
    }
    CHECK(!labeled.empty());

    return PlayerUtil::GetBest(
	labeled,
	[](const LabeledMove &a,
	   const LabeledMove &b) {
	  return a.move_string < b.move_string;
	}).m;
  }
  
  string Name() const override { return "alphabetical"; }
  string Desc() const override {
    return "Return the alphabetically earliest move, using "
      "standard algebraic notation.";
  }
};

struct PacifistPlayer : public EvalResultPlayer {

  int64 PositionPenalty(Position *p) override {
    if (p->IsMated())
      return 0xFFFF'FFFF'FFFF;
    int64 material = 0LL;
    const uint8 opponent_mask = p->BlackMove() ? Position::BLACK : Position::WHITE;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	const uint8 piece = p->PieceAt(r, c);
	if (piece != Position::EMPTY &&
	    (piece & Position::COLOR_MASK) == opponent_mask) {
	  material += TypeValue(piece & Position::TYPE_MASK);
	}
      }
    }

    // Negate opponent material value to avoid capturing.
    // Then we lexicographically avoid checking by giving a large
    // additional penalty in that case (but still we can tiebreak
    // checks by not capturing, or capturing weak pieces).
    if (p->IsInCheck()) {
      return 1000000 - material;
    } else {
      return -material;
    }
  }
  
  string Name() const override { return "pacifist"; }
  string Desc() const override {
    return "Only make checking or capturing moves if forced.";
  }
};


struct MinOpponentMovesPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    return p->NumLegalMoves();
  }
  
  string Name() const override { return "min_oppt_moves"; }
  string Desc() const override {
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
  
  string Name() const override { return "suicide_king"; }
  string Desc() const override {
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
  
  string Name() const override { return "reverse_starting"; }
  string Desc() const override {
    return "Try to move pieces such that they mirror the starting "
      "position, but in the opponent's camp.";
  }
};

struct HuddlePlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *pos) override {
    // (Reversed since we are on the opposite move now.)
    const bool black = !pos->BlackMove();
    const uint8 my_mask = black ? Position::BLACK : Position::WHITE;

    int kr, kc;
    std::tie(kr, kc) = pos->GetKing(black);
    
    int64 dist = 0LL;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	uint8 piece = pos->PieceAt(r, c);
	if (piece != Position::EMPTY &&
	    (piece & Position::COLOR_MASK) == my_mask) {
	  dist += std::max(std::abs(r - kr),
			   std::abs(c - kc));
	}
      }
    }
    return dist;
  }
  
  string Name() const override { return "huddle"; }
  string Desc() const override {
    return "Try to move pieces to surround our own king.";
  }
};

struct SwarmPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *pos) override {
    // (Reversed since we are on the opposite move now.)
    const bool black = !pos->BlackMove();
    const uint8 my_mask = black ? Position::BLACK : Position::WHITE;

    // Same as Huddle, but target the enemy's king.
    int kr, kc;
    std::tie(kr, kc) = pos->GetKing(!black);
    
    int64 dist = 0LL;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	uint8 piece = pos->PieceAt(r, c);
	if (piece != Position::EMPTY &&
	    (piece & Position::COLOR_MASK) == my_mask) {
	  dist += std::max(std::abs(r - kr),
			   std::abs(c - kc));
	}
      }
    }
    return dist;
  }
  
  string Name() const override { return "swarm"; }
  string Desc() const override {
    return "Try to move pieces to surround the opponent's king.";
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
  
  string Name() const override { return "generous"; }
  string Desc() const override {
    return "Try to move pieces so that they can be legally captured "
      "by the opponent.";
  }
};

struct NoIInsistPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    // This is just like "generous," but tries to constrain the opponent
    // to *have* to capture. There are three tiers:
    //   - Player is now mated. This would be quite rude, so it gets
    //     the highest penalty.
    //   - Every legal move captures one of our pieces. This is
    //     the most polite because it is impossible for the opponent to
    //     refuse our gift. In this case, the score is the negative
    //     of the value of the worst gift.
    //   - Otherwise, the expected value of the gift.

    int total_capture = 0;
    int min_capture = 0;
    // From the opponent's perspective. Of the current legal moves,
    // how many capture pieces? Repeatedly count capturing the same
    // piece, since this is even more generous to random players.
    const std::vector<Move> moves = p->GetLegalMoves();
    for (const Move &m : moves) {
      uint8 pd = p->PieceAt(m.dst_row, m.dst_col);
      if (pd == Position::EMPTY) {
	if (p->IsEnPassant(m)) {
	  min_capture = std::min(min_capture, 1);
	  total_capture++;
	} else {
	  min_capture = 0;
	}
      } else {
	int value = TypeValue(pd & Position::TYPE_MASK);
	min_capture = std::min(min_capture, value);
	total_capture += value;
      }
    }

    const int num_moves = moves.size();
    if (num_moves == 0) {
      if (p->IsInCheck()) {
	return 0xFFFF'FFFF'FFFF;
      } else {
	// Also penalize stalemate, but not as much. It is more
	// canonical for two polite players to form a draw by
	// repetition, from continually offering up material to one
	// another and declining it.
	return 0xFFFFFFFF;
      }
    } else if (min_capture > 0) {
      return -min_capture;
    } else {
      // Otherwise, the expected value.
      double expected = (total_capture * 1000000.0) / num_moves;
      return -expected;
    }
  }
  
  string Name() const override { return "no_i_insist"; }
  string Desc() const override {
    return "Move such that the opponent's moves have the highest "
      "forced material gain (or else average gain).";
  }
};


struct SameColorPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    int64 penalty = 0LL;
    // Just reduce the penalty (always non-positive) for each piece on
    // the same-color square. Increasing the penalty would encourage
    // us to sac pieces.

    // (Reversed since we are on the following move now.)
    const bool black = !p->BlackMove();
    const uint8 my_mask = black ? Position::BLACK : Position::WHITE;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	const uint8 piece = p->PieceAt(r, c);
	if (piece != Position::EMPTY &&
	    (piece & Position::COLOR_MASK) == my_mask) {
	  if (black == Position::IsBlackSquare(r, c)) penalty--;
	}
      }
    }
    return penalty;
  }
  
  string Name() const override { return "same_color"; }
  string Desc() const override {
    return "If white, put pieces on white squares.";
  }
};

struct OppositeColorPlayer : public EvalResultPlayer {
  int64 PositionPenalty(Position *p) override {
    int64 penalty = 0LL;
    // Just reduce the penalty (always non-positive) for each piece on
    // the same-color square. Increasing the penalty would encourage
    // us to sac pieces.

    // (Reversed since we are on the following move now.)
    const bool black = !p->BlackMove();
    const uint8 my_mask = black ? Position::BLACK : Position::WHITE;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	const uint8 piece = p->PieceAt(r, c);
	if (piece != Position::EMPTY &&
	    (piece & Position::COLOR_MASK) == my_mask) {
	  if (black != Position::IsBlackSquare(r, c)) penalty--;
	}
      }
    }
    return penalty;
  }
  
  string Name() const override { return "opposite_color"; }
  string Desc() const override {
    return "If white, put pieces on black squares.";
  }
};

struct SymmetryPlayer : public EvalResultPlayer {
  // Get the actual piece at the location symmetric to r, c.
  virtual uint8 BizarroPieceAt(Position *pos, int r, int c) = 0;
  
  int64 PositionPenalty(Position *pos) override {
    int64 penalty = 0LL;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	const uint8 piece = pos->PieceAt(r, c);
	const uint8 other = BizarroPieceAt(pos, r, c);
	if (piece == Position::EMPTY && other == Position::EMPTY)
	  continue;
	if (piece == Position::EMPTY) {
	  penalty += 10;
	  continue;
	}
	if (other == Position::EMPTY) {
	  penalty += 10;
	  continue;
	}
	// They should be opposite colors.
	if ((piece & Position::COLOR_MASK) ==
	    (other & Position::COLOR_MASK)) {
	  penalty += 5;
	  continue;
	}
	// But the same type.
	if ((piece & Position::TYPE_MASK) !=
	    (other & Position::TYPE_MASK)) {
	  penalty++;
	  continue;
	}
      }
    }
    return penalty;
  }
};

struct MirrorYSymmetryPlayer : public SymmetryPlayer {
  uint8 BizarroPieceAt(Position *pos, int r, int c) override {
    return pos->PieceAt(7 - r, c);
  }
  string Name() const override { return "sym_mirror_y"; }
  string Desc() const override {
    return "Move to maximize symmetry (mirroring along the "
      "y axis).";
  }
};

struct MirrorXSymmetryPlayer : public SymmetryPlayer {
  uint8 BizarroPieceAt(Position *pos, int r, int c) override {
    return pos->PieceAt(r, 7 - c);
  }
  string Name() const override { return "sym_mirror_x"; }
  string Desc() const override {
    return "Move to maximize symmetry (mirroring along the "
      "x axis).";
  }
};

// a b c      i h g     (0,0) (0,1) (0,2)      (2,2) (2,1) (2,0)
// d e f  ->  f e d     (1,0) (1,1) (1,2)  ->  (1,2) (1,1) (1,0)
// g h i      c b a     (2,0) (2,1) (2,2)      (0,2) (0,1) (0,0)

struct Symmetry180Player : public SymmetryPlayer {
  uint8 BizarroPieceAt(Position *pos, int r, int c) override {
    return pos->PieceAt(7 - r, 7 - c);
  }
  string Name() const override { return "sym_180"; }
  string Desc() const override {
    return "Move to maximize symmetry (180 degree rotation).";
  }
};

struct SinglePlayerPlayer : public StatelessPlayer {
  explicit SinglePlayerPlayer(int max_depth) : 
    max_depth(max_depth), 
    rc(PlayerUtil::GetSeed()) {
    rc.Discard(800);
  }

  struct LabeledMove {
    Position::Move m;
    int64_t penalty = 0.0;
    uint32_t r = 0u;
  };
  
  static constexpr int64 CHECKMATE = -1000'000'000LL;
  
  int64 GetLowestPenalty(bool black, int depth, Position *p) {
    // Position should have the opponent's move.
    CHECK(p->BlackMove() != black);

    const int opponent_moves = p->NumLegalMoves();
    // If the opponent is in check, then it would be an illegal
    // position if we switched the side. So we just have to
    // return in such situations.
    if (p->IsInCheck())
      return (opponent_moves == 0) ? CHECKMATE : opponent_moves;

    // Otherwise, temporarily set the game to be my turn again.
    p->SetBlackMove(black);
    int64 lowest_penalty = 1000'000'000LL;
    for (const Move &m : p->GetLegalMoves()) {
      p->MoveExcursion(
	  m, [this, black, depth, p, &lowest_penalty]() {
	       if (depth == 0) {
		 lowest_penalty =
		   std::min(lowest_penalty,
			    (int64)p->NumLegalMoves());
	       } else {
		 lowest_penalty =
		   std::min(lowest_penalty,
			    GetLowestPenalty(black, depth - 1, p));
	       }
	       return 0;
	     });
    }
    p->SetBlackMove(!black);
    return lowest_penalty;
  }
  
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;
    const bool black = pos.BlackMove();

    std::vector<LabeledMove> labeled;
    for (const Move &m : pos.GetLegalMoves()) {
      LabeledMove lm;
      lm.m = m;
      pos.MoveExcursion(m,
			[this, black, &pos, &lm]() {
			  lm.penalty = GetLowestPenalty(black, max_depth, &pos);
			  return 0;
			});
      labeled.push_back(lm);
    }
    CHECK(!labeled.empty());

    return PlayerUtil::GetBest(
      labeled,
      [](const LabeledMove &a,
	 const LabeledMove &b) {
	if (a.penalty != b.penalty)
	  return a.penalty < b.penalty;
	
	return a.r < b.r;
      }).m;
  }
  
  string Name() const override {
    return StringPrintf("single_player%d", max_depth); 
  }
  string Desc() const override {
    return StringPrintf("Search (to depth %d), but as though the "
			"opponent takes no turns.", max_depth);
  }

  const int max_depth;
  ArcFour rc;
};

// TODO:
//  - Stockfish, with and without opening book / endgame tablebases
//  - Maximize net control of squares
//  - Try to move your pieces to certain squares:
//       - try to get all pieces close to the center
//  - attack the opponent's piece of maximum value with your piece
//    of minimum value
//  - buddy system; if you are alone then only move next to a buddy.
//    if you are with a buddy, then you may move away
//  - prefer moves that can be mirrored by the opponent
//
//  - select eligible squares with langton's ant, game of life


}  // namespace


Player *FirstMove() {
  return new MakeStateless<FirstMovePlayer>;
}

Player *Random() {
  return new MakeStateless<RandomPlayer>;
}

Player *Alphabetical() {
  return new MakeStateless<AlphabeticalPlayer>;
}

Player *CCCP() {
  return new MakeStateless<CCCPPlayer>;
}

Player *Pacifist() {
  return new MakeStateless<PacifistPlayer>;
}

Player *MinOpponentMoves() {
  return new MakeStateless<MinOpponentMovesPlayer>;
}

Player *SuicideKing() {
  return new MakeStateless<SuicideKingPlayer>;
}

Player *ReverseStarting() {
  return new MakeStateless<ReverseStartingPlayer>;
}

Player *Huddle() {
  return new MakeStateless<HuddlePlayer>;
}

Player *Swarm() {
  return new MakeStateless<SwarmPlayer>;
}

Player *Generous() {
  return new MakeStateless<GenerousPlayer>;
}

Player *NoIInsist() {
  return new MakeStateless<NoIInsistPlayer>;
}

Player *SameColor() {
  return new MakeStateless<SameColorPlayer>;
}

Player *OppositeColor() {
  return new MakeStateless<OppositeColorPlayer>;
}

Player *MirrorYSymmetry() {
  return new MakeStateless<MirrorYSymmetryPlayer>;
}

Player *MirrorXSymmetry() {
  return new MakeStateless<MirrorXSymmetryPlayer>;
}

Player *Symmetry180() {
  return new MakeStateless<Symmetry180Player>;
}

Player *SinglePlayer() {
  return new MakeStateless<SinglePlayerPlayer, int>(1);
}
