
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
  
  virtual Move MakeMove(const Position &orig_pos) override {
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
    
  virtual Move MakeMove(const Position &orig_pos) override {
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

  static int PieceValue(uint8 p) {
    switch (p) {
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
  
  virtual Move MakeMove(const Position &orig_pos) override {
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
		     if (a.captured != b.captured)
		       return PieceValue(b.captured) < PieceValue(a.captured);

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
		     if (b.m.promote_to != a.m.promote_to)
		       return PieceValue(b.m.promote_to) <
			 PieceValue(a.m.promote_to);

		     // Otherwise, we don't express a preference.
		     return MoveCode(a.m) < MoveCode(b.m);
		   }).m;
  }
  
  const char *Name() const override { return "cccp"; }
  const char *Desc() const override {
    return "Checkmate, check, capture, push.";
  }
};


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
