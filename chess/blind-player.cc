
#include <string>
#include <memory>
#include <cstdint>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "subprocess.h"
#include "player.h"
#include "chess.h"
#include "player-util.h"
#include "stockfish.h"
#include "blind/unblinder-mk0.h"
#include "blind/unblinder.h"

using int64 = int64_t;

using Move = Position::Move;
using namespace std;

static constexpr bool VERBOSE = false;

namespace {

// Singleton since it allocates a large block of memory. Never freed.
static Unblinder *GetUnblinder() {
  static Unblinder *the_unblinder = UnblinderMk0::LoadFromFile("blind/net.val");
  return the_unblinder;
}

struct BlindPlayer : public StatelessPlayer {
  BlindPlayer(const string &name,
	      bool spycheck,
	      bool single_king) : name(name),
				  spycheck(spycheck),
				  single_king(single_king),
				  rc(PlayerUtil::GetSeed()) {
    fish.reset(new Stockfish(20, 1'000'000));
    CHECK(fish.get());
    unblinder = GetUnblinder();
  }

  static bool OK(Position *predicted) {
    // Check that castling flags are appropriate. UnblinderMk0 supposedly does
    // this, but stockfish is known to crash if given invalid castling flags in
    // FEN, so take care. (We could also just return false in these cases.)
    if (predicted->CanStillCastle(false, false)) {
      CHECK(predicted->PieceAt(0, 0) == (Position::C_ROOK | Position::BLACK) &&
	    predicted->PieceAt(0, 4) == (Position::KING | Position::BLACK));
    }
    if (predicted->CanStillCastle(false, true)) {
      CHECK(predicted->PieceAt(0, 7) == (Position::C_ROOK | Position::BLACK) &&
	    predicted->PieceAt(0, 4) == (Position::KING | Position::BLACK));
    }
    if (predicted->CanStillCastle(true, false)) {
      CHECK(predicted->PieceAt(7, 0) == (Position::C_ROOK | Position::WHITE) &&
	    predicted->PieceAt(7, 4) == (Position::KING | Position::WHITE));
    }
    if (predicted->CanStillCastle(true, true)) {
      CHECK(predicted->PieceAt(7, 7) == (Position::C_ROOK | Position::WHITE) &&
	    predicted->PieceAt(7, 4) == (Position::KING | Position::WHITE));
    }

    // There must be exactly one of each.
    int white_kings = 0, black_kings = 0;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	uint8 p = predicted->PieceAt(r, c);
	if ((p & Position::TYPE_MASK) == Position::KING) {
	  if ((p & Position::COLOR_MASK) == Position::BLACK) {
	    black_kings++;
	  } else {
	    white_kings++;
	  }
	}
      }
    }

    if (white_kings != 1 || black_kings != 1)
      return false;

    // Opponent king cannot be attacked.
    {
      // Temporarily swap sides.
      bool black = predicted->BlackMove();
      predicted->SetBlackMove(!black);

      bool invalid_check = predicted->IsInCheck();
      
      // Restore.
      predicted->SetBlackMove(black);
      if (invalid_check)
	return false;
    }

    // Finally, if no legal moves, then we would not get here and
    // it may not work to ask the engine to make a move.
    if (!predicted->HasLegalMoves())
      return false;

    return true;
  }
  
  Move MakeMove(const Position &orig_pos) override {
    const uint64 blinded = Unblinder::Blind(orig_pos);
    Position predicted = unblinder->Unblind(single_king, blinded);

    // HERE: Do spy check if enabled. Note that it would be
    // wrong for us to do this using the correct side-to-move, because
    // we don't actually have that information. (Below it can be seen
    // as an optimization, equivalent to running stockfish for both
    // possibilities, since the potentially legal move sets would be
    // completely disjoint.)
    if (spycheck) {
      // ... Possible to do this a symmetric way, though? I guess we
      // can just prioritize any legal move where the source and
      // destination pieces are predicted to be the same color, but
      // maybe put moves for the predicted side-to-move first. Also
      // makes sense to break ties by capturing with a weaker
      // (predicted) piece, probably.
    }

    // Force the current player.
    predicted.SetBlackMove(orig_pos.BlackMove());

    Position pos = orig_pos;
    if (OK(&predicted)) {
      string fen = predicted.ToFEN(10, 10);
      string move_s;
      Stockfish::Score score;
      fish->GetMove(fen, &move_s, &score);
      Move m;
      CHECK(PlayerUtil::ParseLongMove(move_s, predicted.BlackMove(), &m))
	<< predicted.BoardString()
	<< "\n" << fen
	<< "\n[" << move_s << "]";

      // Easy for move to be invalid if we mispredicted the board state.
      if (pos.IsLegal(m))
	return m;

      if (VERBOSE)
	printf("\nMove %s not legal. Predicted:\n%sActual:\n%s",
	       predicted.ShortMoveString(m).c_str(),
	       predicted.BoardString().c_str(),
	       orig_pos.BoardString().c_str());
    } else {
      if (VERBOSE)
	printf("\nPredicted board not OK:\n%sActual:\n%s",
	       predicted.BoardString().c_str(),
	       orig_pos.BoardString().c_str());
    }
    
    // Random move.
    std::vector<Position::Move> legal = pos.GetLegalMoves();
    CHECK(!legal.empty());
    return legal[RandTo32(&rc, legal.size())];
  }

  string Name() const override {
    return name;
  }

  string Desc() const override {
    return StringPrintf("Predict a board state%s.%s If valid (or can be made "
			"valid trivially), use stockfish1m to make a move. "
			"If move is invalid or other problem, random.",
			(spycheck ? " First, spy-check pieces that we think are our "
			 "own." : ""),
			(single_king ? "" : " with exactly one king per side"));
  }

  const string name;
  const bool spycheck = false;
  const bool single_king = false;
  ArcFour rc;
  std::unique_ptr<Stockfish> fish;
  // Not owned.
  Unblinder *unblinder = nullptr;
};

}  // namespace


Player *BlindYolo() {
  return new MakeStateless<BlindPlayer, string, bool, bool>("blind_yolo", false, false);
}

Player *BlindSingleKings() {
  return new MakeStateless<BlindPlayer, string, bool, bool>("blind_kings", false, true);
}

Player *BlindSpycheck() {
  return new MakeStateless<BlindPlayer, string, bool, bool>("blind_spycheck", true, true);
}
