
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"

#include "../chess.h"
#include "../pgn.h"
#include "../bigchess.h"
#include "timer.h"

#include "unblinder.h"
#include "unblinder-mk0.h"

using namespace std;

using int64 = int64_t;

// If false, we don't even look at the game.
static bool Eligible(const PGN &pgn) {
  // Ignore games that don't finish.
  if (pgn.result == PGN::Result::OTHER) {
    return false;
  }
    
  if (pgn.GetTermination() != PGN::Termination::NORMAL) {
    return false;
  }

  return true;
}


static vector<Position> LoadPositions(const string &filename,
				      int64 num_positions,
				      float sample_rate,
				      const string &seed) {
  vector<Position> positions;
  positions.reserve(num_positions);
  ArcFour rc{seed};
  PGNTextStream pgnstream{filename.c_str()};
  PGNParser parser;
  string pgn_text;
  while (pgnstream.NextPGN(&pgn_text)) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));
    if (!Eligible(pgn)) continue;

    Position pos;
    for (int i = 0; i < pgn.moves.size(); i++) {
      const PGN::Move &m = pgn.moves[i];
      Position::Move move;
      const bool move_ok = pos.ParseMove(m.move.c_str(), &move);

      if (!move_ok) {
	fprintf(stderr, "Bad move %s from full PGN:\n%s",
		m.move.c_str(), pgn_text.c_str());
	goto next_pgn;
      }

      pos.ApplyMove(move);

      if (RandFloat(&rc) < sample_rate) {
	positions.push_back(pos);
	if (positions.size() >= num_positions) return positions;
      }
    }

  next_pgn:;
  }
  return positions;
}

bool PieceMatch(uint8 p1, uint8 p2) {
  if ((p1 & Position::COLOR_MASK) != (p2 & Position::COLOR_MASK))
    return false;

  p1 &= Position::TYPE_MASK;
  p2 &= Position::TYPE_MASK;

  if (p1 == p2)
    return true;
  
  if (p1 == Position::C_ROOK && p2 == Position::ROOK)
    return true;
  if (p1 == Position::ROOK && p2 == Position::C_ROOK)
    return true;

  return false;
}

int main(int argc, char **argv) {

  Timer positions_timer;
  vector<Position> positions = 
    LoadPositions(
	"d:/chess/lichess_db_standard_rated_2018-02.pgn",
	10000,
	0.01f,
	"eval");
  fprintf(stderr, "Loaded %lld positions in %.2fs\n",
	  (int64)positions.size(), positions_timer.MS() / 1000.0);
  fflush(stderr);
  
  Timer model_timer;
  std::unique_ptr<Unblinder> unblinder{UnblinderMk0::LoadFromFile("net.val")};
  fprintf(stderr, "Loaded model in %.2fs\n",
	  model_timer.MS() / 1000.0);
  fflush(stderr);
  
  Timer eval_timer;
  std::shared_mutex counters_m;
  int64 total_positions = 0LL;
  // Positions where we got everything correct.
  int64 exactly_correct = 0LL;
  // Potentially lots of mistakes per position.
  int64 piece_mistakes = 0LL;
  int64 castling_mistakes = 0LL;
  int64 move_mistakes = 0LL;
  auto AppMe =
    [&unblinder, &counters_m, &total_positions, &exactly_correct,
     &piece_mistakes, &castling_mistakes, &move_mistakes](
	const Position &pos) {
      uint64 bits = Unblinder::Blind(pos);
      // XXX something is wrong! Error rate is almost as high (27.24 vs 27.61)
      // as just guessing a totally empty board, which we definitely do better
      // than. I guess maybe it's not being simulated correctly?
      Position guess = unblinder->Unblind(bits);

      int piecem = 0;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  // This includes the castling state check.
	  if (!PieceMatch(pos.PieceAt(r, c), guess.PieceAt(r, c)))
	    piecem++;
	}
      }

      int castlem = 0;
      if (pos.CanStillCastle(false, false) != guess.CanStillCastle(false, false))
	castlem++;
      if (pos.CanStillCastle(false, true) != guess.CanStillCastle(false, true))
	castlem++;
      if (pos.CanStillCastle(true, false) != guess.CanStillCastle(true, false))
	castlem++;
      if (pos.CanStillCastle(true, true) != guess.CanStillCastle(true, true))
	castlem++;
      
      int movem = 0;
      if (pos.BlackMove() != guess.BlackMove())
	movem++;

      const int tot = piecem + castlem + movem;
      {
	WriteMutexLock ml(&counters_m);
	total_positions++;
	if (tot == 0) exactly_correct++;
	piece_mistakes += piecem;
	castling_mistakes += castlem;
	move_mistakes += movem;
      }
    };

  ParallelApp(positions, AppMe, 40);
  fprintf(stderr, "Ran eval in %.2fs\n", eval_timer.MS() / 1000.0);
  fflush(stderr);
  
  int64 total_mistakes = piece_mistakes + castling_mistakes + move_mistakes;
  
  printf("In %lld positions:\n"
	 "  %lld exactly correct (%.2f%%)\n"
	 "  %lld piece mistakes (%.2f/pos)\n"
	 "  %lld castling mistakes (%.2f/pos)\n"
	 "  %lld move mistakes (%.2f/pos)\n"
	 "  %lld total mistakes (%.2f/pos)\n",
	 total_positions,
	 exactly_correct, (exactly_correct * 100.0) / total_positions,
	 piece_mistakes, (piece_mistakes / (double)total_positions),
	 castling_mistakes, (castling_mistakes / (double)total_positions),
	 move_mistakes, (move_mistakes / (double)total_positions),
	 total_mistakes, (total_mistakes / (double)total_positions));

  return 0;
}

