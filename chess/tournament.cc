
// Run a tournament of players to determine their Elo ratings.

#include <memory>
#include <unordered_map>

#include "../cc-lib/base/logging.h"

#include "chess.h"
#include "player.h"

#define TESTING true

using Move = Position::Move;
using namespace std;

// Under FIDE rules, after 50 moves without a pawn move or capture, a
// player may CLAIM a draw. We don't allow these computer players to
// claim draws, so the relevant rule is actually 9.6.2, where the game
// is automatically a draw after *75* such moves.
// Similarly, threefold repetition allows a claim, and fivefold
// repetition forces the draw.

enum class Result {
  WHITE_WINS,
  BLACK_WINS,
  // Different forms of draw.
  DRAW_STALEMATE,
  DRAW_75MOVES,
  DRAW_5REPETITIONS,
  // DRAW_INSUFFICIENT,
};

Result PlayGame(Player *white, Player *black) {
  Position pos;

  // For implementing 75-move rule.
  int white_stale_moves = 0;
  int black_stale_moves = 0;

  // For implementing 5-repetition rule.
  // Note that positions are only inserted *after* a move,
  // not counting the starting position; this is deliberate
  // and appears to be the correct interpretation of FIDE rules.
  std::unordered_map<Position, int, PositionHash, PositionEq>
    position_counts;
  
  // TODO: Draw by insufficient material. (But we can
  // be guaranteed that these eventually end in draws by
  // the 75-move rule.)
  
  auto DrawByRule75 =
    [&white_stale_moves,
     &black_stale_moves]() {
      return (white_stale_moves >= 75 &&
	      black_stale_moves >= 75);
    };

  int movenum = 0;
  for (;;) {
    movenum++;
    if (TESTING) { CHECK(!pos.BlackMove()); }

    {
      Move m = white->MakeMove(pos);
      if (TESTING) { CHECK(pos.IsLegal(m)); }
      if (pos.IsCapturing(m) ||
	  pos.IsPawnMove(m)) {
	white_stale_moves = 0;
      } else {
	white_stale_moves++;
      }

      printf("%d. %s ", movenum, pos.ShortMoveString(m).c_str());
      pos.ApplyMove(m);

      // Checkmate takes precedence over draw by
      // 75 moves.

      if (!pos.HasLegalMoves()) {
	if (pos.IsInCheck())
	  return Result::WHITE_WINS;

	return Result::DRAW_STALEMATE;
      }

      int &count = position_counts[pos];
      count++;
      if (count == 5)
	return Result::DRAW_5REPETITIONS;
      
      if (DrawByRule75())
	return Result::DRAW_75MOVES;

      // printf("%s\n\n", pos.BoardString().c_str());
    }
      
    if (TESTING) { CHECK(pos.BlackMove()); }
    
    {
      Move m = black->MakeMove(pos);
      if (TESTING) { CHECK(pos.IsLegal(m)); }
      if (pos.IsCapturing(m) ||
	  pos.IsPawnMove(m)) {
	black_stale_moves = 0;
      } else {
	black_stale_moves++;
      }

      printf("%s ", pos.ShortMoveString(m).c_str());
      pos.ApplyMove(m);
    
      if (!pos.HasLegalMoves()) {
	if (pos.IsInCheck())
	  return Result::BLACK_WINS;

	return Result::DRAW_STALEMATE;
      }

      int &count = position_counts[pos];
      count++;
      if (count == 5)
	return Result::DRAW_5REPETITIONS;
      
      if (DrawByRule75())
	return Result::DRAW_75MOVES;

      // printf("%s\n\n", pos.BoardString().c_str());
    }
  }
}

// TODO...


int main(int argc, char **argv) {
  std::unique_ptr<Player> rplayer{CreateRandom()};
  std::unique_ptr<Player> fmplayer{CreateFirstMove()};
  std::unique_ptr<Player> cccpplayer{CreateCCCP()};

  switch (PlayGame(fmplayer.get(), fmplayer.get())) {
  case Result::WHITE_WINS:
    printf("White wins!\n");
    break;
  case Result::BLACK_WINS:
    printf("Black wins!\n");
    break;
  case Result::DRAW_STALEMATE:
    printf("Draw (stalemate)!\n");
    break;
  case Result::DRAW_75MOVES:
    printf("Draw (75 moves)!\n");
    break;
  case Result::DRAW_5REPETITIONS:
    printf("Draw (5-fold repetition)!\n");
    break;
  }
  
  // implement!
  return 0;
}

