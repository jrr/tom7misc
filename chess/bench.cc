
// Benchmark of Player, which also acts as a benchmark of chess.h code.

#include <mutex>
#include <memory>
#include <unordered_map>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/util.h"

#include "chess.h"
#include "player.h"

using Move = Position::Move;
using namespace std;


enum class Result {
  WHITE_WINS,
  BLACK_WINS,
  // Different forms of draw.
  DRAW_STALEMATE,
  DRAW_75MOVES,
  DRAW_5REPETITIONS,
  // DRAW_INSUFFICIENT,
};

Result PlayGame(Player *white, Player *black, vector<Move> *moves) {
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
  
  // TODO: Draw by insufficient material. (But we can be guaranteed
  // that these eventually end in draws by the 75-move rule (or
  // perhaps some other draw rule earlier).)
  
  auto DrawByRule75 =
    [&white_stale_moves,
     &black_stale_moves]() {
      return (white_stale_moves >= 75 &&
	      black_stale_moves >= 75);
    };

  int movenum = 0;
  for (;;) {
    movenum++;

    {
      Move m = white->MakeMove(pos);
      if (pos.IsCapturing(m) ||
	  pos.IsPawnMove(m)) {
	white_stale_moves = 0;
      } else {
	white_stale_moves++;
      }

      pos.ApplyMove(m);
      moves->push_back(m);
      
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

    }
      
    {
      Move m = black->MakeMove(pos);
      if (pos.IsCapturing(m) ||
	  pos.IsPawnMove(m)) {
	black_stale_moves = 0;
      } else {
	black_stale_moves++;
      }

      pos.ApplyMove(m);
      moves->push_back(m);
      
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
    }
  }
}

static constexpr int NUM_GAMES = 10;

static void RunBenchmark() {
  std::unique_ptr<Player> white{CreateCCCP()};
  std::unique_ptr<Player> black{CreateSinglePlayer()};

  int white_wins = 0, black_wins = 0, draws = 0;
  int total_moves = 0;

  const int64 start_time = time(nullptr);
  for (int game = 0; game < NUM_GAMES; game++) {
    vector<Move> moves;
    switch (PlayGame(white.get(), black.get(), &moves)) {
    case Result::WHITE_WINS:
      white_wins++;
      break;
    case Result::BLACK_WINS:
      black_wins++;
      break;
    case Result::DRAW_STALEMATE:
    case Result::DRAW_75MOVES:
    case Result::DRAW_5REPETITIONS:
      draws++;
    }
    total_moves += moves.size();
  }
  const double seconds = time(nullptr) - start_time;
  printf("Total time: %.2fs (%.2f moves/sec)\n",
	 seconds, total_moves / seconds);
}


int main(int argc, char **argv) {
  RunBenchmark();
  return 0;
}

