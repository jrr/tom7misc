
// Benchmark of Player, which also acts as a benchmark of chess.h code.

#include <mutex>
#include <memory>
#include <unordered_map>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/util.h"

#include "chess.h"
#include "player.h"
#include "blind/timer.h"

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

Result PlayGame(Player *white_player, Player *black_player,
		vector<Move> *moves) {
  Position pos;

  std::unique_ptr<PlayerGame> white{white_player->CreateGame()};
  std::unique_ptr<PlayerGame> black{black_player->CreateGame()};
  
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
      Move m = white->GetMove(pos, nullptr);
      if (pos.IsCapturing(m) ||
	  pos.IsPawnMove(m)) {
	white_stale_moves = 0;
      } else {
	white_stale_moves++;
      }

      white->ForceMove(pos, m);
      black->ForceMove(pos, m);
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
      Move m = black->GetMove(pos, nullptr);
      if (pos.IsCapturing(m) ||
	  pos.IsPawnMove(m)) {
	black_stale_moves = 0;
      } else {
	black_stale_moves++;
      }
      
      white->ForceMove(pos, m);
      black->ForceMove(pos, m);

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

static void PlayerBenchmark() {
  std::unique_ptr<Player> white{SinglePlayer()};
  std::unique_ptr<Player> black{SinglePlayer()};
  
  int white_wins = 0, black_wins = 0, draws = 0;
  int total_moves = 0;

  // const int64 start_time = time(nullptr);
  Timer bench_timer;
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

    printf("%d games, %d moves\n", game + 1, total_moves);
    fflush(stdout);
  }
  // const double seconds = time(nullptr) - start_time;
  const double seconds = bench_timer.MS() / 1000.0;
  printf("Total time: %.2fs (%.2f moves/sec)\n",
	 seconds, total_moves / seconds);
}

static void ExcursionBenchmark() {
  std::vector<Position> positions;
  auto AddPosition = [&positions](const char *fen) {
      Position p;
      CHECK(Position::ParseFEN(fen, &p)) << fen;
      positions.push_back(p);
    };
  // Just a bunch of positions from chess_test etc.
  AddPosition("8/8/7k/8/7K/8/2p5/8 b - 0 1");
  AddPosition("8/2P5/7k/8/7K/8/8/8 w - 0 1");
  AddPosition("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  AddPosition("3K4/5p2/6q1/8/4q3/8/6k1/8 b - - 9 60");
  AddPosition("4R3/pp1k4/2p2p2/8/1P6/4N3/P2r1r2/3RK3 b - - 5 30");
  AddPosition("1rbqkbnr/pPpppppp/8/P7/8/8/2PPPPPP/RNBQKBNR w KQk - 1 7");
  AddPosition("rnbq1bnr/ppppkppp/8/8/4PpP1/8/PPPPK2P/RNBQ1BNR b - g3 0 4");
  AddPosition(
      "r2qk2r/pbpnbppp/1p2pn2/3p4/Q1PP4/2N2NP1/PP1BPPBP/R3K2R w KQkq - 2 9");
  AddPosition(
      "r2qk2r/p1pnbppp/1pQ1pn2/3pN3/2PP4/2N3P1/PP1BPPBP/R3K2R b KQkq - 0 10");
  AddPosition(
      "r3k2r/p1Pnbppp/2B1p3/6P1/1P1q4/2N5/P2BPP1P/R3K2R b KQq - 0 19");
  AddPosition(
      "2kr3r/p1bn1ppp/4p3/1B4P1/1P1q4/2N5/P2BPP1P/R4RK1 w - - 2 22");
  AddPosition(
      "2k5/p4p2/4pB2/2N3p1/1P3b2/5n2/P4K2/3R4 b - - 0 38");
  AddPosition(
      "b2r3r/k4p1p/p2q1np1/NppP4/3p1Q2/P4PPB/1PP4P/1K1RR3 w - - 1 24");
  AddPosition(
      "r4b1r/pp1n2p1/1qp1k2p/4p3/3P4/2P5/P1P1Q1PP/1RB2RK1 b - - 2 15");
  
  Timer bench_timer;
  static constexpr int LOOPS = 100000;
  for (int i = 0; i < LOOPS; i++) {
    for (const Position &orig_pos : positions) {
      Position excursion_pos = orig_pos;
      for (const Position::Move &move : excursion_pos.GetLegalMoves()) {
	Position apply_pos = orig_pos;
	apply_pos.ApplyMove(move);
	excursion_pos.MoveExcursion(move, [&](){
	    CHECK(PositionEq{}(apply_pos, excursion_pos)) <<
	      "Should have applied move:\n" <<
	      apply_pos.BoardString() << "\n" <<
	      excursion_pos.BoardString();
	    return 0;
	  });
	CHECK(PositionEq{}(orig_pos, excursion_pos)) <<
	  "Should have restored state:\n" <<
	  orig_pos.BoardString() << "\n" <<
	  excursion_pos.BoardString();
      }
    }
  }

  const double seconds = bench_timer.MS() / 1000.0;
  printf("Total time: %.2fs\n", seconds);
}

int main(int argc, char **argv) {
  (void)PlayerBenchmark;
  (void)ExcursionBenchmark;
  
  ExcursionBenchmark();
  return 0;
}

