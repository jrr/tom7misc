
// Run a tournament of players to determine their Elo ratings.

#include <mutex>
#include <memory>
#include <unordered_map>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/threadutil.h"

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

      // printf("%d. %s ", movenum, pos.ShortMoveString(m).c_str());
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

      // printf("%s ", pos.ShortMoveString(m).c_str());
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

typedef Player *(*Entrant)();
// using Entrant = Player *(*)();
const vector<Entrant> &GetEntrants() {
  static vector<Entrant> *entrants =
    new vector<Entrant>{CreateRandom,
			CreateFirstMove,
			CreateCCCP,
			CreateMinOpponentMoves};
  return *entrants;
}

struct Cell {
  int row_wins = 0, row_losses = 0, draws = 0;
};

// Number of round-robin rounds.
static constexpr int TOTAL_ROUNDS = 10000;
static constexpr int THREADS = 60;
static constexpr int ROUNDS_PER_THREAD = TOTAL_ROUNDS / THREADS;
static void TournamentThread(int thread_id,
			     vector<Cell> *outcomes) {
  // Create thread-local instances of each entrant.
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  const int num_entrants = entrants.size();

  CHECK(outcomes->size() == num_entrants * num_entrants);
  
  for (int round = 0; round < ROUNDS_PER_THREAD; round++) {
    for (int row = 0; row < num_entrants; row++) {
      for (int col = 0; col < num_entrants; col++) {
	// Maybe don't pit a player against itself? Unclear
	// how this would work in ratings.

	Cell *cell = &(*outcomes)[row * num_entrants + col];

	// One game as white, one game as black.
	switch (PlayGame(entrants[row], entrants[col])) {
	case Result::WHITE_WINS:
	  cell->row_wins++;
	  break;
	case Result::BLACK_WINS:
	  cell->row_losses++;
	  break;
	  
	case Result::DRAW_STALEMATE:
	case Result::DRAW_75MOVES:
	case Result::DRAW_5REPETITIONS:
	  cell->draws++;
	}

	switch (PlayGame(entrants[col], entrants[row])) {
	case Result::WHITE_WINS:
	  cell->row_losses++;
	  break;
	case Result::BLACK_WINS:
	  cell->row_wins++;
	  break;
	  
	case Result::DRAW_STALEMATE:
	case Result::DRAW_75MOVES:
	case Result::DRAW_5REPETITIONS:
	  cell->draws++;
	}
      }
    }
    if (round % 25 == 0) {
      printf("[%d] %d/%d %d%%\n", thread_id, round, ROUNDS_PER_THREAD,
	     (round * 100) / ROUNDS_PER_THREAD);
      fflush(stdout);
    }
  }
  
  for (Player *p : entrants) delete p;
}

// TODO...
static void RunTournament() {
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  int num_entrants = entrants.size();

  auto AddOutcomes =
    [](const vector<Cell> &a,
       const vector<Cell> &b) {
      vector<Cell> res = a;
      CHECK(a.size() == b.size());
      for (int i = 0; i < a.size(); i++) {
	res[i].row_wins += b[i].row_wins;
	res[i].row_losses += b[i].row_losses;
	res[i].draws += b[i].draws;
      }
      return res;
    };

  vector<Cell> outcomes =
    ParallelAccumulate(
	THREADS,
	vector<Cell>(num_entrants * num_entrants),
	AddOutcomes,
	TournamentThread,
	THREADS);
  CHECK(outcomes.size() == num_entrants * num_entrants);
  
  // Print the matrix!
  FILE *f = fopen("tournament.html", "wb");
  CHECK(f);
  fprintf(f, "<!doctype html>\n"
	  "<style>\n"
	  " table, th, td {\n"
	  "  border: 1px solid #AAA;\n"
	  "  border-collapse: collapse;\n"
	  " }\n"
	  " body { font: 14px sans-serif }\n"
	  "</style>\n");
  fprintf(f, "<table><tr><td>&nbsp;</td>\n");
  for (Player *p : entrants)
    fprintf(f, " <td>%s</td>\n", p->Name());
  fprintf(f, "</tr>\n");
  for (int row = 0; row < num_entrants; row++) {
    fprintf(f, "<tr><td>%s</td>\n", entrants[row]->Name());
    for (int col = 0; col < num_entrants; col++) {
      const Cell &cell = outcomes[row * num_entrants + col];
      fprintf(f, "  <td>%d w, %d l, %d d</td>\n",
	      cell.row_wins, cell.row_losses, cell.draws);
    }
    fprintf(f, "</tr>\n");
  }
  fprintf(f, "</table>\n");
  fclose(f);
}


int main(int argc, char **argv) {
#if 0
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
#endif

  RunTournament();
  // implement!
  return 0;
}

