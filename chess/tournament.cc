
// Run a tournament of players to determine their Elo ratings.

#include <mutex>
#include <memory>
#include <unordered_map>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"

#include "chess.h"
#include "player.h"
#include "stockfish-player.h"
#include "uci-player.h"
#include "chessmaster.h"

#define TESTING true

using Move = Position::Move;
using namespace std;

// Number of round-robin rounds.
// (Maybe should be based on the total number of games we want
// to simulate?)
static constexpr int TOTAL_ROUNDS = 40; // 250;
static constexpr int THREADS = 40;
static constexpr int ROUNDS_PER_THREAD = TOTAL_ROUNDS / THREADS;

typedef Player *(*Entrant)();
// using Entrant = Player *(*)();
const vector<Entrant> &GetEntrants() {
  static vector<Entrant> *entrants =
    new vector<Entrant>{
			/*
			CreateWorstfish,
			CreateRandom,
			CreateFirstMove,
			CreateAlphabetical,
			CreateCCCP,
			CreateNoIInsist,
			CreatePacifist,
			CreateSameColor,
			CreateMirrorYSymmetry,
			CreateMirrorXSymmetry,
			CreateSymmetry180,
			CreateOppositeColor,
			CreateSuicideKing,
			CreateReverseStarting,
			*/
			CreateMinOpponentMoves,
			// CreateTopple1M,
			CreateTopple10K,
			CreateHuddle,
			// CreateSwarm,
			// CreateGenerous,
			// CreateSinglePlayer,
			CreateChessmaster1,
			// CreateStockfish0,
			// CreateStockfish5,
			// CreateStockfish10,
			// CreateStockfish15,
			CreateStockfish20,
  };
  return *entrants;
}

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

struct Cell {
  int row_wins = 0, row_losses = 0, draws = 0;
  vector<Move> example_white_win, example_white_loss, example_white_draw;
  vector<Move> example_black_win, example_black_loss, example_black_draw;
};

static void TournamentThread(int thread_id,
			     vector<Cell> *outcomes) {
  // Create thread-local instances of each entrant.
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  const int num_entrants = entrants.size();

  CHECK(outcomes->size() == num_entrants * num_entrants);

  int64 last_message = time(nullptr);
  for (int round = 0; round < ROUNDS_PER_THREAD; round++) {
    for (int row = 0; row < num_entrants; row++) {
      for (int col = 0; col < num_entrants; col++) {
	// Maybe don't pit a player against itself? We ignore
	// them in elo calculations anyway.

	Cell *cell = &(*outcomes)[row * num_entrants + col];

	// One game as white, one game as black.
	{
	  vector<Move> as_white;
	  switch (PlayGame(entrants[row], entrants[col], &as_white)) {
	  case Result::WHITE_WINS:
	    if (cell->example_white_win.empty())
	      cell->example_white_win = as_white;
	    cell->row_wins++;
	    break;
	  case Result::BLACK_WINS:
	    if (cell->example_white_loss.empty())
	      cell->example_white_loss = as_white;
	    cell->row_losses++;
	    break;
	  
	  case Result::DRAW_STALEMATE:
	  case Result::DRAW_75MOVES:
	  case Result::DRAW_5REPETITIONS:
	    if (cell->example_white_draw.empty())
	      cell->example_white_draw = as_white;
	    cell->draws++;
	    break;
	  }
	}
	
	{
	  vector<Move> as_black;
	  switch (PlayGame(entrants[col], entrants[row], &as_black)) {
	  case Result::WHITE_WINS:
	    if (cell->example_black_loss.empty())
	      cell->example_black_loss = as_black;
	    cell->row_losses++;
	    break;
	  case Result::BLACK_WINS:
	    if (cell->example_black_win.empty())
	      cell->example_black_win = as_black;
	    cell->row_wins++;
	    break;
	  
	  case Result::DRAW_STALEMATE:
	  case Result::DRAW_75MOVES:
	  case Result::DRAW_5REPETITIONS:
	    if (cell->example_black_draw.empty())
	      cell->example_black_draw = as_black;
	    cell->draws++;
	    break;
	  }
	}
      }
    }

    int64 now = time(nullptr);
    if (now - last_message > 10) {
      printf("[%d] %d/%d %d%%\n", thread_id, round, ROUNDS_PER_THREAD,
	     (round * 100) / ROUNDS_PER_THREAD);
      fflush(stdout);
      last_message = now;
    }
  }
  
  for (Player *p : entrants) delete p;
  entrants.clear();
}

static constexpr double ELO_START = 1000.0;

struct Elo {
  double elo = ELO_START;
  // Used to determine k-factor.
  int games = 0;
  // Totals.
  int wins = 0, losses = 0, draws = 0;
};

// With the win/loss/draw matrix, compute elo ratings for each player.
static vector<Elo> ComputeElo(int num_entrants, const std::vector<Cell> &orig_outcomes) {
  CHECK(orig_outcomes.size() == num_entrants * num_entrants);
  // We don't need the examples, but it's easiest to just copy
  // everything. We will modify wins/losses/draws to replay the games.
  std::vector<Cell> outcomes = orig_outcomes;
  
  vector<Elo> elos;
  elos.resize(num_entrants);
  
  ArcFour rc(StringPrintf("elo.%lld", (int64)time(nullptr)));
  // Perform iterative updates in random order.
  vector<pair<int, int>> nonempty_matchups;
  // Initialize, ignoring self-play.
  for (int white = 0; white < num_entrants; white++) {
    for (int black = 0; black < num_entrants; black++) {
      if (white != black) {
	Cell *cell = &outcomes[white * num_entrants + black];
	if (cell->row_wins != 0 ||
	    cell->row_losses != 0 ||
	    cell->draws != 0) {
	  nonempty_matchups.emplace_back(white, black);
	}
      }
    }
  }

  auto ClaimResult =
    [&rc](Cell *cell) {
      // Pick uniformly in proportion to the number of games
      // played, not the three (non-empty) categories.
      int total_mass = cell->row_wins + cell->row_losses + cell->draws;
      CHECK(total_mass > 0) << "Invariant";
      int idx = RandTo32(&rc, total_mass);
      if (idx < cell->row_wins) {
	cell->row_wins--;
	return 1;
      }
      idx -= cell->row_wins;
      if (idx < cell->row_losses) {
	cell->row_losses--;
	return -1;
      }
      idx -= cell->row_losses;
      CHECK(idx < cell->draws) << "Bug? " << idx << " " << cell->draws;
      cell->draws--;
      return 0;
    };
  
  while (!nonempty_matchups.empty()) {
    Shuffle(&rc, &nonempty_matchups);
    vector<pair<int, int>> next_matchups;
    for (pair<int, int> p : nonempty_matchups) {
      const int white = p.first, black = p.second;
      Cell *cell = &outcomes[white * num_entrants + black];
      // Randomly pick one of the wins, losses or draws. 1 means
      // 1 white wins.
      const int result = ClaimResult(cell);
      
      const double q_white = pow(10.0, elos[white].elo / 400.0);
      const double q_black = pow(10.0, elos[black].elo / 400.0);
      // Expected score.
      const double e_white = q_white / (q_white + q_black);
      const double e_black = 1.0 - e_white;
      // Actual score.
      double s_white = 0.0, s_black = 0.0;
      switch (result) {
      case -1:
	s_black = 1.0;
	elos[white].losses++;
	elos[black].wins++;
	break;
      case 1:
	s_white = 1.0;
	elos[white].wins++;
	elos[black].losses++;
	break;
      default:
      case 0:
	s_white = 0.5;
	s_black = 0.5;
	elos[white].draws++;
	elos[black].draws++;
      }

      // TODO: Perhaps should use diminishing k as games go on.
      // Modulating this based on the rating does not make that much sense to me;
      // I think this is intended to capture the fact that true human skill changes
      // over time. Our computer players are not like that.
      const double k_white = 10.0;
      const double k_black = 10.0;

      elos[white].elo += k_white * (s_white - e_white);
      elos[black].elo += k_black * (s_black - e_black);
      
      elos[white].games++;
      elos[black].games++;

      // Only keep it around if there are more games to simulate.
      if (cell->row_wins > 0 ||
	  cell->row_losses > 0 ||
	  cell->draws > 0) {
	next_matchups.push_back(p);
      }
    }
    nonempty_matchups = std::move(next_matchups);
  }
  return elos;
}

// TODO...
static void RunTournament() {
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  int num_entrants = entrants.size();

  int actual_rounds = THREADS * ROUNDS_PER_THREAD;
  int games_per_round = 2 * num_entrants * num_entrants;
  printf("Will run %d rounds, each %d^2 x 2 = %d games,\n"
	 "for a total of %d games.\n",
	 actual_rounds, num_entrants, games_per_round,
	 games_per_round * actual_rounds);
  fflush(stdout);
  
  auto AddOutcomes =
    [](const vector<Cell> &a,
       const vector<Cell> &b) {
      vector<Cell> res = a;
      CHECK(a.size() == b.size());
      for (int i = 0; i < a.size(); i++) {
	res[i].row_wins += b[i].row_wins;
	res[i].row_losses += b[i].row_losses;
	res[i].draws += b[i].draws;

#       define MELD(field) \
	do { if (res[i]. field .empty()) res[i]. field = b[i].field; }	\
	while(0)
	MELD(example_white_win);
	MELD(example_white_loss);
	MELD(example_white_draw);
	MELD(example_black_win);
	MELD(example_black_loss);
	MELD(example_black_draw);
#       undef MELD
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

  printf("Running elo:\n");
  fflush(stdout);
  const int64 start_elo = time(nullptr);
  const vector<Elo> elos = ComputeElo(num_entrants, outcomes);
  printf("Done in %lld sec.\n", time(nullptr) - start_elo);
  fflush(stdout);
    
  // Print the matrix!
  string prelude = Util::ReadFile("tournament-prelude.html");
  FILE *f = fopen("tournament.html", "wb");
  CHECK(f);
  
  fprintf(f, "<!doctype html>\n"
	  "<meta charset=\"utf-8\" />\n"
	  "%s", prelude.c_str());

  fprintf(f, "<script>\n"
	  "const cells = [\n");
  for (int i = 0; i < outcomes.size(); i++) {
    fprintf(f, " {");

    auto Field =
      [f](const string &s,
	  const std::vector<Move> &moves) {
	Position pos;
	string pgn;
	// must also work for the case that moves is empty (no example)
	for (int n = 0; n < moves.size(); n++) {
	  if (n % 2 == 0)
	    pgn += StringPrintf(" %d.", (n >> 1) + 1);
	  pgn += StringPrintf(" %s", pos.ShortMoveString(moves[n]).c_str());
	  CHECK(pos.IsLegal(moves[n])) << pos.BoardString() << "\n"
				       << pos.LongMoveString(moves[n]);
	  pos.ApplyMove(moves[n]);
	}
	fprintf(f, "%s: '%s', ", s.c_str(), pgn.c_str());
      };
    const Cell &cell = outcomes[i];
    Field("ww", cell.example_white_win);
    Field("wl", cell.example_white_loss);
    Field("wd", cell.example_white_draw);
    Field("bw", cell.example_black_win);
    Field("bl", cell.example_black_loss);
    Field("bd", cell.example_black_draw);
    fprintf(f, "}");
    if (i != outcomes.size() - 1) fprintf(f, ",\n");
  }
  fprintf(f, "];\n</script>\n\n");

  // U+FF3C fullwidth reverse solidus
  fprintf(f, "<table><tr><td>white \uFF3C black</td>\n");
  for (Player *p : entrants)
    fprintf(f, " <td>%s</td>\n", p->Name().c_str());
  fprintf(f, "</tr>\n");
  for (int row = 0; row < num_entrants; row++) {
    fprintf(f, "<tr><td>%s</td>\n", entrants[row]->Name().c_str());
    for (int col = 0; col < num_entrants; col++) {
      int idx = row * num_entrants + col;
      const Cell &cell = outcomes[idx];
      fprintf(f, "  <td id=\"c%d\"><span class=\"c\" onclick=\"show(%d)\">"
	      "%d w, %d l, %d d</span></td>\n",
	      idx, idx,
	      cell.row_wins, cell.row_losses, cell.draws);
    }
    fprintf(f, "</tr>\n");
  }
  fprintf(f, "</table>\n");


  // TODO: Sort elos!
  fprintf(f, "<table><tr><td>player</td><td>elo</td><td>w/l/d</tr>\n");
  for (int i = 0; i < num_entrants; i++) {
    fprintf(f, " <tr><td>%s</td><td>%.6f</td><td>%d/%d/%d</td></tr>\n",
	    entrants[i]->Name().c_str(),
	    elos[i].elo,
	    elos[i].wins,
	    elos[i].losses,
	    elos[i].draws);
  }
  fprintf(f, "</table>\n");

  fprintf(f, "<div id=\"detail\"></div>\n");
  fclose(f);

  for (Player *p : entrants) delete p;
  entrants.clear();
}


int main(int argc, char **argv) {
  RunTournament();
  return 0;
}

