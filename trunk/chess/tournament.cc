
// Run a tournament of players to determine their Elo ratings.

#include <mutex>
#include <memory>
#include <unordered_map>

#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

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
#include "tournament-db.h"

#define TESTING true

// Cursor to beginning of previous line
#define ANSI_PREVLINE "\x1B[F"
#define ANSI_CLEARLINE "\x1B[2K"
#define ANSI_CLEARTOEOL "\x1B[0K"

#define ANSI_RED "\x1B[1;31;40m"
#define ANSI_GREY "\x1B[1;30;40m"
#define ANSI_BLUE "\x1B[1;34;40m"
#define ANSI_CYAN "\x1B[1;36;40m"
#define ANSI_YELLOW "\x1B[1;33;40m"
#define ANSI_GREEN "\x1B[1;32;40m"
#define ANSI_WHITE "\x1B[1;37;40m"
#define ANSI_PURPLE "\x1B[1;35;40m"
#define ANSI_RESET "\x1B[m"

using Move = Position::Move;
using namespace std;

// Number of round-robin rounds.
// (Maybe should be based on the total number of games we want
// to simulate?)
static constexpr int TOTAL_ROUNDS = 108; // 250;
static constexpr int THREADS = 54;
static constexpr int ROUNDS_PER_THREAD = TOTAL_ROUNDS / THREADS;

typedef Player *(*Entrant)();
// using Entrant = Player *(*)();
const vector<Entrant> &GetEntrants() {
  static vector<Entrant> *entrants =
    new vector<Entrant>{
			CreateWorstfish,
			CreateRandom,
			// CreateFirstMove,
			// CreateAlphabetical,
			CreateCCCP,
			CreateNoIInsist,
			CreatePacifist,
			CreateSameColor,
			// CreateMirrorYSymmetry,
			// CreateMirrorXSymmetry,
			CreateSymmetry180,
			CreateOppositeColor,
			CreateSuicideKing,
			CreateReverseStarting,
			// CreateMinOpponentMoves,
			// CreateHuddle,
			CreateSwarm,
			CreateGenerous,
			// CreateTopple1M,
			// CreateTopple10K,
			// CreateSinglePlayer,
			// CreateChessmaster1,
			// CreateChessmaster2,
			// CreateStockfish0,
			// CreateStockfish5,
			// CreateStockfish10,
			// CreateStockfish15,
			// CreateStockfish20,
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

// Protects status.
std::mutex status_m;
struct Status {
  int total_games = 0;
  int done_games = 0;
  string row;
  string msg;
};

vector<Status> status;
static void ShowStatus() {
  MutexLock ml(&status_m);
  for (int i = 0; i < status.size() + 2; i++) {
    printf("%s", ANSI_PREVLINE);
  }
  int64 total_all = 0, done_all = 0;
  for (int i = 0; i < status.size(); i++) {
    total_all += status[i].total_games;
    done_all += status[i].done_games;
  }
  
  printf("\n----------- %lld / %lld ------------------" ANSI_CLEARTOEOL "\n",
	 done_all, total_all);
  for (int i = 0; i < status.size(); i++) {
    printf(ANSI_GREY "[" ANSI_BLUE "% 2d. " ANSI_YELLOW "%.1f%%" ANSI_GREY "] " ANSI_GREEN "%s: "
	   ANSI_WHITE "%s" ANSI_CLEARTOEOL "\n",
	   i,
	   (status[i].done_games * 100.0) / status[i].total_games,
	   status[i].row.c_str(),
	   status[i].msg.c_str());
  }
}

static string RenderMoves(const vector<Move> &moves) {
  Position pos;
  string pgn;
  // must also work for the case that moves is empty (no example)
  for (int n = 0; n < moves.size(); n++) {
    if (!pgn.empty()) pgn.push_back(' ');
    if (n % 2 == 0)
      pgn += StringPrintf("%d.", (n >> 1) + 1);
    pgn += StringPrintf(" %s", pos.ShortMoveString(moves[n]).c_str());
    CHECK(pos.IsLegal(moves[n])) << pos.BoardString() << "\n"
				 << pos.LongMoveString(moves[n]);
    pos.ApplyMove(moves[n]);
  }
  return pgn;
}

static void TournamentThread(int thread_id,
			     Outcomes *outcomes) {
  // Create thread-local instances of each entrant.
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  const int num_entrants = entrants.size();

  // CHECK(outcomes->size() == num_entrants * num_entrants);

  {
    MutexLock ml(&status_m);
    status[thread_id].msg = "start";
    status[thread_id].total_games = num_entrants * num_entrants * ROUNDS_PER_THREAD;
  }

  int games_done = 0;
  int64 last_message = 0LL; // time(nullptr);
  for (int round = 0; round < ROUNDS_PER_THREAD; round++) {
    for (int row_offset = 0; row_offset < num_entrants; row_offset++) {
      // Try to get different threads running different cells.
      // Some have lower utilization than others.
      const int row = (thread_id + row_offset) % num_entrants;

      string white_name = entrants[row]->Name();

      int64 now = time(nullptr);
      if (now - last_message > 1) {
	{
	  MutexLock ml(&status_m);
	  status[thread_id].msg = "running";
	  status[thread_id].row = white_name;
	  status[thread_id].done_games = games_done;
	}
	ShowStatus();
	last_message = now;
      }
      
      for (int col = 0; col < num_entrants; col++) {
	// Maybe don't pit a player against itself? We ignore
	// them in elo calculations anyway.

	string black_name = entrants[col]->Name();
	
	Cell *cell = &(*outcomes)[make_pair(white_name, black_name)];

	// Row is always white and col is black; there is a symmetric
	// cell for the reverse. (On the diagonal, we get half as
	// many self-play games as other cells, but this is fine since
	// we don't use these for elo.)

	vector<Move> as_white;
	switch (PlayGame(entrants[row], entrants[col], &as_white)) {
	case Result::WHITE_WINS:
	  if (cell->example_win.empty())
	    cell->example_win = RenderMoves(as_white);
	  cell->white_wins++;
	  break;
	case Result::BLACK_WINS:
	  if (cell->example_loss.empty())
	    cell->example_loss = RenderMoves(as_white);
	  cell->white_losses++;
	  break;
	  
	case Result::DRAW_STALEMATE:
	case Result::DRAW_75MOVES:
	case Result::DRAW_5REPETITIONS:
	  if (cell->example_draw.empty())
	    cell->example_draw = RenderMoves(as_white);
	  cell->draws++;
	  break;
	}

	games_done++;
      }
    }
  }

  {
    MutexLock ml(&status_m);
    status[thread_id].msg = "done";
    status[thread_id].row = "-";
    status[thread_id].done_games = games_done;
  }
  
  for (Player *p : entrants) delete p;
  entrants.clear();
}

static void RunTournament() {
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  int num_entrants = entrants.size();

  int actual_rounds = THREADS * ROUNDS_PER_THREAD;
  int games_per_round = num_entrants * num_entrants;
  printf("Will run %d rounds, each %d^2 = %d games,\n"
	 "for a total of %d games.\n",
	 actual_rounds, num_entrants, games_per_round,
	 games_per_round * actual_rounds);
  fflush(stdout);
  
  auto AddOutcomes =
    [](const Outcomes &a,
       const Outcomes &b) {
      Outcomes res = b;
      TournamentDB::MergeInto(a, &res);
      return res;
    };

  status.resize(THREADS);

  Outcomes previous_outcomes = TournamentDB::LoadFromFile("tournament.db");
  
  Outcomes outcomes =
    ParallelAccumulate(
	THREADS,
	Outcomes{},
	AddOutcomes,
	TournamentThread,
	THREADS);

  TournamentDB::MergeInto(previous_outcomes, &outcomes);
  
  TournamentDB::SaveToFile(outcomes, "tournament.db");
  
  for (Player *p : entrants) delete p;
  entrants.clear();
}


int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }

  // Turn on ANSI support in Windows 10+. (Otherwise, use ANSICON etc.)
  // https://docs.microsoft.com/en-us/windows/console/setconsolemode
  //
  // TODO: This works but seems to subsequently break control keys
  // and stuff like that in cygwin bash?
  HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
  // mingw headers may not know about this new flag
  static constexpr int kVirtualTerminalProcessing = 0x0004;
  DWORD old_mode = 0;
  GetConsoleMode(hStdOut, &old_mode);
  // printf("%lld\n", old_mode);
  SetConsoleMode(hStdOut, old_mode | kVirtualTerminalProcessing);
  #endif

  RunTournament();
  return 0;
}

