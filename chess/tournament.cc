
// Run a tournament of players to determine their Elo ratings.

#include <mutex>
#include <memory>
#include <unordered_map>
#include <cstdlib>
#include <unordered_set>

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
#include "../cc-lib/gtl/top_n.h"

#include "chess.h"
#include "player.h"
#include "stockfish-player.h"
#include "uci-player.h"
#include "chessmaster.h"
#include "fate-player.h"
#include "tournament-db.h"
#include "player-util.h"
#include "blind-player.h"

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

// This used to be round-robin style, but since the work grows
// quadratically, it got to the point that running even a single
// round would take hours. New version picks cells that are 
static constexpr int THREADS = 54;
static constexpr int RUN_FOR_SECONDS = 60 * 20;

// Fill in the diagonal just a little. After reaching this number of games,
// we don't bother prioritizing self-play at all.
static constexpr int SELFPLAY_TARGET = 5;

typedef Player *(*Entrant)();

static Player *Stockfish1M_32768() {
  return new BlendRandom<32768>(Stockfish1M());
}
static Player *Stockfish1M_16384() {
  return new BlendRandom<16384>(Stockfish1M());
}
static Player *Stockfish1M_8192() {
  return new BlendRandom<8192>(Stockfish1M());
}
static Player *Stockfish1M_4096() {
  return new BlendRandom<4096>(Stockfish1M());
}
static Player *Stockfish1M_2048() {
  return new BlendRandom<2048>(Stockfish1M());
}
static Player *Stockfish1M_1024() {
  return new BlendRandom<1024>(Stockfish1M());
}
static Player *Stockfish1M_512() {
  return new BlendRandom<512>(Stockfish1M());
}
static Player *Stockfish1M_256() {
  return new BlendRandom<256>(Stockfish1M());
}
static Player *Stockfish1M_128() {
  return new BlendRandom<128>(Stockfish1M());
}
static Player *Stockfish1M_64() {
  return new BlendRandom<64>(Stockfish1M());
}

// 			CreateSinglePlayer,
const vector<Entrant> &GetEntrants() {
  static vector<Entrant> *entrants =
    new vector<Entrant>{
			Safe,
			Dangerous,
			Popular,
			Rare,
			Survivalist,
			Fatalist,
			Equalizer,

			BlindYolo,
			BlindSingleKings,
			
			Worstfish,

			MinOpponentMoves,
			MirrorYSymmetry,
			MirrorXSymmetry,
			Random,
			Symmetry180,
			FirstMove,
			Alphabetical,
			Pacifist,
			Generous,
			NoIInsist,
			SameColor,
			OppositeColor,
			Huddle,
			Swarm,
			SuicideKing,
			ReverseStarting,
			CCCP,

			Topple10K,
			Topple1M,
			Chessmaster1,
			Chessmaster2,

			Stockfish0,
			Stockfish5,
			Stockfish10,
			Stockfish15,
			Stockfish20,
			
			Stockfish1M,

			Stockfish1M_32768,
			Stockfish1M_16384,
			Stockfish1M_8192,
			Stockfish1M_4096,
			Stockfish1M_2048,
			Stockfish1M_1024,
			Stockfish1M_512,
			Stockfish1M_256,
			Stockfish1M_128,
			Stockfish1M_64,
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

Result PlayGame(Player *white_player, Player *black_player,
		vector<Move> *moves) {
  std::unique_ptr<PlayerGame> white{white_player->CreateGame()};
  std::unique_ptr<PlayerGame> black{black_player->CreateGame()};

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
      Move m = white->GetMove(pos);
      if (TESTING) { CHECK(pos.IsLegal(m)); }
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
      
    if (TESTING) { CHECK(pos.BlackMove()); }
    
    {
      Move m = black->GetMove(pos);
      if (TESTING) { CHECK(pos.IsLegal(m)); }
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

// Protects status.
std::mutex status_m;
int64 status_start_time = 0LL;
int64 status_last_time = 0LL;
struct Status {
  int done_games = 0;
  string row;
  string col;
  string msg;
};

vector<Status> status;
static void ShowStatus(int64 now) {
  MutexLock ml(&status_m);
  // Update at most once per second.
  if (now - status_last_time < 1)
    return;
  status_last_time = now;
  
  int64 total_seconds = now - status_start_time;

  int64 minutes = total_seconds / 60;
  int seconds = total_seconds % 60;
  
  for (int i = 0; i < status.size() + 2; i++) {
    printf("%s", ANSI_PREVLINE);
  }
  int64 done_all = 0;
  for (int i = 0; i < status.size(); i++) {
    done_all += status[i].done_games;
  }
  
  printf("\n----------- %lld done ----- "
	 ANSI_CYAN "%lld" ANSI_RESET "m" ANSI_CYAN "%d" ANSI_RESET "s"
	 ANSI_WHITE " -----------" ANSI_CLEARTOEOL "\n",
	 done_all, minutes, seconds);
  for (int i = 0; i < status.size(); i++) {
    printf(ANSI_GREY "[" ANSI_BLUE "% 2d. " ANSI_YELLOW "%d"
	   ANSI_GREY "] " ANSI_GREEN "%s" ANSI_WHITE " vs " ANSI_GREEN "%s: "
	   ANSI_WHITE "%s" ANSI_CLEARTOEOL "\n",
	   i,
	   status[i].done_games,
	   status[i].row.c_str(),
	   status[i].col.c_str(),
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

struct Totals {
  // Coarse locking.
  std::mutex m;
  const int num;
  const vector<string> names;
  vector<int64> totals;
  vector<std::pair<int, int>> todo;
  Totals(const vector<string> &n,
	 const Outcomes &outcomes) : num(n.size()),
				     names(n),
				     totals(n.size() * n.size(), 0) {
    todo.reserve(THREADS);
    for (int white = 0; white < num; white++) {
      for (int black = 0; black < num; black++) {
	auto it = outcomes.find(make_pair(names[white], names[black]));
	if (it != outcomes.end()) {
	  const Cell &cell = it->second;
	  totals[white * num + black] =
	    cell.white_wins + cell.white_losses + cell.draws;
	}
      }
    }
  }

  void Increment(int white, int black) {
    MutexLock ml(&m);
    totals[white * num + black]++;
  }

  std::pair<int, int> GetAssignment() {
    MutexLock ml(&m);
    // If we have anything enqueued, return it.
    if (!todo.empty()) {
      auto ret = todo.back();
      todo.pop_back();
      return ret;
    }

    struct TCell {
      TCell(int w, int b, int64 t) : c(w, b), total(t) {}
      std::pair<int, int> c;
      int64 total;
    };
    struct TCellCmp {
      bool operator()(const TCell &a, const TCell &b) {
	return a.total < b.total;
      };
    };
      
    gtl::TopN<TCell, TCellCmp> topn(THREADS);
    
    // Otherwise, loop over the table and get the N neediest
    // cells.
    for (int white = 0; white < num; white++) {
      for (int black = 0; black < num; black++) {
	// Could use DoRun filterting here.
	int64 t = totals[white * num + black];
	if (white != black || t < SELFPLAY_TARGET) {
	  topn.push(TCell(white, black, t));
	}
      }
    }

    std::unique_ptr<std::vector<TCell>> tops{topn.Extract()};
    for (const TCell &tc : *tops) {
      todo.push_back(tc.c);
    }

    CHECK(!todo.empty());
    auto ret = todo.back();
    todo.pop_back();
    return ret;
  }
};

static void TournamentThread(int thread_id,
			     Totals *totals,
			     Outcomes *outcomes) {
  // Create thread-local instances of each entrant.
  // TODO: Lazily construct these.
  vector<Player *> entrants;
  for (Entrant e : GetEntrants()) {
    entrants.push_back(e());
  }
  const int num_entrants = entrants.size();
  (void)num_entrants;
  
  {
    MutexLock ml(&status_m);
    status[thread_id].msg = "start";
  }

  const int64 start_time = time(nullptr);
  
  int games_done = 0;
  int64 last_message = 0LL; // time(nullptr);

  for (;;) {
    const int64 now = time(nullptr);
    if (now - start_time > RUN_FOR_SECONDS) {
      break;
    }

    int white, black;
    std::tie(white, black) = totals->GetAssignment();
    const string white_name = entrants[white]->Name();
    const string black_name = entrants[black]->Name();
    
    if (now - last_message > 1) {
      {
	MutexLock ml(&status_m);
	status[thread_id].msg = "running";
	status[thread_id].row = white_name;
	status[thread_id].col = black_name;
	status[thread_id].done_games = games_done;
      }
      ShowStatus(now);
      last_message = now;
    }

    // outcomes are accumulated locally.
    Cell *cell = &(*outcomes)[make_pair(white_name, black_name)];

    vector<Move> as_white;
    switch (PlayGame(entrants[white], entrants[black], &as_white)) {
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
    totals->Increment(white, black);
  }

  {
    MutexLock ml(&status_m);
    status[thread_id].msg = "done";
    status[thread_id].row = "-";
    status[thread_id].col = "-";
    status[thread_id].done_games = games_done;
  }
  ShowStatus(time(nullptr));
  
  for (Player *p : entrants) delete p;
  entrants.clear();
}

static void RunTournament() {
  vector<Player *> entrants;
  std::unordered_set<string> entrant_names;
  vector<string> names;
  for (Entrant e : GetEntrants()) {
    Player *p = e();
    entrants.push_back(p);
    string name = p->Name();
    CHECK(entrant_names.find(name) == entrant_names.end())
      << "Duplicate names: " << name;
    entrant_names.insert(name);
    names.push_back(name);
  }
  const int num_entrants = entrants.size();
  (void)num_entrants;
  
  fflush(stdout);
  
  auto AddOutcomes =
    [](const Outcomes &a,
       const Outcomes &b) {
      Outcomes res = b;
      TournamentDB::MergeInto(a, &res);
      return res;
    };

  status_start_time = time(nullptr);
  status.resize(THREADS);

  Outcomes previous_outcomes = TournamentDB::LoadFromFile("tournament.db");

  std::unique_ptr<Totals> totals =
    std::make_unique<Totals>(names, previous_outcomes);
  
  Outcomes outcomes =
    ParallelAccumulate(
	THREADS,
	Outcomes{},
	AddOutcomes,
	[&totals](int thread_id, Outcomes *outcomes) {
	  return TournamentThread(thread_id, totals.get(), outcomes);
	},
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

  // XXX: Topple sometimes hangs around even after we close its pipes
  // and try to kill the child process (probably I'm just doing it
  // wrong); kill them all (globally!)
  system("taskkill /F /IM topple_v0.3.5_znver1.exe /T");
  return 0;
}

