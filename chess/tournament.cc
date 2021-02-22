
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
#include "almanac-player.h"
#include "numeric-player.h"
#include "letter-player.h"

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
// round would take hours. New version picks cells that have
// the fewest number of games, and runs those in parallel.
static constexpr int THREADS = 8;
static constexpr int RUN_FOR_SECONDS = 60 * 20;

// Fill in the diagonal just a little. After reaching this number of games,
// we don't bother prioritizing self-play at all.
static constexpr int SELFPLAY_TARGET = 5;

typedef Player *(*Entrant)();

static Player *Stockfish1M_64512() {
  return new BlendRandom<64512>(Stockfish1M());
}
static Player *Stockfish1M_63488() {
  return new BlendRandom<63488>(Stockfish1M());
}
static Player *Stockfish1M_61440() {
  return new BlendRandom<61440>(Stockfish1M());
}
static Player *Stockfish1M_57344() {
  return new BlendRandom<57344>(Stockfish1M());
}
static Player *Stockfish1M_49152() {
  return new BlendRandom<49152>(Stockfish1M());
}
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

static Player *LetterA() { return Letter(0); }
static Player *LetterB() { return Letter(1); }
static Player *LetterC() { return Letter(2); }
static Player *LetterD() { return Letter(3); }
static Player *LetterE() { return Letter(4); }
static Player *LetterF() { return Letter(5); }
static Player *LetterG() { return Letter(6); }
static Player *LetterH() { return Letter(7); }
static Player *LetterI() { return Letter(8); }
static Player *LetterJ() { return Letter(9); }
static Player *LetterK() { return Letter(10); }
static Player *LetterL() { return Letter(11); }
static Player *LetterM() { return Letter(12); }
static Player *LetterN() { return Letter(13); }
static Player *LetterO() { return Letter(14); }
static Player *LetterP() { return Letter(15); }
static Player *LetterQ() { return Letter(16); }
static Player *LetterR() { return Letter(17); }
static Player *LetterS() { return Letter(18); }
static Player *LetterT() { return Letter(19); }
static Player *LetterU() { return Letter(20); }
static Player *LetterV() { return Letter(21); }
static Player *LetterW() { return Letter(22); }
static Player *LetterX() { return Letter(23); }
static Player *LetterY() { return Letter(24); }
static Player *LetterZ() { return Letter(25); }

const vector<Entrant> &GetEntrants() {
  static vector<Entrant> *entrants =
    new vector<Entrant>{
                        LetterA,
                        LetterB,
                        LetterC,
                        LetterD,
                        LetterE,
                        LetterF,
                        LetterG,
                        LetterH,
                        LetterI,
                        LetterJ,
                        LetterK,
                        LetterL,
                        LetterM,
                        LetterN,
                        LetterO,
                        LetterP,
                        LetterQ,
                        LetterR,
                        LetterS,
                        LetterT,
                        LetterU,
                        LetterV,
                        LetterW,
                        LetterX,
                        LetterY,
                        LetterZ,

                        Safe,
                        Dangerous,
                        Popular,
                        Rare,
                        Survivalist,
                        Fatalist,
                        Equalizer,

                        BlindYolo,
                        BlindSingleKings,
                        BlindSpycheck,

                        Worstfish,

                        BinaryPi,
                        BinaryE,

                        RationalPi,
                        RationalE,

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

                        SinglePlayer,
                        AlmanacPopular,

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

                        Stockfish1M_64512,
                        Stockfish1M_63488,
                        Stockfish1M_61440,
                        Stockfish1M_57344,
                        Stockfish1M_49152,
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

  // TODO: Draw by insufficient material. But we can be guaranteed
  // that these eventually end in draws by the 75-move rule (or
  // perhaps some other draw rule) earlier.

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
      Move m = white->GetMove(pos, nullptr);
      if (TESTING) {
        CHECK(pos.IsLegal(m)) << pos.BoardString()
                              << "\n" << Position::DebugMoveString(m)
                              << "\n" << white_player->Name() << " vs "
                              << black_player->Name();

      }
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
      Move m = black->GetMove(pos, nullptr);
      if (TESTING) {
        CHECK(pos.IsLegal(m)) << pos.BoardString()
                              << "\n" << Position::DebugMoveString(m)
                              << "\n" << white_player->Name() << " vs "
                              << black_player->Name();
      }
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

// Wrapper around Outcomes with atomic access.
// In 2021, we want to be able to read/write this thing from the
// tournament threads, in order to avoid resimulating completely
// deterministic matchups.
struct OutcomeTable {
  explicit OutcomeTable(const Outcomes &outcomes) : outcomes(outcomes) {}

  template<class F>
  void WithCell(const string &white_name, const string &black_name,
                const F &f) {
    WriteMutexLock ml(&m);
    Cell *cell = &outcomes[make_pair(white_name, black_name)];
    f(cell);
  }

  bool HasResult(const string &white_name, const string &black_name) {
    ReadMutexLock ml(&m);
    auto it = outcomes.find(make_pair(white_name, black_name));
    if (it == outcomes.end()) return false;
    const Cell &cell = it->second;
    return cell.white_wins + cell.white_losses + cell.draws > 0;
  }


  std::shared_mutex m;
  Outcomes outcomes;
};

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

  // white, black, games played so far
  using Neediest = std::tuple<string, string, int64>;
  Neediest neediest = {"?", "?", 0LL};
  Neediest GetNeediest() {
    MutexLock ml(&m);
    return neediest;
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
    CHECK(!tops->empty());
    const TCell &most = (*tops)[0];
    neediest = make_tuple(names[most.c.first],
                          names[most.c.second], most.total);

    for (const TCell &tc : *tops) {
      todo.push_back(tc.c);
    }

    CHECK(!todo.empty());
    auto ret = todo.back();
    todo.pop_back();
    return ret;
  }
};

// Protects status.
std::shared_mutex status_m;
int64 status_start_time = 0LL;
int64 status_last_time = 0LL;
struct Status {
  int done_games = 0;
  // "free" means we just replicated an existing
  // deterministic result
  int free_games = 0;
  string row;
  string col;
  string msg;
  // time we started running the game
  int64 run_start = 0;
};

vector<Status> status;
// Gross that we send the Totals object, but it's easier than
// managing our own status thread...
static void ShowStatus(int64 now, Totals *totals, bool force_show = false) {
  // Update at most once per second.
  {
    ReadMutexLock ml(&status_m);
    if (now - status_last_time < 1)
      return;
  }

  Totals::Neediest neediest = totals->GetNeediest();

  WriteMutexLock ml(&status_m);
  // (Possible race...)
  if (now - status_last_time < 1)
    return;
  status_last_time = now;

  int64 total_seconds = now - status_start_time;

  auto AnsiMinSec = [](int64 total_seconds) {
      int64 minutes = total_seconds / 60;
      int seconds = total_seconds % 60;
      if (minutes == 0)
        return StringPrintf(ANSI_CYAN "%d" ANSI_RESET "s",
                            seconds);
      else
        return StringPrintf(ANSI_CYAN "%lld" ANSI_RESET "m"
                            ANSI_CYAN "%d" ANSI_RESET "s",
                            minutes, seconds);
    };

  for (int i = 0; i < status.size() + 3; i++) {
    printf("%s", ANSI_PREVLINE);
  }
  int64 done_all = 0, free_all = 0;
  for (int i = 0; i < status.size(); i++) {
    done_all += status[i].done_games;
    free_all += status[i].free_games;
  }

  printf("\n-------- %lld " ANSI_YELLOW " done " ANSI_RESET
         " -- %lld " ANSI_GREEN " free ----- "
         "%s"
         ANSI_WHITE " ---------" ANSI_CLEARTOEOL "\n",
         done_all, free_all, AnsiMinSec(total_seconds).c_str());
  for (int i = 0; i < status.size(); i++) {
    string num = Util::Pad(-2, StringPrintf("%d", i + 1));
    string done = Util::Pad(-3, StringPrintf("%d", status[i].done_games));
    string minsec;
    if (status[i].run_start > 0)
      minsec = AnsiMinSec(now - status[i].run_start);

    printf(ANSI_GREY "[" ANSI_CYAN "%s. " ANSI_YELLOW "%s"
           ANSI_GREY "] " ANSI_WHITE "%s" ANSI_GREEN " vs " ANSI_BLUE "%s"
           ANSI_RESET ": "
           ANSI_WHITE "%s   %s" ANSI_CLEARTOEOL "\n",
           num.c_str(),
           done.c_str(),
           status[i].row.c_str(),
           status[i].col.c_str(),
           status[i].msg.c_str(),
           minsec.c_str());
  }
  printf(ANSI_PURPLE "Neediest: " ANSI_WHITE "%s"
         ANSI_GREEN " vs "
         ANSI_BLUE "%s" ANSI_RESET " -- " ANSI_YELLOW "%lld"
         ANSI_RESET " game(s) played" ANSI_CLEARTOEOL "\n",
         std::get<0>(neediest).c_str(),
         std::get<1>(neediest).c_str(),
         std::get<2>(neediest));
}

static void TournamentThread(int thread_id,
                             Totals *totals,
                             OutcomeTable *outcome_table) {
  // Thread-local instances of each entrant.
  // These are lazily constructed, because in big tournaments
  // we may not ever even need to run some players.
  const vector<Entrant> &entrant_functions = GetEntrants();
  const int num_entrants = entrant_functions.size();

  vector<Player *> entrants(num_entrants, nullptr);
  // for (Entrant e : GetEntrants()) {
  // entrants.push_back(e());
  // }

  {
    WriteMutexLock ml(&status_m);
    status[thread_id].msg = "start";
    status[thread_id].run_start = time(nullptr);
  }

  const int64 start_time = time(nullptr);

  int games_done = 0, games_free = 0;

  for (;;) {
    const int64 now = time(nullptr);
    if (now - start_time > RUN_FOR_SECONDS) {
      break;
    }

    const auto [white, black] = totals->GetAssignment();

    // Lazily construct local instance of players.
    bool first = false;
    if (entrants[white] == nullptr) {
      entrants[white] = entrant_functions[white]();
      first = true;
      CHECK(entrants[white] != nullptr) << white;
    }
    if (entrants[black] == nullptr) {
      entrants[black] = entrant_functions[black]();
      first = true;
      CHECK(entrants[black] != nullptr) << black;
    }

    Player *white_player = entrants[white];
    Player *black_player = entrants[black];

    const string white_name = white_player->Name();
    const string black_name = black_player->Name();

    {
      WriteMutexLock ml(&status_m);
      status[thread_id].msg = first ? "run first" : "running";
      status[thread_id].row = white_name;
      status[thread_id].col = black_name;
      status[thread_id].done_games = games_done;
      status[thread_id].free_games = games_free;
      status[thread_id].run_start = now;
    }
    ShowStatus(now, totals, false);

    // TODO: If both players are deterministic, just pretend
    // the same outcome happened again.
    if (white_player->IsDeterministic() &&
        black_player->IsDeterministic() &&
        outcome_table->HasResult(white_name, black_name)) {
      games_free++;
      outcome_table->WithCell(
          white_name, black_name,
          [&](Cell *cell) {
            auto Error = [&]() {
                return StringPrintf(
                    "---- %s vs %s ----\n"
                    "Supposedly deterministic matchup, but...:\n"
                    "white wins: %d\n"
                    "white loses: %d\n"
                    "draw: %d\n",
                    white_name.c_str(),
                    black_name.c_str(),
                    cell->white_wins,
                    cell->white_losses,
                    cell->draws);
              };

            if (cell->white_wins > 0) {
              CHECK(cell->white_losses == 0 &&
                    cell->draws == 0)
                << Error();
              cell->white_wins++;
            } else if (cell->white_losses > 0) {
              CHECK(cell->white_wins == 0 &&
                    cell->draws == 0)
                << Error();
              cell->white_losses++;
            } else {
              CHECK(cell->draws > 0 &&
                    cell->white_wins == 0 &&
                    cell->white_losses == 0)
                << Error();
              cell->draws++;
            }
          });
    } else {
      vector<Move> as_white;
      switch (PlayGame(white_player, black_player, &as_white)) {
      case Result::WHITE_WINS: {
        outcome_table->WithCell(white_name, black_name,
                                [&as_white](Cell *cell) {
                                  if (cell->example_win.empty())
                                    cell->example_win = RenderMoves(as_white);
                                  cell->white_wins++;
                                });
        break;
      }
      case Result::BLACK_WINS: {
        outcome_table->WithCell(white_name, black_name,
                                [&as_white](Cell *cell) {
                                  if (cell->example_loss.empty())
                                    cell->example_loss = RenderMoves(as_white);
                                  cell->white_losses++;
                                });
        break;
      }
      case Result::DRAW_STALEMATE:
      case Result::DRAW_75MOVES:
      case Result::DRAW_5REPETITIONS: {
        outcome_table->WithCell(white_name, black_name,
                                [&as_white](Cell *cell) {
                                  if (cell->example_draw.empty())
                                    cell->example_draw = RenderMoves(as_white);
                                  cell->draws++;
                                });
        break;
      }
      }
    }

    games_done++;
    totals->Increment(white, black);
  }

  {
    WriteMutexLock ml(&status_m);
    status[thread_id].msg = "done";
    status[thread_id].row = "-";
    status[thread_id].col = "-";
    status[thread_id].done_games = games_done;
    status[thread_id].run_start = 0;
  }
  ShowStatus(time(nullptr), totals, true);

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

  status_start_time = time(nullptr);
  status.resize(THREADS);

  OutcomeTable outcome_table(TournamentDB::LoadFromFile("tournament.db"));

  std::unique_ptr<Totals> totals =
    std::make_unique<Totals>(names, outcome_table.outcomes);

  ParallelFan(THREADS,
              [&totals, &outcome_table](int thread_id) {
                return TournamentThread(thread_id,
                                        totals.get(),
                                        &outcome_table);
              });

  TournamentDB::SaveToFile(outcome_table.outcomes, "tournament.db");

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

