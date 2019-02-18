
#include "chess.h"

#include <string>
#include <deque>
#include <shared_mutex>
#include <thread>
#include <vector>
#include <utility>
#include <unistd.h>

#include "base/stringprintf.h"
#include "gtl/top_n.h"
#include "base/logging.h"
#include "util.h"
#include "city.h"

#include "pgn.h"
#include "gamestats.h"
#include "bigchess.h"
#include "fate-data.h"

#include <windows.h> // XXX
#include <psapi.h> // XXX

constexpr int MAX_PARALLELISM = 8;

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

// static constexpr int64 MAX_GAMES = 1000000;
static constexpr int64 MAX_GAMES = 0LL;

// #define SELF_CHECK true
#undef SELF_CHECK

// How likely are these fates for the pieces? A number between
// 0 and 1, with 1 meaning most likely.
static double FateScore(const GameStats &result) {
  // Product would probably be more justifiable (like this would
  // be the joint probability of the final state), but that
  // results in extremely small values. TODO: Maybe could add logs,
  // or whatever...
  double sum = 0.0;
  for (int i = 0; i < 32; i++) {
    const int square = result.fates[i] & GameStats::POS_MASK;
    if (result.fates[i] & GameStats::DIED) {
      sum += fate_data[i][square].died;
    } else {
      sum += fate_data[i][square].lived;
    }
  }
  
  return sum / 32.0;
}

struct Processor {
  Processor() : topn(10) {}

  // If false, we don't even look at the game.
  bool Eligible(const PGN &pgn) {
    // Ignore games that don't finish.
    if (pgn.result == PGN::Result::OTHER) {
      return false;
    }

    // We're looking for mate here, so require one side to win.
    /*
    if (pgn.result != PGN::Result::WHITE_WINS &&
	pgn.result != PGN::Result::BLACK_WINS) {
      return false;
    }
    */
    
    if (pgn.GetTermination() != PGN::Termination::NORMAL) {
      return false;
    }

    auto it = pgn.meta.find("Event");
    if (it == pgn.meta.end() ||
	string::npos != it->second.find("Bullet"))
      return false;
    
    // TODO: time format? Ignore bullet?
    // TODO: titled players only?
    // [WhiteTitle "IM"]
    // [BlackTitle "CM"]
    
    const int min_elo = std::min(pgn.MetaInt("WhiteElo", 0),
				 pgn.MetaInt("BlackElo", 0));
    if (min_elo < 2300 &&
	(!ContainsKey(pgn.meta, "WhiteTitle") ||
	 !ContainsKey(pgn.meta, "BlackTitle"))) {
      return false;
    }
    return true;
  }

  // A score of 0 is not saved. Otherwise, looking for the
  // highest score.
  int64 ScorePosition(const Position &pos) {
    int most_white_pawns = 0, most_black_pawns = 0;
    // Find quadrupled (or more) pawns by either player.
    for (int c = 0; c < 8; c++) {
      int white_pawns = 0, black_pawns = 0;
      for (int r = 0; r < 8; r++) {
	uint8 p = pos.PieceAt(r, c);
	if ((p & Position::TYPE_MASK) == Position::PAWN) {
	  if ((p & Position::COLOR_MASK) == Position::WHITE)
	    white_pawns++;
	  else
	    black_pawns++;
	}
      }
      most_white_pawns = std::max(most_white_pawns, white_pawns);
      most_black_pawns = std::max(most_black_pawns, black_pawns);
    }

    int64 res = 0;
    if (most_white_pawns >= 4) res += most_white_pawns;
    if (most_black_pawns >= 4) res += most_black_pawns;
    return res;
  }

  void DoWork(const string &pgn_text) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));

    if (!Eligible(pgn)) return;

    int max_position_score = 0;

    int black_consec_forced = 0, white_consec_forced = 0;
    
    Position pos;
    GameStats gs;
    for (int i = 0; i < pgn.moves.size(); i++) {
      const PGN::Move &m = pgn.moves[i];
      Move move;
      const bool move_ok = pos.ParseMove(m.move.c_str(), &move);

      if (!move_ok) {
	fprintf(stderr, "Bad move %s from full PGN:\n%s",
		m.move.c_str(), pgn_text.c_str());
	// There are a few messed up games in 2016 and earlier.
	// Return early if we find such a game.
	{
	  WriteMutexLock ml(&bad_games_m);
	  bad_games++;
	}
	return;
      }

      /*
      // If either queen is dead, consider this the endgame, and
      // stop counting forced moves.
      // Note: There used to be a bug where BLACK_QUEEN was way
      // out of bounds (chessboard index, not piece index), so
      // when I ran this code it was wrong!
      if (!!(gs.fates[GameStats::BLACK_QUEEN] & GameStats::DIED) ||
	  !!(gs.fates[GameStats::WHITE_QUEEN] & GameStats::DIED))
	break;

      const bool blackmove = pos.BlackMove();
      const bool forced = 1 == pos.ExactlyOneLegalMove();

      if (blackmove)
	black_consec_forced = forced ? black_consec_forced + 1 : 0;
      else
	white_consec_forced = forced ? white_consec_forced + 1 : 0;

      max_position_score = std::max(max_position_score,
				    std::max(black_consec_forced,
					     white_consec_forced));
      */
      
      // const bool castling_move = pos.IsCastling(move);
      // const bool enpassant_move = pos.IsEnPassant(move);
      pos.ApplyMove(move);

      gs.Update(pos, move);
      
      #if 0
      if (enpassant_move) {
	if (pos.IsMated()) {
	  int64 game_score =
	    std::min(pgn.MetaInt("WhiteElo", 0),
		     pgn.MetaInt("BlackElo", 0));      

	  if (ContainsKey(pgn.meta, "WhiteTitle"))
	    game_score += 10000;
	  if (ContainsKey(pgn.meta, "BlackTitle"))
	    game_score += 10000;

	  int base, increment;
	  std::tie(base, increment) = pgn.GetTimeControl();
	  if (base == 0 && increment == 0) game_score += 2000;
	  else game_score += base + increment;

	  // If the game has an interesting position, then output it.
	  // PERF: We could do a read-only check of TopN without taking
	  // the write mutex.
	  WriteMutexLock ml(&topn_m);
	  ScoredGame sg;
	  sg.score = game_score;
	  sg.pgn_text = pgn_text;
	  topn.push(std::move(sg));
	}
      }
      #endif
      
      #if 0
      max_position_score = 
	std::max(max_position_score, ScorePosition(pos));
      #endif
    }

    // End of the game. How weird is the final position?
    // weirdnesss .979933 to .973182 in top 10,
    // but go down to almost .800 in bottom.
    double weirdness = 1.0 - FateScore(gs);
    int64 game_score = weirdness * 1000000LL;    
    if (game_score > 950000LL) {

      /*
      game_score += 
	std::min(pgn.MetaInt("WhiteElo", 0),
		 pgn.MetaInt("BlackElo", 0));      

      if (ContainsKey(pgn.meta, "WhiteTitle"))
	game_score += 10000;
      if (ContainsKey(pgn.meta, "BlackTitle"))
	game_score += 10000;

      int base, increment;
      std::tie(base, increment) = pgn.GetTimeControl();
      if (base == 0 && increment == 0) game_score += 2000;
      else game_score += base + increment;
      */      
      // If the game has an interesting position, then output it.
      // PERF: We could do a read-only check of TopN without taking
      // the write mutex.
      WriteMutexLock ml(&topn_m);
      ScoredGame sg;
      sg.score = game_score;
      sg.pgn_text = pgn_text;
      topn.push(std::move(sg));
    }
  }

  struct ScoredGame {
    int64 score;
    string pgn_text;
  };
  struct ScoredGameCmp {
    bool operator()(const ScoredGame &a, const ScoredGame &b) {
      return a.score > b.score;
    };
  };

  string Status() {
    ReadMutexLock ml(&topn_m);
    size_t s = topn.size();
    return s > 0 ? StringPrintf(" (found %d)", (int)s) : "";
  }
  
  std::shared_mutex topn_m;
  gtl::TopN<ScoredGame, ScoredGameCmp> topn;

  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  Stats stat_buckets[NUM_BUCKETS];
  PGNParser parser;
};

static void ReadLargePGN(const char *filename) {
  Processor processor;

  auto DoWork = [&processor](const string &s) { processor.DoWork(s); };

  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork,
							     MAX_PARALLELISM);

  const int64 start = time(nullptr);

  {
    PGNTextStream stream{filename};
    string game;
    while (stream.NextPGN(&game)) {
      work_queue->Add(std::move(game));
      game.clear();

      const int64 num_read = stream.NumRead();
      if (num_read % 20000LL == 0) {
	int64 done, in_progress, pending;
	work_queue->Stats(&done, &in_progress, &pending);
	const bool should_pause = pending > 5000000;
	const char *pausing = should_pause ? " (PAUSE READING)" : "";
	fprintf(stderr,
		"[Still reading; %lld games at %.1f/sec] %lld %lld %lld %s%s\n",
		num_read,
		num_read / (double)(time(nullptr) - start),
		done, in_progress, pending, pausing,
		processor.Status().c_str());
	fflush(stderr);
	if (MAX_GAMES > 0 && num_read >= MAX_GAMES)
	  break;

	if (should_pause)
	  sleep(60);
      }
    }
  }
  
  work_queue->SetNoMoreWork();

  #if 0
  {
    // XXX DEBUG
    PSAPI_WORKING_SET_EX_INFORMATION workingset[16384];
    CHECK(QueryWorkingSetEx(GetCurrentProcess(),
			    workingset,
			    16384 * sizeof (PSAPI_WORKING_SET_INFORMATION)));
    for (int i = 0; i < 16384; i++) {
      const PSAPI_WORKING_SET_EX_INFORMATION *wsi = &workingset[i];
      /*
      if (wsi->VirtualAddress == 0) {
	fprintf(stderr, "(null)\n");
      }
      */
      const PSAPI_WORKING_SET_EX_BLOCK *wsb = &wsi->VirtualAttributes;
      fprintf(stderr, "%p: ", wsi->VirtualAddress);
      if (wsb->Valid) {
	fprintf(stderr,
		" v shc %d prot %d sh %d node %d lock %d large %d",
		(int)wsb->ShareCount,
		(int)wsb->Win32Protection,
		(int)wsb->Shared,
		(int)wsb->Node,
		(int)wsb->Locked,
		(int)wsb->LargePage
		/* , (int)wsb->Reserved */);
      } else {
	fprintf(stderr, " INVALID");
      }
      fprintf(stderr, "\n");
      fflush(stderr);
    }
  }
  #endif

  // Show status until all games have been run.
  while (work_queue->StillRunning()) {
    int64 done, in_progress, pending;
    work_queue->Stats(&done, &in_progress, &pending);
    fprintf(stderr, "[Done reading] %lld %lld %lld %.2f%% %s\n",
	    done, in_progress, pending,
	    (100.0 * (double)done) / (in_progress + done + pending),
	    processor.Status().c_str());
    fflush(stderr);
    sleep(10);
  }

  fprintf(stderr, "Done! Join threads...\n");
  work_queue.reset(nullptr);

  // HERE write results...

  if (processor.bad_games) {
    fprintf(stderr, "Note: %lld bad games\n", processor.bad_games);
  }

  fprintf(stderr,
	  "There are %lld scored games\n", (int64)processor.topn.size());
  auto top = processor.topn.Extract();
  for (const auto &p : *top) {
    printf("[Score %lld]\n%s\n", p.score, p.pgn_text.c_str());
  }
  delete top;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "chessreduce.exe input.pgn ...\n");
    return -1;
  }
  // FYI, usually much better to run this in parallel, like
  // with make -j 4.
  for (int i = 1; i < argc; i++) {
    fprintf(stderr, "Reading %s...\n", argv[i]);
    ReadLargePGN(argv[i]);
  }
  return 0;
}
