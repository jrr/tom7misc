
#include "chess.h"

#include <string>
#include <deque>
#include <shared_mutex>
#include <thread>
#include <vector>
#include <utility>
#include <unistd.h>

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "city.h"

#include "pgn.h"
#include "gamestats.h"
#include "bigchess.h"

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

// static constexpr int64 MAX_GAMES = 1000000;
static constexpr int64 MAX_GAMES = 0LL;

// #define SELF_CHECK true
#undef SELF_CHECK

enum Criteria {
  ALL_GAMES = 0,
  TITLED_ONLY,
  BULLET_ONLY,
  BLITZ_ONLY,
  RAPID_ONLY,
  CLASSICAL_ONLY,

  NUM_CRITERIA,
};

string CriteriaName(Criteria crit) {
  switch (crit) {
  case ALL_GAMES: return "all";
  case TITLED_ONLY: return "titled";
  case BULLET_ONLY: return "bullet";
  case BLITZ_ONLY: return "blitz";
  case RAPID_ONLY: return "rapid";
  case CLASSICAL_ONLY: return "classical";
  default: return "BAD_CRITERIA";
  }
}

static constexpr const char *const PIECE_NAME[32] = {
  "a8 rook",
  "b8 knight",
  "c8 bishop",
  "d8 queen",
  "e8 king",
  "f8 bishop",
  "g8 knight",
  "h8 rook",
  "a7 pawn", "b7 pawn", "c7 pawn", "d7 pawn",
  "e7 pawn", "f7 pawn", "g7 pawn", "h7 pawn",
  // white
  "a2 pawn", "b2 pawn", "c2 pawn", "d2 pawn",
  "e2 pawn", "f2 pawn", "g2 pawn", "h2 pawn",
  "a1 rook",
  "b1 knight",
  "c1 bishop",
  "d1 queen",
  "e1 king",
  "f1 bishop",
  "g1 knight",
  "h1 rook", };

struct Processor {
  Processor(uint32 want_crit_set) : want_crit_set(want_crit_set) {  }

  // Get the subset of want_crit_set that the game actually
  // possesses. This is used to filter the game, and to decide
  // which stat buckets to accumulate results into.
  uint32 GetCriteria(const PGN &pgn) const {
    uint32 result = 0;

    // PERF not expensive, but we could avoid computing this if we
    // have no time control criteria.
    const PGN::TimeClass tc = pgn.GetTimeClass();
    
    for (int crit = 0; crit < NUM_CRITERIA; crit++) {
      // Are we even looking for this criteria?
      if (0 != ((1 << crit) & want_crit_set)) {
	// If so, perform the test and if it's satisfied,
	// OR it into the result set.
	switch (crit) {
	case Criteria::ALL_GAMES:
	  result |= (1 << crit);
	  break;
	case Criteria::TITLED_ONLY: {
	  auto wit = pgn.meta.find("WhiteTitle");
	  if (wit != pgn.meta.end() &&
	      // "Lichess Master" not counted as a "real" title.
	      wit->second != "LM") {
	    result |= (1 << crit);
	    break;
	  }
	  
	  auto bit = pgn.meta.find("BlackTitle");
	  if (bit != pgn.meta.end() &&
	      bit->second != "LM") {
	    result |= (1 << crit);
	    break;
	  }
	  break;
	}

	case Criteria::BULLET_ONLY:
	  if (tc == PGN::TimeClass::BULLET) {
	    result |= (1 << crit);
	  }
	  break;

	case Criteria::BLITZ_ONLY:
	  if (tc == PGN::TimeClass::BLITZ) {
	    result |= (1 << crit);
	  }
	  break;

	case Criteria::RAPID_ONLY:
	  if (tc == PGN::TimeClass::RAPID) {
	    result |= (1 << crit);
	  }
	  break;

	case Criteria::CLASSICAL_ONLY:
	  if (tc == PGN::TimeClass::CLASSICAL ||
	      tc == PGN::TimeClass::CORRESPONDENCE) {
	    result |= (1 << crit);
	  }
	  break;

	default:
	  LOG(FATAL) << "Impossible! Bad criteria";
	  break;
	}
      }
    }
    return result;
  }

  void DoWork(const string &pgn_text) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));

    // Ignore games that don't finish.
    if (pgn.result == PGN::Result::OTHER)
      return;

    // Does it fit the criteria?
    const uint32 has_crit_set = GetCriteria(pgn);
    // Can just skip because it matches no criteria.
    if (0 == has_crit_set)
      return;
    
    auto wit = pgn.meta.find("White");
    uint64 bucket_hash = 0ULL;
    if (wit != pgn.meta.end()) {
      const string &white = wit->second;
      bucket_hash = CityHash64(white.c_str(), white.size());
    } else {
      // We want *some* hash at least. Hopefully this is rare.
      // Warn in this case?
      bucket_hash = CityHash64(pgn_text.c_str(), pgn_text.size());
    }

    Position pos;
    GameStats gs;
    for (int i = 0; i < pgn.moves.size(); i++) {
      const PGN::Move &m = pgn.moves[i];
#ifdef SELF_CHECK
      const string old_board = pos.BoardString();
#endif
      Move move;
      const bool move_ok = pos.ParseMove(m.move.c_str(), &move);

#ifdef SELF_CHECK
      CHECK(move_ok)
	<< "Could not parse move: "
	<< ((i % 2 == 0) ? "(white) " : "(black) ") << (i >> 1) << ". "
	<< m.move
	<< "\nIn board:\n"
	<< old_board
	<< "\nFrom full PGN:\n"
	<< pgn_text;
#endif
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
	
#ifdef SELF_CHECK
      CHECK(old_board == pos.BoardString()) << "ParseMove modified board "
	"state!";
      CHECK(pos.IsLegal(move)) << m.move;
      CHECK(old_board == pos.BoardString()) << "IsLegal modified board "
	"state!";
#endif

      // Use the move to update the fates of pieces.
      gs.Update(pos, move);
	  
      pos.ApplyMove(move);
    }

    // Need to kill the king if checkmated.
    switch (pgn.result) {
    case PGN::Result::WHITE_WINS:
      gs.fates[4] |= GameStats::DIED;
      break;
    case PGN::Result::BLACK_WINS:
      gs.fates[28] |= GameStats::DIED;
      break;
    default:
      // For draws, both kings survive.
      break;
    }

    const int bucket = bucket_hash & NUM_BUCKETS_MASK;
    if (false) {
      fprintf(stderr, "Fates:\n");
      for (int i = 0; i < 32; i++) {
	fprintf(stderr, "%d (%s). %s on %c%c.\n",
		i, PIECE_NAME[i],
		(GameStats::DIED & gs.fates[i]) ? "DIED" : "Survived",
		'a' + (gs.fates[i] & 7),
		'1' + (7 - ((gs.fates[i] & GameStats::POS_MASK) >> 3)));
      }
    }
    
    for (int crit = 0; crit < NUM_CRITERIA; crit++) {
      if (0 != ((1 << crit) & has_crit_set)) {
	stat_buckets[crit * NUM_BUCKETS + bucket].AddGame(gs);
      }
    }
  }
  
  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  // Bitmask with (1 << crit).
  const uint32 want_crit_set = 0LL;
  // Always space for each criteria, even if we're not tallying them all.

  std::vector<Stats> stat_buckets{NUM_CRITERIA * NUM_BUCKETS};
  //   Stats stat_buckets[NUM_CRITERIA * NUM_BUCKETS] = {};
  PGNParser parser;
};

static void ReadLargePGN(uint32 want_crit_set,
			 string input_filename,
			 string outputbase) {
  fprintf(stderr, "OK %s %s\n", input_filename.c_str(), outputbase.c_str());
  fflush(stderr);

  fprintf(stderr, "Create processor..\n");
  fflush(stderr);
  Processor processor{want_crit_set};

  auto DoWork = [&processor](const string &s) { processor.DoWork(s); };

  // Note: Can be smarter about choosing number of workers if
  // selecting a small set of games. e.g. titled games are rare, so
  // this becomes IO bound; use fewer workers.
  const int num_workers = 30;
  // (want_crit_set == Criteria::TITLED_ONLY) ? 16 : 30;
  
  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork,
							     num_workers);

  const int64 start = time(nullptr);
  fprintf(stderr, "Start at %lld\n", start);
  fflush(stderr);
  
  {
    PGNTextStream stream{input_filename.c_str()};
    string game;
    while (stream.NextPGN(&game)) {
      work_queue->Add(std::move(game));
      game.clear();

      const int64 num_read = stream.NumRead();
      if (num_read % 20000LL == 0) {
	int64 done, in_progress, pending;
	work_queue->Stats(&done, &in_progress, &pending);
	fprintf(stderr,
		"[Still reading; %lld games at %.1f/sec] %lld %lld %lld\n",
		num_read,
		num_read / (double)(time(nullptr) - start),
		done, in_progress, pending);
	fflush(stderr);

	if (MAX_GAMES > 0 && num_read >= MAX_GAMES)
	  break;
      }
    }
  }
  
  work_queue->SetNoMoreWork();
  
  // Show status until all games have been run.
  while (work_queue->StillRunning()) {
    int64 done, in_progress, pending;
    work_queue->Stats(&done, &in_progress, &pending);
    fprintf(stderr, "[Done reading] %lld %lld %lld\n",
	    done, in_progress, pending);
    fflush(stderr);
    sleep(3);
  }

  fprintf(stderr, "Done! Join threads...\n");
  work_queue.reset(nullptr);

  for (int crit = 0; crit < NUM_CRITERIA; crit++) {
    if (0 != ((1 << crit) & want_crit_set)) {
      // We computed stats for this criteria, so output a file.
      string filename = StringPrintf("%s-%s.txt",
				     outputbase.c_str(),
				     CriteriaName((Criteria)crit).c_str());
      FILE *f = fopen(filename.c_str(), "wb");
      CHECK(f != nullptr) << filename;
      for (int bucket = 0; bucket < NUM_BUCKETS; bucket++) {
	const Stats &s = processor.stat_buckets[crit * NUM_BUCKETS + bucket];
	fprintf(f, "%lld\n", s.num_games);
	for (int i = 0; i < 32; i++) {
	  const PieceStats &p = s.pieces[i];
	  for (int d = 0; d < 64; d++)
	    fprintf(f, " %lld", p.died_on[d]);
	  fprintf(f, "\n ");
	  for (int d = 0; d < 64; d++)
	    fprintf(f, " %lld", p.survived_on[d]);
	  fprintf(f, "\n");
	}
      }
      fclose(f);
      fprintf(stderr, "Wrote %s.\n", filename.c_str());
      fflush(stderr);
    }
  }
  if (processor.bad_games) {
    fprintf(stderr, "Note: %lld bad games\n", processor.bad_games);
  }
}

Criteria ParseCriteria(const string &s) {
  if (s == "all") return Criteria::ALL_GAMES;
  else if (s == "titled") return Criteria::TITLED_ONLY;
  else if (s == "bullet") return Criteria::BULLET_ONLY;
  else if (s == "blitz") return Criteria::BLITZ_ONLY;
  else if (s == "rapid") return Criteria::RAPID_ONLY;
  else if (s == "classical") return Criteria::CLASSICAL_ONLY;
  LOG(FATAL) << "Unknown critera: " << s;
  return Criteria::ALL_GAMES;
}

uint32 ParseCriteriaSet(string s) {
  uint32 res = 0;
  for (;;) {
    string tok = Util::chopto(',', s);
    if (tok.empty()) return res;
    res |= (1 << ParseCriteria(tok));
  }
}

int main(int argc, char **argv) {
  if (argc < 4) {
    fprintf(stderr, "rungames.exe criteria1,crit2,crit3 input.pgn outputbase\n");
    return -1;
  }

  uint32 want_crit = ParseCriteriaSet(argv[1]);
  fprintf(stderr, "Criteria: 0%o\n", want_crit);
  const string inputfile = argv[2];
  const string outputbase = argv[3];
  fprintf(stderr, "Reading %s and writing to %s*.txt\n",
	  inputfile.c_str(), outputbase.c_str());
  fflush(stderr);
  ReadLargePGN(want_crit, inputfile, outputbase);
  return 0;
}
