
#include "chess.h"

#include <string>
#include <deque>
#include <shared_mutex>
#include <thread>
#include <vector>
#include <utility>
#include <unistd.h>

#include "base/logging.h"
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

enum class Criteria {
  ALL_GAMES,
  TITLED_ONLY,
  BULLET_ONLY,
  BLITZ_ONLY,
  RAPID_ONLY,
  CLASSICAL_ONLY,
};

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
  Processor(Criteria crit) : crit(crit) {}
  const Criteria crit;
  
  bool Accept(const PGN &pgn) const {
    switch (crit) {
    case Criteria::ALL_GAMES:
      return true;
    case Criteria::TITLED_ONLY:
      return 
	ContainsKey(pgn.meta, "WhiteTitle") ||
	ContainsKey(pgn.meta, "BlackTitle");
      break;
    case Criteria::BULLET_ONLY:
    case Criteria::BLITZ_ONLY:
    case Criteria::RAPID_ONLY:
    case Criteria::CLASSICAL_ONLY:
      LOG(FATAL) << "Unimplemented :(";
      return false;

    default:
      LOG(FATAL) << "Bad criteria";
      return false;
    }
  }

  void DoWork(const string &pgn_text) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));

    // Ignore games that don't finish.
    if (pgn.result == PGN::Result::OTHER)
      return;

    // Does it fit the criteria?
    if (!Accept(pgn))
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
    stat_buckets[bucket].AddGame(gs);
  }

  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  Stats stat_buckets[NUM_BUCKETS];
  PGNParser parser;
};

static void ReadLargePGN(Criteria crit, const char *filename) {
  Processor processor{crit};

  auto DoWork = [&processor](const string &s) { processor.DoWork(s); };

  // Titled games are rare, so this becomes IO bound; use fewer workers.
  const int num_workers = (crit == Criteria::TITLED_ONLY) ? 16 : 30;
  
  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork,
							     num_workers);

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

  for (int bucket = 0; bucket < NUM_BUCKETS; bucket++) {
    const Stats &s = processor.stat_buckets[bucket];
    printf("%lld\n", s.num_games);
    for (int i = 0; i < 32; i++) {
      const PieceStats &p = s.pieces[i];
      for (int d = 0; d < 64; d++)
	printf(" %lld", p.died_on[d]);
      printf("\n ");
      for (int d = 0; d < 64; d++)
	printf(" %lld", p.survived_on[d]);
      printf("\n");
    }
  }
  if (processor.bad_games) {
    fprintf(stderr, "Note: %lld bad games\n", processor.bad_games);
  }
}

Criteria ParseCriteria(string s) {
  if (s == "all") return Criteria::ALL_GAMES;
  else if (s == "titled") return Criteria::TITLED_ONLY;
  else if (s == "bullet") return Criteria::BULLET_ONLY;
  else if (s == "blitz") return Criteria::BLITZ_ONLY;
  else if (s == "rapid") return Criteria::RAPID_ONLY;
  else if (s == "classical") return Criteria::CLASSICAL_ONLY;
  LOG(FATAL) << "Unknown critera: " << s;
  return Criteria::ALL_GAMES;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    fprintf(stderr, "rungames.exe criteria input.pgn ...\n");
    return -1;
  }

  Criteria crit = ParseCriteria(argv[1]);
 
  
  for (int i = 2; i < argc; i++) {
    fprintf(stderr, "Reading %s...\n", argv[i]);
    ReadLargePGN(crit, argv[i]);
  }
  return 0;
}
