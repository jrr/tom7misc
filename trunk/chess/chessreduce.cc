
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

static void ReadLargePGN(const char *filename) {
  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  
  Stats stat_buckets[NUM_BUCKETS];
  
  PGNParser parser;
  auto DoWork = 
    [&bad_games_m, &bad_games,
     &stat_buckets, &parser](const string &pgn_text) {
      printf("DoWork[%s]\n", pgn_text.c_str());
      PGN pgn;
      CHECK(parser.Parse(pgn_text, &pgn));

      // Ignore games that don't finish.
      if (pgn.result == PGN::OTHER)
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
#ifdef SELF_CHECK
	  << "\nIn board:\n"
	  << old_board
#endif
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
	const uint8 src_pos = move.src_row * 8 + move.src_col;
	const uint8 dst_pos = move.dst_row * 8 + move.dst_col;
	for (int i = 0; i < 32; i++) {
	  // Move the living piece.
	  if (gs.fates[i] == src_pos) {
	    gs.fates[i] = dst_pos;
	  } else if (gs.fates[i] == dst_pos) {
	    // Kill piece in the destination square, if any.
	    gs.fates[i] |= GameStats::DIED;
	  }
	}
	
	// Also handle castling. We can assume the move is legal,
	// so if it's a king moving two spaces, we know where the
	// rooks are and where they're going.
	if (src_pos == 4 && pos.PieceAt(0, 4) ==
	    (Position::BLACK | Position::KING)) {
	  if (dst_pos == 2) {
	    gs.fates[0] = 3;
	  } else if (dst_pos == 6) {
	    gs.fates[7] = 5;
	  }
	} else if (src_pos == 60 && pos.PieceAt(7, 4) ==
		   (Position::WHITE | Position::KING)) {
	  if (dst_pos == 58) {
	    gs.fates[24] = 59;
	  } else if (dst_pos == 62) {
	    gs.fates[31] = 61;
	  }
	}

	// If it was an en passant capture, need to kill the captured
	// pawn. The loop above did not 
	if (((move.src_row == 3 && move.dst_row == 2) ||
	     (move.src_row == 4 && move.dst_row == 5)) &&
	    move.src_col != move.dst_col &&
	    pos.PieceAt(move.dst_row, move.dst_col) == Position::EMPTY &&
	    (pos.PieceAt(move.src_row, move.src_col) & Position::TYPE_MASK) ==
	    Position::PAWN) {
	  // en passant capture.
	  // If row 3, then white is capturing black, which is on the row
	  // below the dst pos. Otherwise, the row above.
	  const uint8 cap_pos =
	    (move.src_row == 3) ? dst_pos + 8 : dst_pos - 8;
	  for (int i = 0; i < 32; i++) {
	    if (gs.fates[i] == cap_pos) {
	      gs.fates[i] |= GameStats::DIED;
	      goto success;
	    }
	  }
	  CHECK(false) << "Apparent en passant capture, but no piece "
	    "was at " << cap_pos << " to be captured.\n" <<
	    pos.BoardString() << "\nwith move: " << m.move <<
	    "\nwhich is: " <<
	    move.src_row << " " << move.src_col << " -> " <<
	    move.dst_row << " " << move.dst_col;
	  
	success:;
	}
	  
	pos.ApplyMove(move);
      }

      // Need to kill the king if checkmated.
      switch (pgn.result) {
      case PGN::WHITE_WINS:
	gs.fates[4] |= GameStats::DIED;
	break;
      case PGN::BLACK_WINS:
	gs.fates[28] |= GameStats::DIED;
	break;
      default:
	// For draws, both kings survive.
	break;
      }

      const int bucket = bucket_hash & NUM_BUCKETS_MASK;
      stat_buckets[bucket].AddGame(gs);
    };

  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork, 30);

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
