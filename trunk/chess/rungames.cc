
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

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

// static constexpr int64 MAX_GAMES = 1000000;
static constexpr int64 MAX_GAMES = 0LL;

// #define SELF_CHECK true
#undef SELF_CHECK

// TODO: To threadutil, but note that this is C++17.
struct ReadMutexLock {
  explicit ReadMutexLock(std::shared_mutex *m) : m(m) { m->lock_shared(); }
  ~ReadMutexLock() { m->unlock_shared(); }
  std::shared_mutex *m;
};
// Possible to template this over shared_mutex and mutex without
// requiring an argument?
struct WriteMutexLock {
  explicit WriteMutexLock(std::shared_mutex *m) : m(m) { m->lock(); }
  ~WriteMutexLock() { m->unlock(); }
  std::shared_mutex *m;
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

// TODO: This does not parallelize enough. Only one of the numa nodes
// succeeds in getting significant work scheduled. (Maybe actually
// what happens is the N threads are all scheduled onto the numa node
// that's closest to the data?) Would be pretty simple to have K
// distinct deques and round-robin assignment of work to them. When
// one is empty we could only then start work stealing.
//
// An alternate simple experiment would be to grab larger batches
// of work when lots is available. Like if there are N threads, always
// be willing to claim close to (1/N) * pending work units.
template<class W, class F, int64 max_chunk>
struct WorkQueue {
  static_assert(max_chunk > 0LL, "Must take positive chunks!");
  WorkQueue(F f, int num_workers) : f(std::move(f)),
				    num_workers(num_workers) {
    threads.reserve(num_workers);
    auto th =
      [this]() {
	// This is to avoid having to take the mutex just to
	// increment the done counter. We instead do this when
	// we look for work again.
	int64 local_done = 0LL;
	for (;;) {
	  m.lock();
	  done += local_done;
	  in_progress -= local_done;
	  local_done = 0LL;
	  if (todo.empty()) {
	    if (no_more_work) {
	      m.unlock();
	      return;
	    }

	    // PERF Else better to block on a second mutex here.
	    m.unlock();
	  } else {
	    if /* constexpr */ (max_chunk > 1LL) {
	      int64 get_up_to = std::max(1LL, pending / this->num_workers);
	      vector<W> local_work;
	      local_work.reserve(get_up_to);
	      while (!todo.empty() && get_up_to--) {
		local_work.emplace_back(todo.front());
		todo.pop_front();
		in_progress++;
		pending--;
	      }

	      m.unlock();

	      // Do work without lock.
	      for (W &work : local_work)
		this->f(std::move(work));

	      local_done += local_work.size();
	    } else {
	      auto local_work = todo.front();
	      todo.pop_front();
	      in_progress++;
	      pending--;

	      m.unlock();

	      this->f(std::move(local_work));
	      local_done++;
	    }
	  }
	}
      };
    
    for (int i = 0; i < num_workers; i++)
      threads.emplace_back(th);
  }

  ~WorkQueue() {
    for (std::thread &t : threads)
      t.join();
  }
  
  void Add(W work) {
    WriteMutexLock ml(&m);
    pending++;
    todo.push_back(std::move(work));
  }

  void SetNoMoreWork() {
    WriteMutexLock ml(&m);
    no_more_work = true;
  }

  bool IsNoMoreWork() {
    ReadMutexLock ml(&m);
    return done;
  }

  bool StillRunning() {
    ReadMutexLock ml(&m);
    return !no_more_work || pending > 0 || in_progress > 0;
  }
  
  void Stats(int64 *done, int64 *in_progress, int64 *pending) {
    ReadMutexLock ml(&m);
    *done = this->done;
    *in_progress = this->in_progress;
    *pending = this->pending;
  }
  
private:
  const F f;
  const int num_workers;
  std::shared_mutex m;
  std::vector<std::thread> threads;
  int64 done = 0LL, in_progress = 0LL, pending = 0LL;
  // Also consider list<>?
  std::deque<W> todo;
  // This is set to true when no more work will be added,
  // which signals that the workers can exit.
  bool no_more_work = false;
};


static void ReadLargePGN(const char *filename) {
  FILE *f = fopen(filename, "r");
  CHECK(f != nullptr);
  
  int64 num_read = 0LL;
  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  
  Stats stat_buckets[NUM_BUCKETS];
  
  PGNParser parser;
  auto DoWork = 
    [&bad_games_m, &bad_games,
     &stat_buckets, &parser](const string &pgn_text) {
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
    };

  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork, 30);

  const int64 start = time(nullptr);
  
  std::vector<uint8> data;
  int64 data_idx = 0LL;
  auto BigGetC =
    [f, &data, &data_idx]() -> int {
      if (data_idx == data.size()) {
	if (feof(f)) return EOF;
	
	data_idx = 0LL;
	static constexpr int64 MAX_READ_SIZE = 1LL << 24;
	data.resize(MAX_READ_SIZE);
	const size_t bytes_read =
	  fread(&data.front(), 1, MAX_READ_SIZE, f);
	if (bytes_read != MAX_READ_SIZE)
	  data.resize(bytes_read);

	if (data.empty()) {
	  return EOF;
	}
      }
      // Otherwise, data is non-empty and data_idx points at the
      // next element.
      return data[data_idx++];
    };
  
  string game;
  int c = 0;
  int blank_lines = 0;
  int lastc = 0;
  while ( EOF != (c = BigGetC()) ) {
    game += c;
    if (c == '\n' && lastc == '\n') {
      blank_lines++;
      if (blank_lines == 2) {
	work_queue->Add(std::move(game));
	game.clear();
	game.reserve(1024);
	num_read++;
	if (num_read % 20000LL == 0) {
	  int64 done, in_progress, pending;
	  work_queue->Stats(&done, &in_progress, &pending);
	  fprintf(stderr,
		  "[Still reading; %lld games at %.1f/sec] %lld %lld %lld\n",
		  num_read,
		  num_read / (double)(time(nullptr) - start),
		  done, in_progress, pending);
	  fflush(stderr);
	}

	if (MAX_GAMES > 0 && num_read >= MAX_GAMES)
	  break;
	
	blank_lines = 0;
	lastc = 0;
	continue;
      }
    }

    lastc = c;
  }
  
  fclose(f);

  work_queue->SetNoMoreWork();
  
  // Show status until all games have been run.
  while (work_queue->StillRunning()) {
    int64 done, in_progress, pending;
    work_queue->Stats(&done, &in_progress, &pending);
    fprintf(stderr, "[Done reading; %lld games] %lld %lld %lld\n",
	    num_read, done, in_progress, pending);
    fflush(stderr);
    sleep(3);
  }

  fprintf(stderr, "Done! Join threads...\n");
  work_queue.reset(nullptr);

  for (int bucket = 0; bucket < NUM_BUCKETS; bucket++) {
    const Stats &s = stat_buckets[bucket];
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
  if (bad_games) {
    fprintf(stderr, "Note: %lld bad games\n", bad_games);
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "rungames.exe input.pgn ...\n");
    return -1;
  }
  for (int i = 1; i < argc; i++) {
    //     "d:\\chess\\lichess_db_standard_rated_2017-04.pgn";
    fprintf(stderr, "Reading %s...\n", argv[i]);
    ReadLargePGN(argv[i]);
  }
  return 0;
}
