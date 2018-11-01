
#include "chess.h"

#include <string>
#include <deque>
#include <shared_mutex>
#include <thread>
#include <vector>
#include <utility>
#include <unistd.h>

#include "base/logging.h"
#include "pgn.h"
#include "util.h"

using namespace std;
using int64 = int64_t;

using Move = Position::Move;

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

template<class W, class F>
struct WorkQueue {
  WorkQueue(F f, int num_workers) : f(std::move(f)) {
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
	  done = 0LL;
	  if (todo.empty()) {
	    if (no_more_work) {
	      m.unlock();
	      return;
	    }

	    // PERF Else better to block on a second mutex here.
	    m.unlock();
	  } else {
	    auto work = todo.front();
	    todo.pop_front();
	    in_progress++;
	    pending--;
	    m.unlock();
	    
	    // Do work.
	    this->f(std::move(work));

	    local_done++;
	  }
	}
      };
    
    while (num_workers--)
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
    done = true;
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

  #if 0
  // Caller takes ownership of pointer. Null if no work, and sets
  // is_done to true (in that case) if there will never be more work.
  string *GetWork(bool *is_done) {
    WriteMutexLock ml(&m);
    if (todo.empty()) {
      *is_done = done;
      return nullptr;
    } else {
      auto ret = todo.front();
      todo.pop_front();
      return ret;
    }
  }
  #endif
  
private:
  const F f;
  std::shared_mutex m;
  std::vector<std::thread> threads;
  int64 done = 0LL, in_progress = 0LL, pending = 0LL;
  // Also consider list<>?
  std::deque<W> todo;
  // This is set to true when no more work will be added,
  // which signals that the workers can exit.
  bool no_more_work = false;
};

static const char *START_FEN =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

static void ReadLargePGN(const char *filename) {
  FILE *f = fopen(filename, "r");
  CHECK(f != nullptr);
  
  int64 num_read;

  auto DoWork = 
    [](string pgn_text) {
      PGN pgn;
      CHECK(PGN::Parse(pgn_text, &pgn));
      Position pos;
      CHECK(Position::ParseFEN(START_FEN, &pos));
      
      for (int i = 0; i < pgn.moves.size(); i++) {
	const PGN::Move &m = pgn.moves[i];
	const string old_board = pos.BoardString();
	Move move;
	CHECK(pos.ParseMove(m.move.c_str(), &move))
	  << "Could not parse move: "
	  << ((i % 2 == 0) ? "(white) " : "(black) ") << (i >> 1) << ". "
	  << m.move
	  << "\nIn board:\n"
	  << old_board
	  << "\nFrom full PGN:\n"
	  << pgn_text;
	
	CHECK(old_board == pos.BoardString()) << "ParseMove modified board "
	  "state!";
	CHECK(pos.IsLegal(move)) << m.move;
	CHECK(old_board == pos.BoardString()) << "IsLegal modified board "
	  "state!";
	pos.ApplyMove(move);
      }
    };

  // TOOD: How to get this to deduce second argument at least?
  WorkQueue<string, decltype(DoWork)> work_queue{DoWork, 50};
  
  string game;
  int c = 0;
  int blank_lines = 0;
  int lastc = 0;
  while ( EOF != (c = getc(f)) ) {
    game += c;
    if (c == '\n' && lastc == '\n') {
      blank_lines++;
      if (blank_lines == 2) {
	work_queue.Add(std::move(game));
	game.clear();
	game.reserve(1024);
	num_read++;
	if (num_read % 10000LL == 0) {
	  int64 done, in_progress, pending;
	  work_queue.Stats(&done, &in_progress, &pending);
	  printf("[Still reading; %lld games] %lld %lld %lld\n",
		 num_read, done, in_progress, pending);
	  fflush(stdout);
	}
	blank_lines = 0;
	lastc = 0;
	continue;
      }
    }

    lastc = c;
  }
  
  fclose(f);

  work_queue.SetNoMoreWork();
  
  // Show status until 
  while (work_queue.StillRunning()) {
    int64 done, in_progress, pending;
    work_queue.Stats(&done, &in_progress, &pending);
    printf("[Done reading; %lld games] %lld %lld %lld\n",
	   num_read, done, in_progress, pending);
    fflush(stdout);
    sleep(3);
  }

  printf("Done! Join threads...\n");
}

int main(int argc, char **argv) {
  ReadLargePGN("d:\\chess\\lichess_db_standard_rated_2017-04.pgn");
  return 0;
}
