
#include "loadpositions.h"

#include <functional>
#include <chrono>
#include <string>
#include <vector>
#include <shared_mutex>

#include "../chess/bigchess.h"
#include "../chess/chess.h"
#include "../chess/pgn.h"

using namespace std;

using Move = Position::Move;

namespace {
struct GameProcessor {
  GameProcessor(LoadPositions *parent, int64 max_positions) :
    parent(parent),
    max_positions(max_positions) {}

  // If false, we don't even look at the game.
  static bool Eligible(const PGN &pgn) {
    // Ignore games that don't finish.
    if (pgn.result == PGN::Result::OTHER) {
      return false;
    }
    
    if (pgn.GetTermination() != PGN::Termination::NORMAL) {
      return false;
    }

    return true;
  }

  void DoWork(const string &pgn_text) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));

    if (!Eligible(pgn)) return;

    Position pos;
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

      pos.ApplyMove(move);

      // XXX don't just add every position!
      {
	WriteMutexLock ml(&parent->positions_m);
	if (parent->positions.size() >= max_positions)
	  return;
	
	parent->positions.push_back(pos);
      }
    }
  }

  string Status() {
    return "";
  }
  
  LoadPositions *parent = nullptr;

  const int64 max_positions = 0LL;
  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  PGNParser parser;
};

}

LoadPositions::LoadPositions(
    std::function<bool()> ExitEarly,
    int max_parallelism,
    int64 max_games,
    int64 max_positions)
  : max_parallelism(max_parallelism),
    max_games(max_games),
    max_positions(max_positions),
    ExitEarly(ExitEarly) {

  positions.reserve(max_positions);
}

// In parallel, load a bunch of positions from the games in the PGN files into RAM.
void LoadPositions::Load(const string &games_file) {
  {
    ReadMutexLock ml(&positions_m);
    if (positions.size() >= max_positions)
      return;
  }
  
  GameProcessor processor{this, max_positions};

  auto DoWork = [&processor](const string &s) { processor.DoWork(s); };

  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork,
							     max_parallelism);

  const int64 start = time(nullptr);

  {
    PGNTextStream stream(games_file.c_str());
    string game;
    while (stream.NextPGN(&game)) {
      work_queue->Add(std::move(game));
      game.clear();

      const int64 num_read = stream.NumRead();
      if (num_read % 20000LL == 0) {
	int64 done, in_progress, pending, num_positions;
	work_queue->Stats(&done, &in_progress, &pending);
	const bool should_pause = pending > 5000000;
	const char *pausing = should_pause ? " (PAUSE READING)" : "";

	{
	  ReadMutexLock ml(&positions_m);
	  num_positions = positions.size();
	}

	printf("[Still reading; %lld games at %.1f/sec] %lld %lld %lld [%lld pos] %s%s\n",
	       num_read,
	       num_read / (double)(time(nullptr) - start),
	       done, in_progress, pending,
	       num_positions,
	       pausing,
	       processor.Status().c_str());
	if (max_games > 0 && num_read >= max_games)
	  break;

	if (ExitEarly && ExitEarly()) {
	  work_queue->Abandon();
	  return;
	}
	
	if (num_positions >= max_positions) {
	  printf("Enough positions!\n");
	  work_queue->Abandon();
	  return;
	}
	
	if (should_pause)
	  std::this_thread::sleep_for(60s);
      }
    }
  }
  work_queue->SetNoMoreWork();

  while (work_queue->StillRunning()) {
    int64 done, in_progress, pending, num_positions;
    work_queue->Stats(&done, &in_progress, &pending);

    {
      ReadMutexLock ml(&positions_m);
      num_positions = positions.size();
    }

    printf("[Done reading] %lld %lld %lld %.2f%% [%lld pos] %s\n",
	   done, in_progress, pending,
	   (100.0 * (double)done) / (in_progress + done + pending),
	   num_positions,
	   processor.Status().c_str());

    if (ExitEarly && ExitEarly()) {
      work_queue->Abandon();
      return;
    }

    if (num_positions >= max_positions) {
      work_queue->Abandon();
      break;
    }
    std::this_thread::sleep_for(10s);
  }

  printf("Work queue finalizing...\n");
  work_queue.reset(nullptr);
  printf("Done loading %s.\n", games_file.c_str());
}
