
// Try indexing the game database, and representing it more efficiently.
// We should have a hash table where we can look up by the bitmask,
// so that it can be used for blinded lookup.
// Move lists are actually a very compact way to store games, because
// there's a lot of redundancy if you represent each position! So a
// decent way to store this may be to have the keys be positions, but
// the values are game indices, which we can then just simulate in
// order to look up the matching position. Really common positions,
// like the starting one, may be intractable?

// (Anyway I didn't get anywhere on this; it's just a copy of one
// of the chessreduce samples.)

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
#include "packedgame.h"

constexpr int MAX_PARALLELISM = 12;

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

// static constexpr int64 MAX_GAMES = 1000000;
static constexpr int64 MAX_GAMES = 0LL;

// #define SELF_CHECK true
#undef SELF_CHECK

struct Processor {
  Processor(string file_base) : file_base(file_base) {
    for (int i = 0; i < 16; i++) {
      string f = StringPrintf("d:\\chess\\packed\\%x-%s.pack",
                              i, file_base.c_str());
      outputs.push_back(fopen(f.c_str(), "wb"));
    }
  }

  ~Processor() {
    for (FILE *f : outputs) {
      if (f) fclose(f);
    }
    outputs.clear();
  }

  // If false, we don't even look at the game.
  bool Eligible(const PGN &pgn) {
    // Ignore games that don't finish or where something
    // weird happens.
    if (pgn.result == PGN::Result::OTHER ||
        pgn.GetTermination() != PGN::Termination::NORMAL) {
      return false;
    }

    return true;
  }

  void DoWork(const string &pgn_text) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));
    if (!Eligible(pgn)) return;

    Position pos;
    PackedGame pgame;
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
      pgame.PushMove(PackedGame::PackMove(move));
    }

    switch (pgn.result) {
    case PGN::Result::OTHER:
      LOG(FATAL) << "Should be filtered by Eligible";
      break;
    case PGN::Result::WHITE_WINS:
      pgame.SetResult(PackedGame::Result::WHITE_WINS);
      break;
    case PGN::Result::BLACK_WINS:
      pgame.SetResult(PackedGame::Result::BLACK_WINS);
      break;
    case PGN::Result::DRAW:
      pgame.SetResult(PackedGame::Result::DRAW);
      break;
    }

    uint64 hc = pgame.HashCode();
    WriteGame(hc, pgame.Serialize());
  }

  void WriteGame(uint64 hc, const std::vector<uint8> &bytes) {
    int idx = (hc >> 60) & 15;
    // With consistent byte order.
    uint8 hbytes[8];
    for (int i = 0; i < 8; i++) {
      hbytes[i] = (hc >> (56 - (i * 8))) & 0xFF;
    }
    WriteMutexLock ml(&output_locks[idx]);
    fwrite(&hbytes[0], 8, 1, outputs[idx]);
    fwrite(bytes.data(), bytes.size(), 1, outputs[idx]);
  }

  string Status() {
    return "";
  }

  vector<FILE *> outputs;
  std::shared_mutex output_locks[16] = {};

  const string file_base;

  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  Stats stat_buckets[NUM_BUCKETS];
  PGNParser parser;
};

static void ReadLargePGN(const char *filename, string file_base) {
  Processor processor{file_base};

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

  if (processor.bad_games) {
    fprintf(stderr, "Note: %lld bad games\n", processor.bad_games);
  }
}

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr, "makealmanac.exe input.pgn output_file\n");
    return -1;
  }

  fprintf(stderr, "Converting %s to %s...\n", argv[1], argv[2]);
  ReadLargePGN(argv[1], argv[2]);

  return 0;
}
