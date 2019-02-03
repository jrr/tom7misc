
#ifndef __STOCKFISH_H
#define __STOCKFISH_H

#include <string>
#include <memory>
#include <mutex>

#include "subprocess.h"

struct Stockfish {
  // Stockfish wrapper. Thread safe, but spawns
  // a child process.

  // Level in [0, 20] with 20 being strongest.
  Stockfish(int level);

  struct Score {
    bool is_mate = false;
    // From the engine's point of view.
    // If is_mate = false, then this is the centipawn score.
    //   (centipawns negative if the engine is losing)
    // If is_mate = true, then this many moves to mate.
    //   (moves negative if the engine is getting mated)
    int value = 0;
  };
  
  // Get a move. The position must be legal and have moves!
  void GetMove(const std::string &fen, std::string *move, Score *score);
  
private:
  std::mutex subprocess_m;
  std::unique_ptr<Subprocess> subprocess;
};

#endif
