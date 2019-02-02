
#ifndef __STOCKFISH_H
#define __STOCKFISH_H

#include <memory>

#include "subprocess.h"

struct Stockfish {
  // Stockfish wrapper. Thread safe, but spawns
  // a child process.

  Stockfish();


  std::unique_ptr<Subprocess> subprocess;
};

#endif
