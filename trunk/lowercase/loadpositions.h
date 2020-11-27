
#ifndef __LOADPOSITIONS_H
#define __LOADPOSITIONS_H

#include <functional>
#include <string>
#include <shared_mutex>
#include <cstdint>

#include "../chess/chess.h"

struct LoadPositions {
  using int64 = int64_t;
  
  LoadPositions(
      std::function<bool()> ExitEarly,
      int max_parallelism,
      int64 max_games,
      int64 max_positions);

  // Synchronous, but can run in a separate thread. Can be called
  // multiple times (even in parallel), although better to call it
  // serially.
  void Load(const std::string &filename);
  
  std::shared_mutex positions_m;
  // Protected by positions_m.
  std::vector<Position> positions;

private:
  
  const int max_parallelism;
  const int64 max_games, max_positions;
  const std::function<bool()> ExitEarly;
};


#endif
