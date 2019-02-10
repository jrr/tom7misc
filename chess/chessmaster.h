
// Wrapper for NES Chessmaster game.

#ifndef __CHESSMASTER_H
#define __CHESSMASTER_H

#include <vector>
#include <cstdint>
#include <memory>
#include <mutex>

#include "chess.h"

struct Emulator;

struct Chessmaster {
  // Stockfish wrapper. Thread safe, but spawns
  // a child process.

  // Level in [0, 20] with 20 being strongest.
  // Note that engine loading is lazy; errors like missing chessmaster.nes
  // won't occur until the first call to GetMove.
  Chessmaster(int level);
  
  // Get a move. The position must be legal and have moves!
  // If something goes wrong, returns a move from 0,0 to 0,0.
  Position::Move GetMove(const Position &pos);

  ~Chessmaster();
  
private:
  // Must hold lock.
  void InitEngine();
  // Must hold lock.
  bool WaitInputReady();
  const int level;
  std::mutex emulator_m;
  std::vector<uint8_t> edit_save;
  std::unique_ptr<Emulator> emu;

  std::vector<uint8_t> return_to_game, change_sides;
};


#endif
