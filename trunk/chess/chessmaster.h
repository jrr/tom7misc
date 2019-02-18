
// Wrapper for NES Chessmaster game.

#ifndef __CHESSMASTER_H
#define __CHESSMASTER_H

#include <vector>
#include <cstdint>
#include <memory>
#include <mutex>

#include "chess.h"
#include "player.h"
#include "../cc-lib/arcfour.h"

struct Emulator;

struct Chessmaster {
  // Level in [0, 20] with 20 being strongest.
  // TODO: (Levels not yet used.)
  
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
  bool WaitInputReady(uint8 button);
  void Screenshot(const std::string &f);
  const int level;
  std::mutex emulator_m;
  std::vector<uint8_t> edit_save;
  std::unique_ptr<Emulator> emu;

  std::vector<uint8_t> return_to_game, change_sides;
  ArcFour rc;
  // std::vector<uint8_t> change_sides1, change_sides2;
};

// Level 1. Makes random move if chessmaster fails for some reason.
Player *Chessmaster1();
// Level 2.
Player *Chessmaster2();

#endif
