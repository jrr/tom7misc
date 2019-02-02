
#ifndef __PLAYER_H
#define __PLAYER_H

#include "chess.h"

// Interface for a chess-playing algorithm.
struct Player {
  // Make a move in an arbitrary position. The position will
  // have at least one legal move (i.e., it is not currently
  // mate). The move returned must be legal!
  // This position need not correspond to any previous positions
  // invoked on, but it is okay for it to keep some state (like a
  // random number generator).
  virtual Position::Move MakeMove(const Position &pos) = 0;

  // Return the name of the algorithm. Should be distinct
  // across all implementations of the interface.
  virtual const char *Name() const = 0;
  // Brief description of how the player works.
  virtual const char *Desc() const = 0;

  virtual ~Player() {}
};

// And some factory functions for players.
// Each returns a new-ly created object, owned by the caller.
Player *CreateFirstMove();
Player *CreateRandom();
Player *CreateCCCP();
Player *CreateMinOpponentMoves();
Player *CreateSuicideKing();
Player *CreateReverseStarting();
Player *CreateGenerous();

#endif
