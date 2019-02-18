
#ifndef __PLAYER_H
#define __PLAYER_H

#include <string>

#include "chess.h"

// Interface for a stateless chess-playing algorithm. Can be
// wrapped into a Player.
struct StatelessPlayer {
  // Make a move in an arbitrary position. The position will
  // have at least one legal move (i.e., it is not currently
  // mate). The move returned must be legal!
  // This position need not correspond to any previous positions
  // invoked on, but it is okay for it to keep some state (like a
  // random number generator).
  virtual Position::Move MakeMove(const Position &pos) = 0;

  // Return the name of the algorithm. Should be distinct
  // across all implementations of the interface.
  virtual std::string Name() const = 0;
  // Brief description of how the player works.
  virtual std::string Desc() const = 0;

  virtual ~StatelessPlayer() {}
};

// And some factory functions for players.
// Each returns a new-ly created object, owned by the caller.
StatelessPlayer *CreateFirstMove();
StatelessPlayer *CreateRandom();
StatelessPlayer *CreateAlphabetical();
StatelessPlayer *CreateCCCP();
StatelessPlayer *CreatePacifist();
StatelessPlayer *CreateMinOpponentMoves();
StatelessPlayer *CreateSuicideKing();
StatelessPlayer *CreateReverseStarting();
StatelessPlayer *CreateHuddle();
StatelessPlayer *CreateSwarm();
StatelessPlayer *CreateGenerous();
StatelessPlayer *CreateNoIInsist();
StatelessPlayer *CreateSameColor();
StatelessPlayer *CreateOppositeColor();
StatelessPlayer *CreateMirrorYSymmetry();
StatelessPlayer *CreateMirrorXSymmetry();
StatelessPlayer *CreateSymmetry180();
StatelessPlayer *CreateSinglePlayer();

#endif
