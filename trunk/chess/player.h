
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

// This shouldn't be thought of like a typical chess player interface,
// since it must be able to switch sides at a whim.
struct PlayerGame {
  // Force a specific move, which changes the current side.
  virtual void ForceMove(Position::Move move) = 0;
  // Get a move for the current player in the current position.
  // pos is guaranteed to match the series of moves already
  // applied, in case the object does not want to track this.
  // The move should not be applied; ForceMove will typically be
  // called on it.
  virtual Position::Move GetMove(const Position &pos) = 0;

  virtual ~PlayerGame() {}
};

// Algorithm that may keep state.
struct Player {
  // Creates a new game instance, in the starting position.
  // new-ly allocated object, owned by caller.
  virtual PlayerGame *CreateGame() = 0;

  // As above.
  virtual std::string Name() const = 0;
  virtual std::string Desc() const = 0;

  virtual ~Player() {}
};

Player *Random();
Player *Huddle();

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
