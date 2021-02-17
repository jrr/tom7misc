
#ifndef _PLAYER_H
#define _PLAYER_H

#include <string>
#include <cstdint>

#include "chess.h"

class ImageRGBA;

// Abstract "visualization" of the player's internal state.
struct Explainer {
  // All the moves must be legal, but it can be any subset of them.
  // int64 is the penalty (negative means better move), string is
  // arbitrary.
  virtual void SetScoredMoves(
      const std::vector<
          std::tuple<Position::Move, int64_t, std::string>> &v) = 0;
  virtual void SetMessage(const std::string &s) = 0;
  virtual void SetPosition(const Position &pos) = 0;
  virtual void SetGraphic(const ImageRGBA &rgba) = 0;
};

// Interface for a stateless chess-playing algorithm. Can be
// wrapped into a Player.
struct StatelessPlayer {
  // Make a move in an arbitrary position. The position will
  // have at least one legal move (i.e., it is not currently
  // mate). The move returned must be legal!
  // This position need not correspond to any previous positions
  // invoked on, but it is okay for it to keep some state (like a
  // random number generator).
  virtual Position::Move MakeMove(const Position &pos,
                                  Explainer *explainer = nullptr) = 0;

  // Return the name of the algorithm. Should be distinct
  // across all implementations of the interface.
  virtual std::string Name() const = 0;
  // Brief description of how the player works.
  virtual std::string Desc() const = 0;

  virtual bool IsDeterministic() const { return false; }

  virtual ~StatelessPlayer() {}
};

// This shouldn't be thought of like a typical chess player interface,
// since it must be able to switch sides at a whim.
struct PlayerGame {
  // Force a specific move, which changes the current side.
  virtual void ForceMove(const Position &pos,
                         Position::Move move) = 0;
  // Get a move for the current player in the current position.
  // pos is guaranteed to match the series of moves already
  // applied, in case the object does not want to track this.
  // The move should not be applied; ForceMove will typically be
  // called on it.
  virtual Position::Move GetMove(const Position &pos,
                                 Explainer *explainer = nullptr) = 0;

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

  // True if the player always makes the same move given the
  // same situation. Note that many players are not deterministic
  // because they use randomness to break ties.
  virtual bool IsDeterministic() const { return false; }

  virtual ~Player() {}
};

// And some factory functions for players.
// Each returns a new-ly created object, owned by the caller.
Player *FirstMove();
Player *Random();
Player *Alphabetical();
Player *CCCP();
Player *Pacifist();
Player *MinOpponentMoves();
Player *SuicideKing();
Player *ReverseStarting();
Player *Huddle();
Player *Swarm();
Player *Generous();
Player *NoIInsist();
Player *SameColor();
Player *OppositeColor();
Player *MirrorYSymmetry();
Player *MirrorXSymmetry();
Player *Symmetry180();
Player *SinglePlayer();

#endif
