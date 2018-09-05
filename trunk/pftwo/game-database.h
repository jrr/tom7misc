// Database of games that we know something about.
//
// Aside from the mapping of romfile to training data movie (which is
// fair game), and maybe basics like "is the game two-player?" the
// other information should only be used for evaluating stuff like
// autocamera and autolives. It would be "cheating" to use this to
// automatically play games, since it is game-specific and often
// derived manually.

#ifndef __GAME_DATABASE_H
#define __GAME_DATABASE_H

#include <string>
#include <map>
#include <vector>
#include <utility>

using namespace std;

// Rude!!
#undef UNKNOWN
#undef FALSE
#undef TRUE
enum class OBool { UNKNOWN, FALSE, TRUE };

struct Game {
  string romfile;
  string moviefile;

  // string deathmoviefile;

  bool is_two_player = false;
  
  // For memory locations, -1 is standard for "unknown."
  // Note that it is possible for one of xloc,yloc to be known
  // but not the other.

  struct Player {
    // x,y position on screen.
    // TODO: Should indicate how scroll is taken into account,
    // but probably outside the Player struct?
    int xloc = -1, yloc = -1;
    // Integral; when it goes to zero (or negative) 
    int lives = -1;

    int health = -1;
    // TODO:
    // record whether warping works?
    // location of 'health'
    // dx, dy?
    Player(int xloc, int yloc, int lives) :
      xloc(xloc), yloc(yloc), lives(lives) {}
    Player() {}
  };
  
  Player p1;
  Player p2;
  // If true, then zero lives "in reserve" is not game over
  // yet; it must go negative. If false, then 0 means game
  // over.
  OBool allow_zero_lives = OBool::UNKNOWN;
  // Same for health. Surprisingly, there are many games that
  // have different rules for these two.
  OBool allow_zero_health = OBool::UNKNOWN;
  
  // TODO: Good objective function?
  // TODO: Vaguer hints like "protect these memory locations"
  // or "related to player position"?
  
  Game() {}
};

struct GameDB {
  GameDB();
  vector<Game> GetAll() const;
  // Like with {"contra", "mario"}.
  vector<Game> GetMatching(const std::vector<string> &vec) const;

 private:
  // map key is like "mario"
  std::map<string, Game> db_;
};

#endif
