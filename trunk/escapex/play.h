
#ifndef __PLAY_H
#define __PLAY_H

#include "level.h"
#include "player.h"
#include "dirt.h"
#include "draw.h"

/* XXX clean this up ! */

enum class PlayResultType {
  QUIT, ERROR, SOLVED, EXIT,
};

/* XXX move ... */
struct PlayResult {
  PlayResultType type;
  Solution sol;

  static PlayResult Quit() {
    PlayResult p;
    p.type = PlayResultType::QUIT;
    return p;
  }

  static PlayResult Error() {
    PlayResult p;
    p.type = PlayResultType::ERROR;
    return p;
  }

  static PlayResult Solved(Solution s) {
    PlayResult p;
    p.type = PlayResultType::SOLVED;
    p.sol = std::move(s);
    return p;
  }
};

struct Play : public Drawable {
  static Play *Create();
  virtual PlayResult DoPlaySave(Player *p, Level *lev,
                                Solution *saved, const string &md5) = 0;
  virtual PlayResult DoPlay(Player *plr, Level *lev, const string &md5) = 0;
  /* play, recording the game in the player's solution file */
  static void playrecord(string file, Player *plr, bool allowrate = true);
  void draw() override = 0;
  void screenresize() override = 0;
  virtual ~Play();

  /* makes move d (returning true if successful and false if not),
     animating the action.

     assumes a non-invalidated recent "draw()",
     caller should draw() after, too. */
  static bool AnimateMove(Drawing &dr, Disamb *ctx, Dirt *dirty, dir d);
};

#endif
