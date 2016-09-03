
#ifndef __PLAY_H
#define __PLAY_H

#include "level.h"
#include "player.h"
#include "dirt.h"
#include "draw.h"

/* XXX clean this up ! */

enum prtype {
  PR_QUIT, PR_ERROR, PR_SOLVED, PR_EXIT,
};

#define STAT(fn, ty) \
  static PlayResult fn() { \
    PlayResult p; \
    p.type = PR_ ## ty; \
    return p; \
  }
 
/* XXX move ... */
struct PlayResult {
  
  prtype type;

  union u {
    Solution *sol;
  } u;

  STAT(quit, QUIT);
  STAT(error, ERROR);

  static PlayResult solved(Solution *s) {
    PlayResult p;
    p.type = PR_SOLVED;
    p.u.sol = s;
    return p;
  }

};

#undef STAT

struct Play : public Drawable {
  static Play *Create();
  virtual PlayResult doplay_save(Player *, Level *, Solution *&saved, string md5) = 0;
  virtual PlayResult doplay(Player *plr, Level *lev, string md5) = 0;
  /* play, recording the game in the player's solution file */
  static void playrecord(string file, Player *plr, bool allowrate = true);
  virtual void draw() = 0;
  virtual void screenresize() = 0;
  virtual ~Play();

  /* makes move d (returning true if successful and false if not),
     animating the action.

     assumes a non-invalidated recent "draw()",
     caller should draw() after, too. */
  static bool animatemove(Drawing &dr, Disamb *ctx, Dirt *dirty, dir d);
};

#endif
