
#ifndef _ESCAPE_PLAY_H
#define _ESCAPE_PLAY_H

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
  // Does not take ownership of level.
  static Play *Create(const Level *l);

  virtual PlayResult DoPlaySave(Player *p, Solution *saved,
				const std::string &md5) = 0;
  virtual PlayResult DoPlay(Player *plr, const std::string &md5) = 0;
  /* play, recording the game in the player's solution file */
  static void PlayRecord(const std::string &file, Player *plr,
			 bool allowrate = true);
  void Draw() override = 0;
  void ScreenResize() override = 0;
  virtual ~Play();
};

#endif
