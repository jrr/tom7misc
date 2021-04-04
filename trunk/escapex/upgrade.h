
#ifndef _ESCAPE_UPGRADE_H
#define _ESCAPE_UPGRADE_H

#include "escapex.h"
#include "player.h"
#include "http.h"
#include "draw.h"

/* should be in a config somewhere */
#define UPGRADEURL "/" PLATFORM "/UPGRADE"

/* XXX no real difference between UP_FAIL and UP_OK. */
enum UpgradeResult {
  UP_FAIL, UP_OK, UP_EXIT,
};

/* upgrading is updating Escape itself. */
struct Upgrader : public Drawable {
  static Upgrader *Create(Player *p);
  virtual UpgradeResult Upgrade(string &msg) = 0;
  virtual ~Upgrader();

  void Draw() override = 0;
  void ScreenResize() override = 0;
};

#endif
