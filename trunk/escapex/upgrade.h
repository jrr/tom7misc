
#ifndef __UPGRADE_H
#define __UPGRADE_H

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
  static Upgrader *create(Player *p);
  virtual UpgradeResult upgrade(string &msg) = 0;
  virtual void destroy() = 0;
  virtual ~Upgrader() {};

  void draw() override = 0;
  void screenresize() override = 0;
};

#endif
