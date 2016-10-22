
#ifndef __MAINMENU_H
#define __MAINMENU_H

#include "escapex.h"
#include "player.h"
#include "mainshow.h"

struct MainMenu {
  enum result { LOAD, QUIT, EDIT, REGISTER, UPDATE, UPGRADE, LOAD_NEW, };

  virtual result Show() = 0;

  static MainMenu *Create(Player *plr);

  virtual ~MainMenu();
};

#endif
