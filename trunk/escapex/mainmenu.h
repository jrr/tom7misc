
#ifndef _ESCAPE_MAINMENU_H
#define _ESCAPE_MAINMENU_H

#include "escapex.h"
#include "player.h"
#include "mainshow.h"

struct MainMenu {
  enum Result { LOAD, QUIT, EDIT, REGISTER, UPDATE, UPGRADE, LOAD_NEW, };

  virtual Result Show() = 0;

  static MainMenu *Create(Player *plr);

  virtual ~MainMenu();
};

#endif
