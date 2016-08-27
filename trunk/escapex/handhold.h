#ifndef __HANDHOLD_H
#define __HANDHOLD_H

struct HandHold {
  /* do something the first time the game is launched */
  static void firsttime();
  static void init();

  static void did_update();
  static void did_upgrade();

  static bool recommend_update();
  static bool recommend_upgrade();
};

#endif
