
#ifndef _ESCAPE_PREFS_H
#define _ESCAPE_PREFS_H

#include <string>

#include "escapex.h"
#include "player.h"

#define DEFAULT_SERVER "escape.spacebar.org"

enum Pref : uint32 {
  PREF_ALTCONNECT = 0x100,
  PREF_SERVER = 0x101,
  PREF_ASKRATE = 0x102,
  PREF_SHOWTUT = 0x103,
  PREF_BACKUP_PLAYER = 0x104,
  PREF_DEBUG_NET = 0x105,
  PREF_ANIMATION_SPEED = 0x106, /* UNUSED!! */
  PREF_ANIMATION_ENABLED = 0x107,
  PREF_OPTIMIZE_SOLUTIONS = 0x108,
};

struct Prefs {
  /* show preferences menu for plr */
  static void show(Player *plr);

  /* call this on any chunk that is read in.
     it ensures that there's at least a
     default value for each expected preference
     key. */
  static void defaults(Player *plr);

  static bool   getbool  (Player *plr, uint32 key);
  static string getstring(Player *plr, uint32 key);
  static int32  getint   (Player *plr, uint32 key);

  static void putbool  (Player *plr, uint32 key, bool);
  static void putstring(Player *plr, uint32 key, string);
  static void putint   (Player *plr, uint32 key, int32);
};

#endif
