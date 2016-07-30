
#ifndef __PREFS_H
#define __PREFS_H

#include "escapex.h"
#include "player.h"

#define DEFAULT_SERVER "escape.spacebar.org"

enum { PREF_ALTCONNECT = 0x100,
       PREF_SERVER,
       PREF_ASKRATE,
       PREF_SHOWTUT,
       PREF_BACKUP_PLAYER,
       PREF_DEBUG_NET,
       PREF_ANIMATION_SPEED, /* UNUSED!! */
       PREF_ANIMATION_ENABLED,
       PREF_OPTIMIZE_SOLUTIONS,
};


struct prefs {
  /* show preferences menu for plr */
  static void show (player * plr);

  /* call this on any chunk that is read in.
     it ensures that there's at least a
     default value for each expected preference 
     key. */
  static void defaults (player * plr);

  static bool   getbool   (player * plr, uint32 key);
  static string getstring (player * plr, uint32 key);
  static int32  getint    (player * plr, uint32 key);

  static void putbool   (player * plr, uint32 key, bool);
  static void putstring (player * plr, uint32 key, string);
  static void putint    (player * plr, uint32 key, int32);
};

#endif
