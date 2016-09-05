
/* XXX This is deprecated (in favor of the bookmarks menu)
   and should be deleted. */

#ifndef __SMANAGE_H
#define __SMANAGE_H

#include "escapex.h"
#include "font.h"
#include "selector.h"
#include "player.h"
#include "util.h"

#define ALLSOLS_URL "/f/a/escape/allsols/"

struct smanage {
  // static void manage(Player *, string lmd5, Level *lev);

  static void promptupload(Drawable *below,
			   Player *, string lmd5, 
			   const Solution &s, string msg,
			   string name,
			   bool speedrec = false);

  // static void playback(Player *plr, Level *lev, NamedSolution *ns);
};

#endif
