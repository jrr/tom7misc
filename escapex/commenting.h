
#ifndef __COMMENTING_H
#define __COMMENTING_H

#include "escapex.h"

struct CommentScreen {
  static void comment(Player *plr, Level *l, string md5,
		      bool cookmode = false);
};

#endif
