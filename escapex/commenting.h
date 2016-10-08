
#ifndef __COMMENTING_H
#define __COMMENTING_H

#include "escapex.h"

struct CommentScreen {
  static void Comment(Player *plr, const Level *l, const string &md5,
                      bool cookmode = false);
};

#endif
