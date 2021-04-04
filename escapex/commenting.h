
#ifndef _ESCAPE_COMMENTING_H
#define _ESCAPE_COMMENTING_H

#include <string>

#include "player.h"
#include "level.h"
#include "escapex.h"

struct CommentScreen {
  static void Comment(Player *plr, const Level *l, const std::string &md5,
                      bool cookmode = false);
};

#endif
