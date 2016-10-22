
#ifndef __BROWSE_H
#define __BROWSE_H

/* New (4.0) version of level browser. Implements the UI on top of
   leveldb.

   Since it's based on leveldb, it can only be used for that global
   player (this is natural, anyway, because you only want to browse
   from the perspective of the logged-in player).
 */

#include "escapex.h"
#include "font.h"
#include "selector.h"
#include "player.h"
#include "util.h"

/* abstract interface */
struct Browse : public Drawable {
  static Browse *Create(bool allow_corrupted = false);
  virtual ~Browse() {}

  /* Display the browser modally until the user selects a level;
     return a filename for that level or the empty string if
     the user cancels. */
  virtual string SelectLevel() = 0;

  /* Drawable */
  void Draw() override = 0;
  void ScreenResize() override = 0;
};

#endif
