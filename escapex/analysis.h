#ifndef __ANALYSIS_H
#define __ANALYSIS_H

#include "base.h"
#include "../cc-lib/union-find.h"
#include "level.h"

/* analysis of levels. used for editai,
   maybe for solution analysis, etc */

struct Analysis {
  /* get the set of equivalence classes */
  static std::unique_ptr<UnionFind> Reachable(Level *);

  /* the tile at x,y is a separator if:

     - for any tile x2,y2 =/= x,y that is in the
       same equivalence class as x1,y1, updating the
       level with a block at x,y makes x1,y1,x2,y2
       become different equivalence classes

       the idea is that the block is some sort of
       critical place separating two parts of the
       level, so it would make a good target for
       a panel or some other removable item.

     this operation is fairly expensive (at least
     linear in the size of the level)
  */
  static bool IsSep(Level *, int x, int y,
                    int x1, int y1,
                    int &x2, int &y2, int tile = T_STOP);

  /* same, but provide x2 and y2 as inputs: adding
     tile at x/y must separate previously connected
     x1/y1 and x2/y2. */
  static bool DoesSep(Level *,
                      int x, int y,
                      int x1, int y1,
                      int x2, int y2, int tile = T_STOP);

  /* player can stand on this tile */
  static bool IsEmpty(int tile);
};

#endif
