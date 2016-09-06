
#ifndef __OPTIMIZE_H
#define __OPTIMIZE_H

#include "level.h"
#include "player.h"

/* solution optimization. */

struct Optimize {
  /* starting with a (legal) solution, try to optimize it to a shorter
     solution, and return that. The returned solution may be the same
     as the argument, but if it isn't, then it will be a valid solution
     for the level, and no more moves than the input solution. */
  static Solution Opt(const Level *l, const Solution &s);

  /* starting with a partial solution 'prefix', try to
     find the minimal suffix of solutions within 'sources'
     that solves l. If successful, return true and set sol to
     a complete solution. */
  static bool TryComplete(Level *l, const Solution &prefix,
                          const vector<NamedSolution> &sources,
                          Solution *sol);
};

#endif
