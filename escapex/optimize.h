
#ifndef __OPTIMIZE_H
#define __OPTIMIZE_H

#include "level.h"
#include "player.h"

/* solution optimization. */

struct Optimize {
  /* starting with a (legal) solution, try to optimize it to a shorter
     solution, and return that. Neither argument is destroyed (and the
     return will be newly allocated, even if it no different from the
     input s). */
  static Solution *opt(Level *l, Solution *s);

  /* starting with a partial solution 'prefix', try to
     find the minimal suffix of solutions within 'sources'
     that solves l. If successful, return l. */
  static Solution *trycomplete(Level *l, Solution *prefix,
			       PtrList<NamedSolution> *sources);
};

#endif
