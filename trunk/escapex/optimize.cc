
#include "escapex.h"
#include "optimize.h"
#include "message.h"
#include "hashtable.h"
#include "solution.h"
#include "player.h"
#include "level.h"
#include "extent.h"

/* hash table entry. corresponds to a level state
   and the earliest position (in the solution) at
   which we are in this state.

   we never check actual level equality, just hash
   equality. This means we can screw up in the
   presence of collisions, but we check our work
   when we're done, so this is only a performance
   issue rather than a correctness one. */

#define ROR64(h, n) (((h) >> (n)) | ((h) << (64 - (n))))
/* from md5 */
#define F1(x, y, z) (z ^ (x & (y ^ z)))


/* XXX it's likely that some of these operations are actually
   defaulting to 32 bits, which wastes entropy */

/* non-linear permutation */
inline void PERMUTE1(Uint64 & h) {
  /* always rotate at least one position,
     but be dependent on the value of h */
  h = ROR64(h, (h & 31) + 1);

  h = (((h & 0xF0F0F0F0F0F0F0F0ULL) >> 4) |
       ((h & 0x0F0F0F0F0F0F0F0FULL) << 4));

  h =  (h &  0xFFFFFFFF00000000ULL) |
      ((h &  0x00000000FFFF0000ULL) >> 16) |
      ((h &  0x000000000000FFFFULL) << 16);
}

/* linear */
inline void PERMUTE2(Uint64 & h) {
  h = ((h >> 32) * 0x12345678ULL) |
      ((h & 0xFFFFFFFFULL) * 0x09ABCDEFULL);
  h ^= 0x112233FFEEDDCCULL;
}

/* linear */
inline void PERMUTE3(Uint64 & h) {
  h ^= 0x1F1F2E2E3D3D4C4CULL;
  h = ((h >> 32) * 0x19283746ULL) |
      ((h & 0xFFFFFFFFULL) * 0xF9E8D7C6ULL);
}


static Uint64 hashlevel(const Level *l) {
  /* ignore title, author, w/h, dests, flags,
     since these don't change.
     also ignore botd and guyd, which are presentational. */
  Uint64 h = 0x3333333333333333ULL;

  /* start with position of guy */
  h ^= ((Uint64)l->guyx << 31);
  h ^=  l->guyy;

  PERMUTE2(h);

  /* hash up tiles */
  for (int i = 0; i < l->w * l->h; i++) {
    h ^= (Uint64)l->tiles[i];
    PERMUTE1(h);
    h += (Uint64)l->otiles[i];
    PERMUTE2(h);
  }

  /* then bots */
  for (int j = 0; j < l->nbots; j++) {
    h = ROR64(h, j);
    h ^= (Uint64)l->bott[j];
    PERMUTE3(h);
    h ^= (Uint64)l->boti[j];
    PERMUTE1(h);
    h ^= (Uint64)l->bota[j];
    PERMUTE2(h);
  }

  return h;
}

namespace {
struct LState {
  /* index (in cf) before which we are in this
     state */
  int pos;

  /* if 0, the entry has been invalidated */
  Uint64 thiskey;

  Uint64 key() const {
    return thiskey;
  }

  /* truncate */
  static unsigned int hash(Uint64 i) {
    return (unsigned int)(i & 0xFFFFFFFFl);
  }

  LState(int p, const Level *ll, bool initial = false) : pos(p) {
    thiskey = hashlevel(ll);
    /* need to treat this case specially, or else
       the Level "impossible" (and ones like it) get
       optimized to a zero-length solution, which won't
       verify */
    if (ll->iswon() && initial) thiskey ^= 0xFFFF0000FFFF0000ULL;

    /* 0 is used for something special */
    if (!thiskey) thiskey++;
  }

  void destroy() {
    delete this;
  }
};
}

static void inval_above(LState *ls, int cutoff) {
  if (ls->pos > cutoff) ls->thiskey = 0l;
}

// static
Solution Optimize::Opt(const Level *orig, const Solution &s) {
  std::unique_ptr<Level> l = orig->Clone();

  if (!Level::Verify(l.get(), s)) {
    Message::Bug(0, "optimizer: solution is not valid to start!");
    return s;
  }

  /* first, a conservative optimization pass.

     this is simple: for each move we make, we hash the current
     level state. If we ever re-enter the same state later, then
     we can cut out the "cycle". Lots of pointless walking around
     or panel testing can easily cause such cycles.

     Note, this optimization pass could be better. We are very
     eager about excising cycles, but this means that we might not
     get rid of the "best" cycles. Instead, we could build the
     actual state graph and then find the shortest path. In my
     opinion this suboptimal behavior is very rare. */

  /* "cycle-free" solution */
  Solution cf;

  // XXX2016 in for?
  Solution::iter i(s);
  hashtable<LState, Uint64> *ht = hashtable<LState, Uint64>::create(1023);
  Extent<hashtable<LState, Uint64>> eh(ht);

  /* insert initial state */
  ht->insert(new LState(0, l.get(), true));

  for (; i.hasnext(); i.next()) {
    dir d = i.item();

    /* execute the move. If it didn't
       do anything, we'll just ignore
       it. (These shouldn't make their way
       into solutions during regular play,
       anyway.) */
    if (l->Move(d)) {

      /* provisionally execute this move in our
         cycle-free solution. */
      cf.Append(d);
      int n = cf.Length();

      /* now check if we've already been here. */

      LState *ls = new LState(n, l.get());

      LState *existing = ht->lookup(ls->key());

      if (existing) {
        // printf("   found! at %d\n", existing->pos);
        /* Well, some prefix of the existing solution
           already gets us here. Instead of making this
           move, we should just backtrack to the point
           at which we were in this state! */
        cf.Truncate(existing->pos);

        /* now, anything we've done since then is
           invalidated, (this is where the sub-optimality
           comes in) so "remove" it. The hashtable doesn't
           have a deletion method, so we just set its hash
           key to zero so we'll never find it again (this
           is actually fairly bogus since it will be in the
           wrong bin, now, but we aren't going to try to find
           it!) */

        hashtable_app<LState, Uint64, int>(ht, inval_above, cf.Length());

        /* don't need this any more */
        delete ls;

      } else {
        /* new state, so insert it */
        ht->insert(ls);
      }
    }
  }

  if (!Level::Verify(orig, cf)) {
    printf("Result didn't verify!!\n");
    return s;
  }

  if (cf.Length() < s.Length()) {
    /*
    printf("Correct result: %d moves->%d moves\n",
           s->length, cf->length);
    */
    return cf;
  } else {
    return s;
  }
}

// static
bool Optimize::TryComplete(Level *start, const Solution &prefix,
                           const vector<NamedSolution> &sources,
                           Solution *sol) {
  /* do breadth first search by suffix length. */

  std::unique_ptr<Level> wprefix = start->Clone();
  int moves_unused;
  /* Assume this works.. */
  wprefix->Play(prefix, moves_unused);

  /* find the longest solution. */
  int max_slen = 0;
  for (const NamedSolution &ns : sources) {
    max_slen = std::max(ns.sol.Length(), max_slen);
  }

  for (int slen = 0; slen < max_slen; slen++) {
    /* for each solution that is at least this many moves,
       try its suffix */
    for (const NamedSolution &ns : sources) {
      if (ns.sol.Length() >= slen) {
        /* do it! */
	std::unique_ptr<Level> trysuf = wprefix->Clone();

        const Solution &trysol = ns.sol;

        /* execute exactly this suffix */
        int moves;
        if (trysuf->PlayPrefix(trysol, moves,
                               trysol.Length() - slen,
                               slen)) {
          /* success! (with move moves) */
          *sol = prefix;
          for (int j = 0; j < moves; j++) {
            sol->Append(trysol.At(j + trysol.Length() - slen));
          }

          return true;
        }
      }
    }
    // printf("No suffixes of length %d...\n", slen);
  }

  return false;
}
