/*          -*- text -*-

    (c-mode can't figure this mess out)
*/

/* sorry about this.

   because we want to use this code twice, once in highly
   optimiz(able) form for use on the server and in non-interactive
   solution verification, and once in interactive form, returning
   animations, we need to generate two different functions from the
   same source. (It's also important to minimize the amount of
   dependencies that the server code has.) The two choices in C are to
   make the function a macro, or to use this include hack.

   If this code is included with ANIMATING_MOVE defined, then
   it generates a function move_animate, otherwise it generates
   plain ol' move.
*/

// Notes for templatizing this thing, 2016:
// Two goals overall:
//   - efficient version of move() that doesn't pay for animation
//   - ability to compile level without dependency on SDL.
//
// ...
// Before I started on 27 Aug 2016, tom7 had 1282/2509 Triage levels solved,
// and 485/803 minor leagues, plus obviously 34/34 regressions and 22/22
// official levels.

using AList = PtrList<aevent>;

/* pushmove is used like this:
   XXX why is this called pushmove??

   PUSHMOVE(fly, e) (* note: no opening brace *)
     e->srcx = 10;
     e->srcy = 20;
     ...
   }
*/

#undef SWAPO
#undef PUSHMOVE
#undef CHECKLEAVEPANEL
#undef CHECKSTEPOFF
#undef TRAP
#undef RET
#undef AFFECT
#undef AFFECTI

/* We need to correctly track the position of
   an entity during a move. We have the variables
   entx and enty, but they can't be references because
   we store some positions as a single integer index
   (bots in the boti array). So instead, we use this
   function to reflect changes to entx/enty in the
   permanent store. */
#define SETENTPOS(xx, yy) do {       \
   entx = xx;                        \
   enty = yy;                        \
   SetEntPos(enti, xx, yy);          \
 } while (0)

#ifdef ANIMATING_MOVE
# include "util.h"
# include "aevent.h"

# define AFFECT(x, y) ctx->affect(x, y, this, etail)
# define AFFECTI(i) ctx->affecti(i, this, etail)

# define PUSHMOVE(type, var) {        \
    aevent *a ## var = new aevent;    \
    *etail = new AList(a ## var, 0);  \
    etail = &((*etail)->next);        \
    a ## var->serial = ctx->Serial(); \
    a ## var->t = tag_ ## type;       \
    type ## _t *var = &(a ## var->u. type);

# define SWAPO(idx)                   \
    PUSHMOVE(swap, e)                 \
      int xx, yy;                     \
      where(idx, xx, yy);             \
      e->x = xx;                      \
      e->y = yy;                      \
      e->was = tileat(xx, yy);        \
      e->now = otileat(xx, yy);       \
      swapo(idx);                     \
   }
# define TRAP(xx, yy, t)   \
    PUSHMOVE(trap, e)      \
      e->x = xx;           \
      e->y = yy;           \
      e->whatold = t;      \
    }

#else
# define SWAPO(idx) swapo(idx)
# define TRAP(a, b, c) do { ; } while (0)
# define AFFECT(a, b) false
# define AFFECTI(a) false

#endif

/* helper functions */
/* after stepping off a tile, deactivate a panel
   if there was one there. */
/* nb: only for regular panels */
#define CHECKLEAVEPANEL(xx, yy) do {  \
  if (tileat(xx, yy) == T_PANEL) {    \
    (void)AFFECTI(destat(xx, yy));    \
    SWAPO(destat(xx, yy));            \
  }                                   \
} while (0)

#define CHECKTRAP(xx, yy) do {             \
  if (tileat(xx, yy) == T_TRAP1) {         \
    (void)AFFECT(xx, yy);                  \
    settile(xx, yy, T_HOLE);               \
    TRAP(xx, yy, T_TRAP1);                 \
  } else if (tileat(xx, yy) == T_TRAP2) {  \
    (void)AFFECT(xx, yy);                  \
    settile(xx, yy, T_TRAP1);              \
    TRAP(xx, yy, T_TRAP2);                 \
  }                                        \
} while (0)

/* actions on the player stepping off of a tile */
/* generally, you should only call this once per
   motion, at the very end. that's because it may
   install new panels (by swapping), and panel swaps
   are supposed to happen at the end. */
#define CHECKSTEPOFF(xx, yy) do {          \
  CHECKTRAP(xx, yy);                       \
  CHECKLEAVEPANEL(xx, yy);                 \
} while (0)

#ifndef ANIMATING_MOVE
/* generate callable versions of the macros */
/* n.b. this is used */
void Level::checkstepoff(int x, int y) {
  CHECKSTEPOFF(x, y);
}

#endif

#ifdef ANIMATING_MOVE
template<class DAB>
static void postanimate(Level *l, DAB *ctx,
                        AList *&events, AList **&etail) {

  /* make sure there is animation for everything */
  //  printf("... postanimate ...\n");
  ctx->serialup(l, etail);

  int lx, ly; dir from;
  if (l->isdead(lx, ly, from)) {
    /* XXX or affect? */

    /* lite up laser tile (lx, ly), too  ... if laser death */
    ctx->preaffectplayer(l, etail);
    ctx->postaffectplayer();
    PUSHMOVE(lasered, e)
      e->x = l->guyx;
      e->y = l->guyy;
      e->lx = lx;
      e->ly = ly;
      e->from = dir_reverse(from);
    }

    ctx->serialup(l, etail);

  } else if (l->iswon()) {

    ctx->preaffectplayer(l, etail);
    ctx->postaffectplayer();

    PUSHMOVE(winner, e)
      e->x = l->guyx;
      e->y = l->guyy;
    }

    ctx->serialup(l, etail);
  }
}

/* always increment the serial at the end, which
   maintains the invt that every phase has a player
   motion in 'events.' Finally, call postanimate
   to add in winning or death events. */
// XXX this is just used in one place now, so inline
# define RET(b) do { postanimate<Disamb>(this, ctx, events, etail); \
                     return (b); } while (0)

#else

# define RET(b) return (b);

#endif


#ifdef ANIMATING_MOVE
  bool Level::move_animate(dir d, Disamb *ctx, AList *&events) {
  events = nullptr;
  AList **etail = &events;
  ctx->clear();

  //  printf("----start move----\n");
#else
  bool Level::move(dir d) {
#endif

    /* change our orientation, even if we don't move.
       XXX animate this!
    */
    guyd = d;

    /* player always moves first */
    #ifdef ANIMATING_MOVE
      bool m = MoveEnt<true, Disamb>(d, -1, (Capabilities)GUYCAP, guyx, guyy,
                                     ctx, events, etail);
    #else
      // XXX 2016 fix
      NullDisamb unused_disamb;
      PtrList<aevent> *unused = nullptr;
      AList **etail_unused = &unused;
      bool m = MoveEnt<false, NullDisamb>(
           d, -1, (Capabilities)GUYCAP, guyx, guyy,
           &unused_disamb, unused, etail_unused);
    #endif

    if (m) {

      for (int b = 0; b < nbots; b++) {

        int x, y;
        where(boti[b], x, y);

        dir bd = DIR_NONE; /* dir to go */
        dir bd2 = DIR_NONE; /* second choice */
        unsigned int bc = 0; /* its capabilities */

    if (Level::isbomb(bott[b])) {
      /* bombs never move */
      bd = DIR_NONE;

      if (bota[b] == 0) {
         /* time's up: explodes */
#            ifdef ANIMATING_MOVE
               Bombsplode<true, Disamb>(b, b, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               Bombsplode<false, NullDisamb>(b, b, &unused_disamb, unused, etail_unused);
#            endif

      } else if (bota[b] > 0) {
             /* fuse burns */
             bota[b]--;
             /* XXX animate? */
      } else {
        /* unlit: do nothing */
      }
          continue;

    } else
        switch (bott[b]) {
          /* nb, not isbomb */
          case B_BOMB_X:
             /* disappear */
             bott[b] = B_DELETED;
             /* no animation since it's already invisible */
             bd = DIR_NONE;
          break;

          /* these two are the same except for their
             capabilities */
          case B_HUGBOT:
          case B_DALEK: {
            /* dalek always moves towards player, favoring
               left/right movement */

            if (x == guyx) {
              /* same column? move up/down */
              if (y < guyy) bd = DIR_DOWN;
              else if (y > guyy) bd = DIR_UP;
              else bd = DIR_NONE; /* on player !! EXTERMINATE */
            } else {
              if (x > guyx) bd = DIR_LEFT;
              else bd = DIR_RIGHT;

              /* but still set second choice */
              if (y < guyy) bd2 = DIR_DOWN;
              else if (y > guyy) bd2 = DIR_UP;
            }

            switch (bott[b]) {
            default: /* impossible */
            case B_DALEK: bc = DALEKCAP; break;
            case B_HUGBOT: bc = HUGBOTCAP; break;
            }
          break;
          }
          default: bd = DIR_NONE;
          break;
        }

        if (bd != DIR_NONE) {
          #ifdef ANIMATING_MOVE
          bool bm = MoveEnt<true, Disamb>(bd, b, (Capabilities)bc, x, y,
                                          ctx, events, etail);
          #else
          // XXX 2016 fix
          NullDisamb unused_disamb;
          PtrList<aevent> *unused = nullptr;
          AList **etail_unused = &unused;
          bool bm = MoveEnt<false, NullDisamb>(
               bd, b, (Capabilities)bc, x, y,
               &unused_disamb, unused, etail_unused);
          #endif

          /* try second choice */
          if (!bm && bd2 != DIR_NONE) {
            #ifdef ANIMATING_MOVE
            (void)MoveEnt<true, Disamb>(bd2, b, (Capabilities)bc, x, y,
                                        ctx, events, etail);
            #else
            // XXX 2016 fix
            NullDisamb unused_disamb;
            PtrList<aevent> *unused = nullptr;
            AList **etail_unused = &unused;
            (void)MoveEnt<false, NullDisamb>(
                 bd2, b, (Capabilities)bc, x, y,
                 &unused_disamb, unused, etail_unused);
            #endif
          }
        }
      }

      RET(true);
    } else RET(false);

  }



#ifdef PUSHMOVE
# undef PUSHMOVE
#endif
