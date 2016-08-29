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
#undef WALKED
#undef WALKEDEX
#undef PUSHED
#undef TOGGLE
#undef TRAP
#undef RET
#undef AFFECT
#undef AFFECTI
#undef PREAFFECTENTEX
#undef POSTAFFECTENTEX
#undef BOTEXPLODE

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
# define PREAFFECTENTEX(ei) if (ei == -1) ctx->preaffectplayer(this, etail); \
                            else ctx->preaffectbot(ei, this, etail);
# define POSTAFFECTENTEX(ei) if (ei == -1) ctx->postaffectplayer(); \
                             else ctx->postaffectbot(ei);

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
# define WALKEDEX(d, ex, ey, ei, push)  \
    PUSHMOVE(walk, e)                 \
      e->srcx = ex;                   \
      e->srcy = ey;                   \
      e->d = d;                       \
      e->pushing = push;              \
      e->whatunder =                  \
         tileat(ex, ey);              \
      e->entt =                       \
        (ei==-1)?B_PLAYER:bott[ei];   \
      e->data =                       \
        (ei==-1)?0:bota[ei];          \
    }
# define WALKED(d, push) WALKEDEX(d, entx, enty, enti, push)
# define PUSHED(d, w, x, y, u, z, h)  \
    PUSHMOVE(push, e)                 \
      e->srcx = x;                    \
      e->srcy = y;                    \
      e->d = d;                       \
      e->under = u;                   \
      e->what = w;                    \
      e->zap = z;                     \
      e->hole = h;                    \
    }
# define TOGGLE(xx, yy, t, d) \
    PUSHMOVE(toggle, e)       \
      e->x = xx;              \
      e->y = yy;              \
      e->whatold = t;         \
      e->delay = d;           \
    }
# define TRAP(xx, yy, t)   \
    PUSHMOVE(trap, e)      \
      e->x = xx;           \
      e->y = yy;           \
      e->whatold = t;      \
    }
# define BOTEXPLODE(botidx)             \
   PUSHMOVE(botexplode, e)              \
    where(boti[botidx], e->x, e->y);    \
   }

#else
# define SWAPO(idx) swapo(idx)
# define WALKED(a, b) do { ; } while (0)
# define WALKEDEX(a, b, c, d, e) do { ; } while (0)
# define PUSHED(a, b, c, d, e, f, g) do { ; } while (0)
# define TOGGLE(a, b, c, d) do { ; } while (0)
# define TRAP(a, b, c) do { ; } while (0)
# define AFFECT(a, b) false
# define AFFECTI(a) false
# define PREAFFECTENTEX(a) do { ; } while (0)
# define POSTAFFECTENTEX(a) do { ; } while (0)
# define BOTEXPLODE(a) do { ; } while (0)

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

/* must call this whenever a bot steps
   onto a tile where there might be other
   bots. (walking atop one, or teleporting)

   we assume that there is at most ONE other
   bot, by invariant (which this function restores).
*/
#define CHECKBOTDEATH(xx, yy, me)                       \
   do {                                                 \
   if (me != -1) { /* checked at end of turn */         \
     int mei = index(xx, yy);                           \
     for (int b = 0; b < nbots; b++) {                  \
        if (me != b && bott[b] != B_DELETED             \
            && bott[b] != B_BOMB_X                      \
            && mei == boti[b]) {                        \
            /* yes! delete other bot and make me */     \
            /* broken. */                               \
            (void)AFFECTI(mei);                         \
            bott[b] = B_DELETED;                        \
            bott[me] = B_BROKEN;                        \
            /* AFFECTENT;  */                           \
            BOTEXPLODE(b);                              \
        }                                               \
     }                                                  \
   } } while (0)

#ifndef ANIMATING_MOVE
/* generate callable versions of the macros */
void Level::checkstepoff(int x, int y) {
  CHECKSTEPOFF(x, y);
}

void Level::checkleavepanel(int x, int y) {
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
    bool m;
    #ifdef ANIMATING_MOVE
      m = moveent_animate(d, -1, GUYCAP, guyx, guyy,
                          events, ctx, etail);
    #else
      m = moveent(d, -1, GUYCAP, guyx, guyy);
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
          bool bm =
          #ifdef ANIMATING_MOVE
            moveent_animate(bd, b, bc, x, y,
                            events, ctx, etail);
          #else
            moveent(bd, b, bc, x, y);
          #endif

          /* try second choice */
          if (!bm && bd2 != DIR_NONE) {
            #ifdef ANIMATING_MOVE
              moveent_animate(bd2, b, bc, x, y,
                              events, ctx, etail);
            #else
              moveent(bd2, b, bc, x, y);
            #endif
          }

        }
      }

      RET(true);
    } else RET(false);

  }


#ifdef ANIMATING_MOVE
  bool Level::moveent_animate(dir d, int enti,
                              unsigned int cap, int entx, int enty,
                              AList *&events, Disamb *ctx,
                              AList **&etail) {
  //  printf("==== entity %d's turn\n", enti);
#else
  bool Level::moveent(dir d, int enti,
                      unsigned int cap, int entx, int enty) {
  //  #define printf if (0) printf
#endif

  int newx = 0, newy = 0;
  int target; // XXX2016 ok to move into decl?
  if (travel(entx, enty, d, newx, newy)) {
    switch (target = tileat(newx, newy)) {

    /* these aren't pressed by the player so act like floor */
    case T_BPANEL:
    case T_GPANEL:
    case T_RPANEL:

    /* these are only affected when we step *off* */
    case T_TRAP2:
    case T_TRAP1:

    case T_FLOOR:
    case T_ROUGH:
    case T_BDOWN:
    case T_RDOWN:
    case T_GDOWN:

    /* panels are mostly the same */
    case T_PANEL: {
#            ifdef ANIMATING_MOVE
               return MoveEntFloorlike<true, Disamb>(target, d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntFloorlike<false, NullDisamb>(target, d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_EXIT: {
#            ifdef ANIMATING_MOVE
               return MoveEntExit<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntExit<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
      }

    case T_ON: {
#            ifdef ANIMATING_MOVE
               return MoveEntOn<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntOn<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_0:
    case T_1: {
#            ifdef ANIMATING_MOVE
               return MoveEnt01<true, Disamb>(target, d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEnt01<false, NullDisamb>(target, d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_BSPHERE:
    case T_RSPHERE:
    case T_GSPHERE:
    case T_SPHERE:
    case T_GOLD: {
#            ifdef ANIMATING_MOVE
               return MoveEntGoldlike<true, Disamb>(target, d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntGoldlike<false, NullDisamb>(target, d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_TRANSPORT: {
#            ifdef ANIMATING_MOVE
               return MoveEntTransport<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntTransport<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif

    }

    case T_BUTTON: {
#            ifdef ANIMATING_MOVE
               return MoveEntButton<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntButton<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_BROKEN: {
#            ifdef ANIMATING_MOVE
               return MoveEntBroken<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntBroken<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_GREEN: {
#            ifdef ANIMATING_MOVE
               return MoveEntGreen<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntGreen<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_STEEL:
    case T_RSTEEL:
    case T_GSTEEL:
    case T_BSTEEL: {
#            ifdef ANIMATING_MOVE
               return MoveEntSteel<true, Disamb>(target, d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntSteel<false, NullDisamb>(target, d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    /* simple pushable blocks use this case */
    case T_TRANSPONDER:
    case T_RED:
    case T_NSWE:
    case T_NS:
    case T_NE:
    case T_NW:
    case T_SE:
    case T_SW:
    case T_WE:

    case T_LR:
    case T_UD:

    case T_GREY: {

    /* don't push a block that an entity stands on! */
    if (playerat(newx, newy) ||
        botat(newx, newy)) return false;


    /* we're always stepping onto the panel that
       the block was on, so we don't need to change
       its state. (if it's a regular panel, then
       don't change because our feet are on it. if
       it's a colored panel, don't change because
       neither the man nor the block can activate it.)
       But we do need to put a panel there
       instead of floor. */
    int replacement = (flagat(newx, newy) & TF_HASPANEL)?
              realpanel(flagat(newx,newy)):T_FLOOR;

    bool doswap = false;
    bool zap = false;
    (void)zap;
    bool hole = false;
    (void)hole;
    int destx, desty;


      if (target == T_LR && (d == DIR_UP || d == DIR_DOWN)) return(false);
      if (target == T_UD && (d == DIR_LEFT || d == DIR_RIGHT)) return(false);

      if (travel(newx, newy, d, destx, desty)) {

        int destt = tileat(destx, desty);
        if (playerat(destx, desty) || botat(destx, desty))
           return false;
        switch (destt) {
        case T_FLOOR:
          /* easy */
          settile(destx, desty, target);
          settile(newx, newy, replacement);
          break;
        case T_ELECTRIC:
          /* Zap! */
          if (target != T_LR && target != T_UD) {
            settile(newx, newy, replacement);
          } else return(false);
            zap = true;
          break;
        case T_HOLE:
          /* only grey blocks into holes */
          if (target == T_GREY) {
            settile(destx, desty, T_FLOOR);
            settile(newx, newy, replacement);
            hole = true;
            break;
          } else return(false);
        /* all panels are pretty much the same */
        case T_BPANEL:
        case T_RPANEL:
        case T_GPANEL:
        case T_PANEL:
          if (target != T_LR && target != T_UD) {
            /* delay the swap */
            /* (can only push down grey panels */
            doswap = (destt == T_PANEL);
            settile(destx, desty, target);
            settile(newx, newy, replacement);
          } else return(false);
          break;
        default: return(false);
        }
      } else return(false);

      /* Success! */

    (void)AFFECT(newx, newy);
    (void)AFFECT(destx, desty);
    PREAFFECTENTEX(enti);
    POSTAFFECTENTEX(enti);

    PUSHED(d, target, newx, newy, replacement, zap, hole);
    WALKED(d, true);

    CHECKSTEPOFF(entx, enty);

    SETENTPOS(newx, newy);

    if (doswap) {
      (void)AFFECTI(destat(destx, desty));
      SWAPO(destat(destx, desty));
    }

    return(true);
    }

    case T_HEARTFRAMER: {
#            ifdef ANIMATING_MOVE
               return MoveEntHeartframer<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntHeartframer<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_ELECTRIC: {
#            ifdef ANIMATING_MOVE
               return MoveEntElectric<true, Disamb>(d, enti,
                      (Capabilities)cap, entx, enty,
                      newx, newy, ctx, events, etail);
#            else
             // XXX 2016 pass along existing events, etail when
             // move takes those as well
               NullDisamb unused_disamb;
               PtrList<aevent> *unused = nullptr;
               AList **etail_unused = &unused;
               return MoveEntElectric<false, NullDisamb>(d, enti,
                      (Capabilities)cap, entx, enty, newx, newy,
                      &unused_disamb, unused, etail_unused);
#            endif
    }

    case T_BLUE:
    case T_HOLE:
    case T_LASER:
    case T_STOP:
    case T_RIGHT:
    case T_LEFT:
    case T_UP:
    case T_DOWN:
    case T_BLIGHT:
    case T_RLIGHT:
    case T_GLIGHT:
    case T_RUP:
    case T_BUP:
    case T_GUP:
    case T_OFF:
    case T_BLACK:

      /* XXX animate pushing up against these
         unsuccessfully (anim 'press?') */

    default:
      return false;

    }
  } else return false;
}

#ifdef PUSHMOVE
# undef PUSHMOVE
#endif
