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

/* Capabilities of entities (players and bots). */

#ifndef GUYCAP
# define CAP_ISPLAYER 1
# define CAP_CANTELEPORT 2
# define CAP_CRUSHPLAYER 4
# define CAP_WALKINTOBOTS CAP_CRUSHPLAYER
# define CAP_PUSHPLAYER 8
# define CAP_ZAPSELF CAP_WALKINTOBOTS

# define CAP_PUSHBOTS CAP_PUSHPLAYER

# define CAP_HEARTFRAMERS 16

/* should have cap for 'unpushable',
   but we don't have any such robots yet. */

# define GUYCAP (CAP_ISPLAYER | CAP_CANTELEPORT | CAP_PUSHBOTS | CAP_HEARTFRAMERS)
# define DALEKCAP (CAP_CANTELEPORT | CAP_CRUSHPLAYER | CAP_WALKINTOBOTS)
# define HUGBOTCAP (CAP_PUSHPLAYER | CAP_PUSHBOTS)
#endif

#undef AM
#undef SWAPO
#undef PUSHMOVE
#undef CHECKLEAVEPANEL
#undef CHECKSTEPOFF
#undef WALKED
#undef WALKEDEX
#undef PUSHED
#undef TOGGLE
#undef BUTTON
#undef TRAP
#undef PUSHGREEN
#undef LITEWIRE
#undef OPENDOOR
#undef RET
#undef AFFECT
#undef AFFECTI
#undef PREAFFECTENTEX
#undef POSTAFFECTENTEX
#undef BOTEXPLODE
#undef WAKEUPDOOR
#undef WAKEUP
#undef STANDBOT
#undef GETHEARTFRAMER
#undef TELEPORTOUT
#undef TELEPORTIN
#undef TRANSPONDERBEAM
#undef BOMBSPLOSION

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
   if (enti == B_PLAYER) {           \
     guyx = xx;                      \
     guyy = yy;                      \
   } else {                          \
     boti[enti] = index(xx, yy);     \
   }                                 \
 } while (0)

#define SETENTDIR(d) do {            \
   if (enti == B_PLAYER) {           \
     guyd = d;                       \
   } else {                          \
     botd[enti] = d;                 \
   }                                 \
 } while (0)

#ifdef ANIMATING_MOVE
# include "util.h"
# include "aevent.h"
# define AM

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
# define BUTTON(xx, yy, t) \
    PUSHMOVE(button, e)    \
      e->x = xx;           \
      e->y = yy;           \
      e->whatold = t;      \
    }
# define TRAP(xx, yy, t)   \
    PUSHMOVE(trap, e)      \
      e->x = xx;           \
      e->y = yy;           \
      e->whatold = t;      \
    }
# define PUSHGREEN(xx, yy, dd) \
    PUSHMOVE(pushgreen, e)     \
      e->srcx = xx;            \
      e->srcy = yy;            \
      e->d = dd;               \
    }
# define LITEWIRE(xx, yy, wh, pd, cc) \
   PUSHMOVE(litewire, e)          \
      e->x = xx;                  \
      e->y = yy;                  \
      e->what = wh;               \
      e->dir = pd;                \
      e->count = cc;              \
   }
# define OPENDOOR(xx, yy) \
   PUSHMOVE(opendoor, e)  \
      e->x = xx;          \
      e->y = yy;          \
   }
# define BOTEXPLODE(botidx)             \
   PUSHMOVE(botexplode, e)              \
    where(boti[botidx], e->x, e->y);    \
   }
# define WAKEUPDOOR(xx, yy)             \
   PUSHMOVE(wakeupdoor, e)              \
     e->x = xx; e->y = yy;              \
   }
# define WAKEUP(xx, yy)                 \
   PUSHMOVE(wakeup, e)                  \
     e->x = xx; e->y = yy;              \
   }
# define STANDBOT(i)                    \
   PUSHMOVE(stand, e)                   \
     where(boti[i], e->x, e->y);        \
     e->d = botd[i];                    \
     e->entt = bott[i];                 \
     e->data = bota[i];                 \
   }
# define GETHEARTFRAMER(xx, yy)         \
   PUSHMOVE(getheartframer, e)          \
     e->x = xx; e->y = yy;              \
   }
# define TELEPORTOUT(ei, xx, yy)           \
   PUSHMOVE(teleportout, e)                \
     e->x = xx; e->y = yy;                 \
     e->entt = (ei==-1)?B_PLAYER:bott[ei]; \
   }
# define TELEPORTIN(ei, xx, yy)            \
   PUSHMOVE(teleportin, e)                 \
     e->x = xx; e->y = yy;                 \
     e->entt = (ei==-1)?B_PLAYER:bott[ei]; \
   }

# define TRANSPONDERBEAM(xx, yy, destx, desty, fr, delay) \
    PUSHMOVE(transponderbeam, e)        \
      e->x = destx;                     \
      e->y = desty;                     \
      e->lx = xx;                       \
      e->ly = yy;                       \
      e->from = fr;                     \
      e->count = delay;                 \
    }
# define BOMBSPLOSION(xx, yy)   \
    PUSHMOVE(bombsplosion, e)      \
      e->x = xx;           \
      e->y = yy;           \
    }
#else
# define SWAPO(idx) swapo(idx)
# define WALKED(a, b) do { ; } while (0)
# define WALKEDEX(a, b, c, d, e) do { ; } while (0)
# define OPENDOOR(a, b) do { ; } while (0)
# define PUSHED(a, b, c, d, e, f, g) do { ; } while (0)
# define TOGGLE(a, b, c, d) do { ; } while (0)
# define BUTTON(a, b, c) do { ; } while (0)
# define TRAP(a, b, c) do { ; } while (0)
# define PUSHGREEN(a, b, c) do { ; } while (0)
# define LITEWIRE(a, b, c, d, e) do { ; } while (0)
# define AFFECT(a, b) false
# define AFFECTI(a) false
# define PREAFFECTENT /* nuthin' */
# define PREAFFECTENTEX(a) do { ; } while (0)
# define POSTAFFECTENT /* nuthin' */
# define POSTAFFECTENTEX(a) do { ; } while (0)
# define BOTEXPLODE(a) do { ; } while (0)
# define WAKEUPDOOR(a, b) do { ; } while (0)
# define WAKEUP(a, b) do { ; } while (0)
# define STANDBOT(i) do { ; } while (0)
# define GETHEARTFRAMER(a, b) do { ; } while (0)
# define TRANSPONDERBEAM(xx, yy, destx, desty, fr, delay) do { ; } while (0)
# define TELEPORTOUT(ei, xx, yy) do { ; } while (0)
# define TELEPORTIN(ei, xx, yy) do { ; } while (0)
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

#define SWAPTILES(t1, t2, d) do { \
  for (int y = 0; y < h; y++) {   \
  for (int x = 0; x < w; x++) {   \
   int t = tileat(x, y);          \
   if (t == t1) {                 \
     (void)AFFECT(x, y);          \
     TOGGLE(x, y, t, d);          \
     settile(x, y, t2);           \
   } else if (t == t2) {          \
     (void)AFFECT(x, y);          \
     TOGGLE(x, y, t, d);          \
     settile(x, y, t1);           \
   }                              \
  } }                             \
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

void Level::swaptiles(int t1, int t2) {
  SWAPTILES(t1, t2, 0);
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
    #ifdef AM
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
          #ifdef AM
            moveent_animate(bd, b, bc, x, y,
                            events, ctx, etail);
          #else
            moveent(bd, b, bc, x, y);
          #endif

          /* try second choice */
          if (!bm && bd2 != DIR_NONE) {
            #ifdef AM
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
  int target;
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

      bool pushing = false;
      int pushent = 0;
      if (playerat(newx, newy)) {
         /* if player is on bot, can't push */
         if (botat(newx, newy)) return false;
         /* push? */
         if (cap & CAP_PUSHPLAYER) {
           pushing = true;
           pushent = -1;
           /* step on? */
         } else if (cap & CAP_CRUSHPLAYER) {
           pushing = false;
         } else return false;
      } else if (botat(newx, newy, pushent)) {
         /* push? */
         if (cap & CAP_PUSHBOTS) {
            /* check that the target bot is pushable */
            pushing = true;
         /* step on? */
         } else if (cap & CAP_WALKINTOBOTS) {
            pushing = false;
         } else return false;
      }

      if (pushing) {
        /* OK, we know that the next spot
           contains a bot (or player), and
           we have the capability to push that
           entity. But we have to check what
           lives beyond this spot. */
        int farx, fary;
        if (travel(newx, newy, d, farx, fary)) {
           int ftarget = tileat(farx, fary);
           switch (ftarget) {
             case T_ELECTRIC:
                /* only bots pushed into electric */
                if (pushent == -1) return false;
                else break;
             case T_TRAP2:
             case T_TRAP1:
             case T_FLOOR:
             case T_ROUGH:
             case T_RDOWN:
             case T_GDOWN:
             case T_BDOWN:
             case T_PANEL:
             case T_RPANEL:
             case T_GPANEL:
             case T_BPANEL:
                break;
             default:
                return false;
           }
           /* also check bot -- can't push
              two in a row */
           if (botat(farx, fary)) return false;
           if (playerat(farx, fary)) return false;

           /* affect first, then make anims */

           PREAFFECTENTEX(enti);
           PREAFFECTENTEX(pushent);
           (void)AFFECT(farx, fary);
           (void)AFFECT(newx, newy);

           /* if a bomb, light and reset fuse */
           /* XXX animate? */
           if (pushent != -1 &&
           isbomb(bott[pushent]))
               bota[pushent] = ((int)bott[pushent] - (int)B_BOMB_0);

           POSTAFFECTENTEX(enti);
           POSTAFFECTENTEX(pushent);

           /* XXX should be "waspushed" or whatever */
           WALKEDEX(d, newx, newy, pushent, false);
           /* okay, now move the pushing ent */
           WALKED(d, true);


           /* okay, now move the pushing ent */
           //           WALKED(d, true);

           // printf("now change positions...\n");
           /* okay, push! */
           if (pushent == -1) {
              guyx = farx;
              guyy = fary;
           } else {
              int id = index(farx, fary);
              boti[pushent] = id;
           }

           /* handle leaving current (pusher) pos */
           CHECKTRAP(entx, enty);
           /* but still need to check panels, later... */
           int srcx = entx, srcy = enty;
           bool swapsrc = tileat(entx, enty) == T_PANEL;

           /* then move me. */
           SETENTPOS(newx, newy);

           /* now deal with zapping */
           if (ftarget == T_ELECTRIC &&
               pushent != -1) {
              /* can't be player: kill bot */
              bott[pushent] = B_DELETED;
              (void)AFFECT(farx, fary);
              BOTEXPLODE(pushent);
           }

           /* the tile in the middle is being stepped off
              and stepped on. if it's a panel, don't do anything.
              (to avoid a double swap) */
           if (target == T_PANEL) {
              /* do nothing */
           } else {
              CHECKTRAP(newx, newy);
           }

           /* -- panel phase -- */

           /* first, if pusher stepped off a panel, it swaps */
           if (swapsrc) {
              (void)AFFECTI(destat(srcx, srcy));
              SWAPO(destat(srcx, srcy));
           }

           /* pushed ent is stepping onto new panel, perhaps */
           if (ftarget == T_PANEL) {
              (void)AFFECTI(destat(farx, fary));
              SWAPO(destat(farx, fary));
           }

           return true;

        } else return false;

      } else {
        /* XXX also affect source? */
        PREAFFECTENTEX(enti);
        (void)AFFECT(newx, newy);
        POSTAFFECTENTEX(enti);
        WALKED(d, false);

        //        printf("first ent is at %d/%d\n", entx, enty);

        /* might have stepped onto bot */
        CHECKBOTDEATH(newx, newy, enti);

        /* panel actions */
        //        printf(" %d   Checkstepoff...\n", enti);
        CHECKSTEPOFF(entx, enty);

        SETENTPOS(newx, newy);

        //        printf("now ent is at %d/%d\n", entx, enty);


        if (target == T_PANEL) {
          // printf(" %d   step on panel...\n", enti);
          (void)AFFECTI(destat(newx, newy));
          SWAPO(destat(newx, newy));
        }

        return(true);
      }
    }
    case T_EXIT:

      /* XXX check cap */

      /* can't push bots off exits */
      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      CHECKSTEPOFF(entx, enty);

      PREAFFECTENTEX(enti);
      POSTAFFECTENTEX(enti);
      WALKED(d, false);

      SETENTPOS(newx, newy);

      /* open door */
      (void)AFFECT(newx, newy);
      OPENDOOR(newx, newy);

      return(true);

    case T_ON: {
      /* can't activate if someone stands on it */
      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      for (int y = 0; y < h; y++)
        for (int x = 0; x < w; x++) {
          if (tileat(x, y) == T_ELECTRIC) {
            (void)AFFECT(x, y);
            TOGGLE(x, y, T_ELECTRIC, abs(x - newx) + abs(y - newy));
            settile(x, y, T_FLOOR);
          }
        }

      (void)AFFECT(newx, newy);
      settile(newx, newy, T_OFF);
      /* XXX animate pushing, but not moving */
      BUTTON(newx, newy, T_ON);
      return(true);
    }
    case T_0:
    case T_1: {

      /* ditto */
      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;


      int opp = (target == T_0 ? T_1 : T_0);

      SWAPTILES(T_UD, T_LR, 0);

      (void)AFFECT(newx, newy);
      settile(newx, newy, opp);
      BUTTON(newx, newy, target);

      return(true);
    }

    case T_BSPHERE:
    case T_RSPHERE:
    case T_GSPHERE:
    case T_SPHERE:
    case T_GOLD: {

      /* spheres allow pushing in a line:
         ->OOOO  becomes OOO   ---->O

         so keep traveling while the tile
         in the destination direction is
         a sphere of any sort.
      */
      #ifdef AM
      int firstx = newx, firsty = newy;
      #endif
      int tnx, tny;
      while (issphere(tileat(newx, newy)) &&
             !(playerat(newx, newy) ||
               botat(newx, newy)) &&
             travel(newx, newy, d, tnx, tny) &&
             issphere(tileat(tnx, tny))) {
        newx = tnx;
        newy = tny;
        target = tileat(tnx, tny);
      }

      /* can't push if we ran into entity */
      /* XXX jiggle anyway */
      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      #ifdef AM
      /* if we passed through any spheres,
         'jiggle' them (even if we don't ultimately push). */
      if (firstx != newx || firsty != newy) {

        /* affect whole row */
        for (int ix = firstx, iy = firsty;
            (firstx != newx && firsty != newy);
            travel(ix, iy, d, ix, iy)) (void)AFFECT(ix, iy);

        PUSHMOVE(jiggle, e)
          e->startx = firstx;
          e->starty = firsty;
          e->d = d;
          e->num = abs(newx - firstx) + abs(newy - firsty);
        }
      }
      #endif

      int goldx = newx, goldy = newy;

      /* remove gold block */
      (void)AFFECT(goldx, goldy);
      int replacement = (flagat(goldx, goldy) & TF_HASPANEL)?
                           realpanel(flagat(goldx, goldy)):
                           T_FLOOR;

      settile(goldx, goldy, replacement);

      int tgoldx, tgoldy;

      while (travel(goldx, goldy, d, tgoldx, tgoldy)) {

        int next = tileat(tgoldx, tgoldy);
        if (!(next == T_ELECTRIC ||
              next == T_PANEL ||
              next == T_BPANEL ||
              next == T_RPANEL ||
              next == T_GPANEL ||
              next == T_FLOOR) ||
            botat(tgoldx, tgoldy) ||
            playerat(tgoldx, tgoldy)) break;

        goldx = tgoldx;
        goldy = tgoldy;

        if (next == T_ELECTRIC) break;

    // Otherwise, going to fly through this spot.
#ifdef AM
    bool did = AFFECT(goldx, goldy);
    printf("%d %d: %s %d %d\n", goldx, goldy, did?"did":"not",
           ctx->serialat(goldx, goldy),
           ctx->Serial());
#endif
    // AFFECT(goldx, goldy);
      }

      /* goldx is dest, newx is source */
      if (goldx != newx ||
          goldy != newy) {

        int landon = tileat(goldx, goldy);
        int doswap = 0;

        /* untrigger from source */
        if (flagat(newx, newy) & TF_HASPANEL) {
          int pan = realpanel(flagat(newx,newy));
          /* any */
          /* XXX use triggers here */
          if (pan == T_PANEL ||
              /* colors */
              (target == T_GSPHERE &&
               pan == T_GPANEL) ||
              (target == T_RSPHERE &&
               pan == T_RPANEL) ||
              (target == T_BSPHERE &&
               pan == T_BPANEL))
            doswap = 1;
        }

        /* only the correct color sphere can trigger
           the colored panels */
        bool doswapt = false;
        if (Level::triggers(target, landon)) {
          doswapt = true;
        }

        /* XXX may have already affected this
           if the gold block didn't move at all */
        (void)AFFECT(goldx, goldy);
        settile(goldx, goldy, target);

        bool zapped = false;
        (void)zapped;
        if (landon == T_ELECTRIC) {
          /* gold zapped. however, if the
             electric was the target of a panel
             that we just left, the electric has
             been swapped into the o world (along
             with the gold). So swap there. */
            settile(goldx, goldy, T_ELECTRIC);

            zapped = true;
        }

        #ifdef AM
        PUSHMOVE(fly, e)
           e->what = target;
           e->whatunder = replacement;
           e->srcx = newx;
           e->srcy = newy;
           e->d = d;
           /* number of tiles traveled */
           e->distance = abs((goldx - newx) + (goldy - newy));
           e->zapped = zapped;
        }
        #endif

        if (doswapt) {
          (void)AFFECTI(destat(goldx, goldy));
          SWAPO(destat(goldx, goldy));
        }

        if (doswap) {
          (void)AFFECTI(destat(newx, newy));
          SWAPO(destat(newx, newy));
        }

        return(true);

      } else {
        /* didn't move; put it back */
        settile(newx, newy, target);

        /* XXX stuck aevent */
        return(false);
      }
    }

    case T_TRANSPORT: {
      /* not if there's an entity there */
      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      if (cap & (CAP_CANTELEPORT | CAP_ISPLAYER)) {
         (void)AFFECT(newx, newy);
         PREAFFECTENTEX(enti);
         POSTAFFECTENTEX(enti);
         WALKED(d, true);
         PREAFFECTENTEX(enti);
         POSTAFFECTENTEX(enti);
         TELEPORTOUT(enti, newx, newy);

         int targx, targy;
         where(dests[w * newy + newx], targx, targy);

         /* cache this, since stepping off might change it */
         int targ = tileat(targx, targy);

         CHECKSTEPOFF(entx, enty);
         SETENTPOS(targx, targy);

         (void)AFFECT(targx, targy);
         PREAFFECTENTEX(enti);
         POSTAFFECTENTEX(enti);
         /* teleporting always faces the player down;
            there's just one animation. */
         SETENTDIR(DIR_DOWN);
         TELEPORTIN(enti, targx, targy);

         switch (targ) {
         case T_PANEL:
           (void)AFFECTI(destat(targx, targy));
           SWAPO(destat(targx, targy));
           break;
         default:;
         }

         CHECKBOTDEATH(targx, targy, enti);

         return true;
      } else return false;
    }

    /* careful: the order in which swaps happen
       matters, because an earlier pulse could
       block beams for a later one (by raising floor).
       So we have to save up the effects and then
       perform them all at once at the end. (Panel
       swaps from remotes are last as always.)
    */
    case T_BUTTON: {
      /* XXX check caps */

      /* We want the wires to only glow if they are connected to
         something, so the function ''isconnected'' will tell us
         if a pulse will succeed in hitting something.

         After that, in each successful direction we trace out
         and animate. We also remember the effects; these all
         have to happen at the end or else it could matter what
         order we try the pulses in.
      */

      /* these are for remote swaps */
      struct SwapList {
         int target;
         SwapList *next;
         SwapList(int t, SwapList *n) : target(t), next(n) {}
      };
      SwapList *remotes = nullptr;

      /* need to delay swaps to the end. */
      int bswaps = 0, rswaps = 0, gswaps = 0;

      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      /* but always push a button pressing anim */
      (void)AFFECT(newx, newy);
      BUTTON(newx, newy, T_BUTTON);

      for (dir dd = FIRST_DIR; dd <= LAST_DIR; dd++) {
      if (
#ifndef AM
          /* if animation is off, then don't pre-scan */
          1 ||
#endif
          isconnected(newx, newy, dd)) {

      /* send a pulse in that direction. */
      int pulsex = newx, pulsey = newy;
      dir pd = dd;

      int dist = 0;

      while (pd != DIR_NONE &&
                travel(pulsex, pulsey, pd, pulsex, pulsey)) {
        int targ = tileat(pulsex, pulsey);

        /* liteup animation if a wire. note: this code
           would lite up any tile (floor, exit, etc.) except
           that those are avoided by the pre-scan above. */
        #ifdef AM
        switch (targ) {
           case T_BLIGHT:
           case T_RLIGHT:
           case T_GLIGHT:
           case T_REMOTE:
             break;
           default:
         /* Should affect here, because for example remotes
            might target some of these wires. But then subsequent
            wires are delayed because of the delay hack we use.
            So, if affecting advances the serial, reset the delay.
                XXX: The result is correct but not desirable; wires
                pause in the middle of their electricity. Probably
                should arrange it so it all goes in one shot. */
             if (AFFECT(pulsex, pulsey)) {
               dist = 0;
             }
             LITEWIRE(pulsex, pulsey, targ, pd, dist);
             break;
        }
        #endif

        dist++;

        switch (targ) {
        case T_REMOTE:
          #ifdef AM
            (void)AFFECT(pulsex, pulsey);
            PUSHMOVE(liteup, e)
              e->x = pulsex;
              e->y = pulsey;
              e->what = targ;
              e->delay = dist;
            }
          #endif
          remotes = new SwapList(destat(pulsex, pulsey), remotes);

        /* since this counts as being connected so far, we want to
           make sure that the circuit continues before animating
           it ... */
              if (
                  #ifndef AM
                  0 &&
                  #endif
                  !isconnected(pulsex, pulsey, pd)) pd = DIR_NONE;
          continue;

        case T_BLIGHT:
        case T_RLIGHT:
        case T_GLIGHT:
          #ifdef AM
          (void)AFFECT(pulsex, pulsey);
          PUSHMOVE(liteup, e)
            e->x = pulsex;
            e->y = pulsey;
            e->what = targ;
            e->delay = dist;
          }
          #endif
          if (targ == T_BLIGHT) bswaps++;
          if (targ == T_RLIGHT) rswaps++;
          if (targ == T_GLIGHT) gswaps++;
          pd = DIR_NONE;
          break;

        case T_TRANSPONDER: {
              // printf("transponder at %d/%d\n", pulsex, pulsey);
          #ifdef AM
            int transx = pulsex;
            int transy = pulsey;
          #endif
          if (!travel(pulsex, pulsey, pd, pulsex, pulsey)) {
            pd = DIR_NONE;
          } else {
        /* keep going until we hit another transponder. */
        do {
           int ta = tileat(pulsex, pulsey);
                   // printf(" ... at %d/%d: %d\n", pulsex, pulsey, ta);
           if (!allowbeam(ta) ||
               botat(pulsex, pulsey) ||
               playerat(pulsex, pulsey)) {
              /* hit something. is it a transponder? */
              if (ta == T_TRANSPONDER) {
            /* okay, then we are on the 'old' tile with
               the direction set, so we're ready to continue
               the pulse loop */
                        #ifdef AM
              LITEWIRE(pulsex, pulsey, targ, pd, dist);
              TRANSPONDERBEAM(pulsex, pulsey,
                                      transx, transy,
                      pd, dist);
            #endif
              } else {
            /* didn't hit transponder! stop. */
            pd = DIR_NONE;
              }
              break;
           } else {
              /* in preparation for beam across these squares... */
              #ifdef AM
                (void)AFFECT(pulsex, pulsey);
              #endif
           }
           /* otherwise keep going... */
        } while (travel(pulsex, pulsey, pd, pulsex, pulsey));
          }

          break;
            }

        case T_NSWE:
          /* just keep going in same direction */
          continue;

        case T_NS:
          if (pd == DIR_UP || pd == DIR_DOWN) continue;
          else pd = DIR_NONE;
          break;

        case T_WE:
          if (pd == DIR_LEFT || pd == DIR_RIGHT) continue;
          else pd = DIR_NONE;
          break;

        case T_NW:
          if (pd == DIR_DOWN) pd = DIR_LEFT;
          else if (pd == DIR_RIGHT) pd = DIR_UP;
          else pd = DIR_NONE;
          break;

        case T_SW:
          if (pd == DIR_UP) pd = DIR_LEFT;
          else if (pd == DIR_RIGHT) pd = DIR_DOWN;
          else pd = DIR_NONE;
          break;

        case T_NE:
          if (pd == DIR_DOWN) pd = DIR_RIGHT;
          else if (pd == DIR_LEFT) pd = DIR_UP;
          else pd = DIR_NONE;
          break;

        case T_SE:
          if (pd == DIR_UP) pd = DIR_RIGHT;
          else if (pd == DIR_LEFT) pd = DIR_DOWN;
          else pd = DIR_NONE;
          break;

        default: /* any non-wire stops electricity */
          pd = DIR_NONE;
          break;
            }
          }
        }

      }

      /* XXX for better results, delay according
         to the time (push times onto a stack or
         something) */
      while (bswaps--) {
        SWAPTILES(T_BUP, T_BDOWN, 0);
      }

      while (rswaps--) {
        SWAPTILES(T_RUP, T_RDOWN, 0);
      }

      while (gswaps--) {
        SWAPTILES(T_GUP, T_GDOWN, 0);
      }

      while (remotes) {
        SwapList* t = remotes;
        remotes = remotes->next;
        #ifdef AM
        { int x, y; where(t->target, x, y);
          if (0) printf("(was %d %d) ",
                        ctx->serialat(x, y),
                        ctx->Serial());
          bool did = AFFECTI(t->target);
          if (0) printf("%d=%d,%d: %s %d %d\n",
                        t->target, x, y, did?"did":"not",
                        ctx->serialat(x, y),
                        ctx->Serial());
          // AFFECTI(t->target);
        }
        #endif
        SWAPO(t->target);
        delete t;
      }

      return(true);
    }
    case T_BROKEN:

      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      (void)AFFECT(newx, newy);
      settile(newx, newy, T_FLOOR);

      #ifdef AM
      PUSHMOVE(breaks, e)
         e->x = newx;
         e->y = newy;
      }
      #endif
      return(true);

    case T_GREEN: {
      if (playerat(newx, newy) ||
          botat(newx, newy)) return false;

      int destx, desty;
      if (travel(newx, newy, d, destx, desty)) {
        if (tileat(destx, desty) == T_FLOOR &&
            !botat(destx, desty) &&
            !playerat(destx, desty)) {
          settile(destx, desty, T_BLUE);
          settile(newx, newy, T_FLOOR);

          (void)AFFECT(destx, desty);
          (void)AFFECT(newx, newy);
          PREAFFECTENTEX(enti);
          POSTAFFECTENTEX(enti);
          PUSHGREEN(newx, newy, d);
          WALKED(d, true);

          CHECKSTEPOFF(entx, enty);

          SETENTPOS(newx, newy);

          return true;
        } else return false;
      } else return false;
    }

    case T_STEEL:
    case T_RSTEEL:
    case T_GSTEEL:
    case T_BSTEEL: {

    /* three phases. first, see if we can push this
       whole column one space.

       if so, generate animations.

       then, update panel states. this is tricky. */

    int destx = newx, desty = newy;
    {
      int curx = newx, cury = newy;
      /* go until not steel, or if we hit a robot
         anywhere along this, end */
      while (!botat(curx, cury) && !playerat(curx, cury) &&
             travel(curx, cury, d, destx, desty) &&
             issteel(tileat(destx, desty))) {
         curx = destx;
         cury = desty;
      }
    }

    /* entity in our column or at the end? sorry */
    if (botat(destx, desty) ||
        playerat(destx, desty)) return false;

    /* what did we hit? */
    int hittile;
    bool zap = false;
    (void)zap;
    switch (hittile = tileat(destx, desty)) {
    /* nb if we "hit" steel, then it's steel to the edge of the
       level, so no push. */
    case T_PANEL:
    case T_GPANEL:
    case T_BPANEL:
    case T_RPANEL:
    case T_FLOOR:
       break;
    case T_ELECTRIC:
       zap = true;
       break;
    default: return(false);
    }

    /*  guy            destx,desty
        v              v
       [ ][S][S][S][S][ ]
           ^
           steels
           starting at newx,newy

        d  ---->
    */
    dir revd = dir_reverse(d);


    /* at this point, the push is going through */
    #ifdef AM
    /* make pass to affect (so that all move in same serial) */
    { int xx = destx, yy = desty;
      do {
        travel(xx, yy, revd, xx, yy);
        (void)AFFECT(xx, yy);
      } while (! (xx == newx && yy == newy));
      (void)AFFECT(newx, newy);
    }

    { int xx = destx, yy = desty;
      bool zappy = zap;
      do {
        travel(xx, yy, revd, xx, yy);
        int replacement = (flagat(xx, yy) & TF_HASPANEL)?
                           realpanel(flagat(xx,yy)):T_FLOOR;
        int what = tileat(xx, yy);
        PUSHED(d, what, xx, yy, replacement, zappy, false);

        /* only last one can zap */
        zappy = false;
      } while (! (xx == newx && yy == newy));
    }
    #endif

    /* move the steel blocks first. */
    { int movex = destx, movey = desty;
      while (! (movex == newx && movey == newy)) {
        int nextx, nexty;
        travel(movex, movey, revd, nextx, nexty);
        settile(movex, movey, tileat(nextx, nexty));
        movex = nextx;
        movey = nexty;
      }
    }

    /* and one more, for the tile that we're stepping onto */
    {
      int replacement = (flagat(newx, newy) & TF_HASPANEL)?
                         realpanel(flagat(newx,newy)):T_FLOOR;
      settile(newx, newy, replacement);
    }

    /* reconcile panels.

       imagine pushing a row of blocks one space to the
       right.

       we loop over the NEW positions for the steel blocks.
       If a steel block is on a panel (that it can trigger),
       then we trigger that panel as long as the thing to
       its right (which used to be there) couldn't trigger
       it. this handles new panels that are turned ON.

       if we can't trigger the panel, then we check to see
       if the panel to our right (which used to be there)
       also can't trigger it. If so, we don't do anything.
       Otherwise, we "untrigger" the panel.

       To simplify, if triggerstatus_now != triggerstatus_old,
       we trigger. (Trigger has the same effect as untriggering.)

       Because these swaps are supposed to be delayed, we
       set the TF_TEMP flag if the tile should do a swap
       afterwards.
    */

    bool swapnew = false;
    { int lookx = destx, looky = desty;
      int prevt = T_FLOOR; /* anything that doesn't trigger */
      while (! (lookx == newx && looky == newy)) {

        int heret = tileat(lookx, looky);

        /* triggerstatus for this location (lookx, looky) */
        bool triggerstatus_now =
             (flagat(lookx, looky) & TF_HASPANEL) &&
             triggers(heret, realpanel(flagat(lookx, looky)));

        bool triggerstatus_old =
             (flagat(lookx, looky) & TF_HASPANEL) &&
             issteel(prevt) &&
             triggers(prevt, realpanel(flagat(lookx, looky)));

        if (triggerstatus_now != triggerstatus_old) {
           setflag(lookx, looky, flagat(lookx, looky) | TF_TEMP);
           //           printf("Yes swap at %d/%d\n", lookx, looky);
        } else setflag(lookx, looky, flagat(lookx, looky) & ~TF_TEMP);

        prevt = heret;

        int nextx, nexty;
        travel(lookx, looky, revd, nextx, nexty);

        lookx = nextx;
        looky = nexty;
      }

      /* first panel is slightly different */
      { int first = tileat(newx, newy);
        bool trig_now = (first == T_PANEL);
        bool trig_old =
           ispanel(first) &&
           triggers(prevt, realpanel(flagat(newx, newy)));

        if (trig_old != trig_now) {
          swapnew = true;
        }
      }
    }


    /* zap, if necessary, before swapping */
    if (zap) {
      settile(destx, desty, T_ELECTRIC);
      /* XXX animate */
    }

    /* now we can start swapping. */
    CHECKSTEPOFF(entx, enty);

    /* this part is now invariant to order, because there is
       only one destination per location */

    if (swapnew) {
      (void)AFFECTI(destat(newx, newy));
      SWAPO(destat(newx, newy));
    }

    { int lookx = destx, looky = desty;
      while (! (lookx == newx && looky == newy)) {

        if (flagat(lookx, looky) & TF_TEMP) {
          (void)AFFECTI(destat(lookx, looky));
          SWAPO(destat(lookx, looky));
          setflag(lookx, looky, flagat(lookx, looky) & ~TF_TEMP);
        }

        /* next */
        int nextx, nexty;
        travel(lookx, looky, revd, nextx, nexty);
        lookx = nextx;
        looky = nexty;
      }
    }

    /* XXX also boundary conditions? (XXX what does that mean?) */
    PREAFFECTENTEX(enti);
    POSTAFFECTENTEX(enti);
    WALKED(d, true);

    SETENTPOS(newx, newy);

    return(true);
    break;
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

    case T_HEARTFRAMER:
    /* only the player can pick up
       heart framers */
    if (botat(newx, newy) ||
        playerat(newx, newy)) return false;
    if (cap & CAP_HEARTFRAMERS) {
      (void)AFFECT(newx, newy);
      PREAFFECTENTEX(enti);
      POSTAFFECTENTEX(enti);
      WALKED(d, true);

      /* snag heart framer */
      settile(newx, newy, T_FLOOR);
      GETHEARTFRAMER(newx, newy);

      /* any heart framers left? */

      if (!hasframers()) {
        for (int y = 0; y < h; y++) {
          for (int x = 0; x < w; x++) {
           int t = tileat(x, y);
           if (t == T_SLEEPINGDOOR) {
             (void)AFFECT(x, y);
             /* better anim, like exclamation mark */
             // TOGGLE(x, y, T_SLEEPINGDOOR, T_EXIT);
             WAKEUPDOOR(x, y);
             settile(x, y, T_EXIT);
           }
          }
        }

        /* also bots */
        for (int i = 0; i < nbots; i++) {
          switch (bott[i]) {
          case B_DALEK_ASLEEP:
            PREAFFECTENTEX(i);
            (void)AFFECTI(boti[i]);
            POSTAFFECTENTEX(i);

            bott[i] = B_DALEK;
            STANDBOT(i);

            { int xx, yy;
              where(boti[i], xx, yy);
              WAKEUP(xx, yy);
            }
            break;
          case B_HUGBOT_ASLEEP:
            PREAFFECTENTEX(i);
            (void)AFFECTI(boti[i]);
            POSTAFFECTENTEX(i);

            bott[i] = B_HUGBOT;
            STANDBOT(i);

            { int xx, yy;
              where(boti[i], xx, yy);
              WAKEUP(xx, yy);
            }
            break;
            default: ;
            /* nothing */
            break;
          }
        }
      }

      /* panel actions are last */
      CHECKSTEPOFF(entx, enty);

      SETENTPOS(newx, newy);

      return true;
    } else return false;
    break;

    case T_ELECTRIC:
    if (botat(newx, newy) ||
        playerat(newx, newy)) return false;
    /* some bots are stupid enough
       to zap themselves */
      if (enti != -1 &&
          (cap & CAP_ZAPSELF)) {

        (void)AFFECT(newx, newy);
        PREAFFECTENTEX(enti);
        POSTAFFECTENTEX(enti);
        WALKED(d, false);

        /* change where it is */
        boti[enti] = index(newx, newy);

        /* then kill it */
        bott[enti] = B_DELETED;
        PREAFFECTENTEX(enti);
        POSTAFFECTENTEX(enti);
        BOTEXPLODE(enti);

        /* might have stepped off a panel/trap */
        CHECKSTEPOFF(entx, enty);

        return true;
      } else return false;
    break;

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
      return(false);

    }
  } else return(false);
}

#ifdef PUSHMOVE
# undef PUSHMOVE
#endif

#ifdef AM
# undef AM
#endif
