#include "edit.h"

#include <math.h>

#include "SDL.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/union-find.h"

#include "level.h"
#include "draw.h"
#include "escapex.h"
#include "play.h"
#include "util.h"
#include "message.h"
#include "analysis.h"
#include "generator.h"
#include "pattern.h"

/* matches anything */
static bool pred_any(Level *, void *_, int x, int y) {
  return true;
}

static bool pred_gold_or_sphere(Level *lev, void *_, int x, int y) {
  const int t = lev->tileat(x, y);
  return t == T_GOLD || Level::issphere(t);
}

static bool pred_stops_gold(Level *lev, void *_, int x, int y) {
  switch (lev->tileat(x, y)) {
  case T_FLOOR:
  case T_PANEL:
  case T_RPANEL:
  case T_BPANEL:
  case T_GPANEL:
  case T_ELECTRIC:
    return false;
  default:
    return true;
  }
}

void Editor::DoRandom() {
  if (changed) {
    if (!Message::Quick(this,
                        "Random will destroy your unsaved changes.",
                        "Do Random",
                        "Don't do Random")) {
      Redraw();
      return;
    }
  }

  switch (randtype) {

  case RT_MAZE:
  case RT_MAZE2: {

    if (level->w < 3 ||
        level->h < 3) {
      dr.message = RED "Sorry" POP ", the level is too small to make a maze.";
      Redraw();
      return;
    }

    FullClear(T_BLUE);

    /* start with seed */
    level->settile(1 + util::random() % (level->w - 2),
                    1 + util::random() % (level->h - 2),
                    T_EXIT);

    /* look for this

    B.B
    B!B
    .F.

    .. and consider setting ! to
    floor. in the following, x,y
    are the F above.
    */

    int hadchange = 0;
    do {

      hadchange = 0;

      int innerw = level->w - 2;
      int innerh = level->h - 2;

      /* try to remove bias towards top left
         by starting in a random spot. */
      int randx = abs(util::random());
      int randy = abs(util::random());

      for (int iy = 0; iy < innerh; iy++) {
        for (int ix = 0; ix < innerw; ix++) {

          int y = 1 + ((iy + randy) % innerh);
          int x = 1 + ((ix + randx) % innerw);

          if (level->tileat(x, y) != T_BLUE) {
            /* try all dirs, but start with a
               random one. */

            int tries = 4;
            int sd = 1 + (util::random() & 3);

            while (tries--) {
              int tx, ty; /* targets */
              int nx, ny, /* temp */
                  bx, by; /* one beyond target */

              if (level->travel(x, y, sd, tx, ty) &&
                  /* must be blue, otherwise this won't be a change */
                  level->tileat(tx, ty) == T_BLUE &&
                  /* to its left must be blue. */
                  level->travel(tx, ty, turnleft(sd), nx, ny) &&
                  level->tileat(nx, ny) == T_BLUE &&
                  /* ... and right */
                  level->travel(tx, ty, turnright(sd), nx, ny) &&
                  level->tileat(nx, ny) == T_BLUE &&
                  /* and beyond it ... */
                  level->travel(tx, ty, sd, bx, by) &&
                  /* must have blue to its left */
                  level->travel(bx, by, turnleft(sd), nx, ny) &&
                  level->tileat(nx, ny) == T_BLUE &&
                  /* ... and right */
                  level->travel(bx, by, turnright(sd), nx, ny) &&
                  level->tileat(nx, ny) == T_BLUE &&
                  /* if maze, we can't be connecting with
                     another corridor. */
                  (randtype == RT_MAZE2 ||
                   (level->tileat(bx, by) == T_BLUE))) {

                hadchange = 1;
                level->settile(tx, ty, T_FLOOR);
                break; /* from try loop */
              }

              sd = 1 + (sd & 3);
            }
            /* not safe in any direction. */

          } /* if we found seed */
        } /* x */
      } /* y */
    } while (hadchange);


    break;
  }

  case RT_CORRIDORS: {

    FullClear(T_BLUE);

    /*
       set to floor only if in this
       configuration:

       B.B
       .?.
       B.B

    */
    int hadchange = 0;
    do {

      hadchange = 0;

      for (int y = 0; y < level->h - 1; y++) {
        for (int x = 0; x < level->w - 1; x++) {

          /* pick a number 0..3 */
          int which = util::random() & 3;

          /* try each corner, selecting the
             'which'th one that succeeds */

          /* consider setting xx,yy if no diagonal floors. */
#             define TRY(xx, yy) \
                if (level->tileat( xx,       yy     ) == T_BLUE && \
                    level->tileat((xx) - 1, (yy) - 1) == T_BLUE && \
                    level->tileat((xx) + 1, (yy) - 1) == T_BLUE && \
                    level->tileat((xx) + 1, (yy) + 1) == T_BLUE && \
                    level->tileat((xx) - 1, (yy) + 1) == T_BLUE) { \
                   if (which) which--; \
                   else { \
                      level->settile(xx, yy, T_FLOOR); \
                      hadchange = 1; \
                      break; \
                   } \
                }

          /* try each of the cases */
          if (x > 0 &&
              y > 0) {
            TRY(x, y);
          }

          if (x > 0 &&
              y < (level->h - 2)) {
            TRY(x, y + 1);
          }

          if (x < (level->w - 2) &&
              y > 0) {
            TRY(x + 1, y);
          }

          if (x < (level->w - 2) &&
              y < (level->h - 2)) {
            TRY(x + 1, y + 1);
          }
#             undef TRY

        }
      }

    } while (hadchange);

    break;
  }

    /* XXX once this didn't halt for me: could be bug below */
  case RT_MAZEBUG1: {

    FullClear(T_BLUE);

    /* XXX disable because of bugs */
#   if 0

    /* look for anything shaped like this:

    BB
    BB

    choose one corner randomly to become
    floor, but only if that corner is
    itself corner to just blue, like this:

    B.B
    B?.
    BBB

    */
    int hadchange = 0;
    do {

      hadchange = 0;

      for (int y = 0; y < level->h - 1; y++) {
        for (int x = 0; x < level->w - 1; x++) {
          /* XXX: valgrind reports invalid read here */
          if (level->tileat(x, y) == T_BLUE &&
              level->tileat(x + 1, y) == T_BLUE &&
              level->tileat(x + y, y + 1) == T_BLUE &&
              level->tileat(x, y + 1) == T_BLUE) {

            /* found 2x2 blue square. */

            /* pick a number 0..3 */
            int which = util::random() & 3;

            /* try each corner, selecting the
               'which'th one that succeeds */


            /* consider setting xx,yy if no diagonal floors. */
#             define TRY(xx, yy) \
                if (level->tileat((xx) - 1, (yy) - 1) == T_BLUE && \
                    level->tileat((xx) + 1, (yy) - 1) == T_BLUE && \
                    level->tileat((xx) + 1, (yy) + 1) == T_BLUE && \
                    level->tileat((xx) - 1, (yy) + 1) == T_BLUE) { \
                   if (which) which--; \
                   else { \
                      level->settile(xx, yy, T_FLOOR); \
                      hadchange = 1; \
                      break; \
                   } \
                }

            /* try each of the cases */
            if (x > 0 &&
                y > 0) {
              TRY(x, y);
            }

            if (x > 0 &&
                y < (level->h - 2)) {
              TRY(x, y + 1);
            }

            if (x < (level->w - 2) &&
                y > 0) {
              TRY(x + 1, y);
            }

            if (x < (level->w - 2) &&
                y < (level->h - 2)) {
              TRY(x + 1, y + 1);
            }
#             undef TRY

          }
        }
      }

    } while (hadchange);

#   endif
    break;
  }

    /* broken version of above */
  case RT_MAZEBUG2: {

    FullClear(T_BLUE);

    /* look for anything shaped like this:

    BB
    BB

    choose one corner randomly to become
    floor, but only if that corner is
    itself corner to blue, like this:

    ..B
    B?.
    BB.

    */
    int hadchange = 0;
    do {

      hadchange = 0;

      for (int y = 0; y < (level->h - 1); y++) {
        for (int x = 0; x < (level->w - 1); x++) {
          if (level->tileat(x, y) == T_BLUE &&
              level->tileat(x + 1, y) == T_BLUE &&
              level->tileat(x + y, y + 1) == T_BLUE &&
              level->tileat(x, y + 1) == T_BLUE) {

            /* found 2x2 blue square. */

            /* pick a number 0..3 */
            int which = util::random() & 3;

            /* try each corner, selecting the
               'which'th one that succeeds */

            int looking = 0;
            do {

              /* XXX if looking=1 ... nonterm?? why? */

              /* consider setting rx ry if xx xy checks out */
#             define TRY(rx, ry, xx, yy) \
                if (level->tileat(xx, yy) == T_BLUE) { \
                   looking = 0; \
                   if (which) which--; \
                   else { \
                      level->settile(rx, ry, T_FLOOR); \
                      hadchange = 1; \
                      break; \
                   } \
                }

              /* try each of the cases */
              if (x > 0 &&
                  y > 0) {
                TRY(x, y, x - 1, y - 1);
              }

              if (x > 0 &&
                  y < (level->h - 2)) {
                TRY(x, y + 1, x - 1, y + 2);
              }

              if (x < (level->w - 2) &&
                  y > 0) {
                TRY(x + 1, y, x + 2, y - 1);
              }

              if (x < (level->w - 2) &&
                  y < (level->h - 2)) {
                TRY(x + 1, y + 1, x + 2, y + 2);
              }
#             undef TRY
            } while (looking);
          }
        }
      }

    } while (hadchange);

    break;
  }

  case RT_ROOMS: {
    /* XXX use 'clear' flag to decide whether
       to do this or not */
    FullClear(T_FLOOR);

    int nl = 12;

    while (nl--) {
      /* pick random x, y, */
      int x = util::random() % level->w;
      int y = util::random() % level->h;

      /* XXX depends on order of dir enum */
      int d = 1 + (util::random() & 3);

      /* draw until we hit something. */
      level->settile(x, y, current);
      while (level->travel(x, y, d, x, y)) {
        if (level->tileat(x, y) != current)
          level->settile(x, y, current);
        else break;
      }
    }

    break;
  }

  case RT_CRAZY: {
    /* XXX testing */
    std::unique_ptr<Pattern<void>> pat_test =
      Pattern<void>::Create("...\n"
			    "B\\0BB\n"
			    "...\n");
    if (pat_test.get() != nullptr) {
      pat_test->settile('G', T_GREY);
      pat_test->settile('B', T_BLUE);
      pat_test->setpredicate('.', pred_any);

      Match::stream *ms_test =
        pat_test->findall(level.get(), 0);

      Match *m;
      while ((m = ms_test->next())) {
        int x = 0, y = 0;
        m->getindex(0, x, y);
        level->settile(x, y, T_FLOOR);
        delete m;
      }
    }

    dr.message = GREEN "crazy!!!!";
    break;
  }

  case RT_RETRACT1: {
    /* reverse1 never 'clears' */
    retract1();
    break;
  }

  case RT_RETRACTGOLD: {
    if (!retract_gold()) {
      dr.message = RED "Unable to retract.";
    }
    break;
  }

  default:
    dr.message = "randtype not implemented";
  }

  /* right after a random, treat the level as 'unchanged'
     so that we can 'random' again without a prompt. */
  changed = 0;
  Redraw();
}

/* go one macro-move 'into the past' */

/* this includes:

   - if we can manoeuvre ourselves into a row or
     column with a gold block (which is against
     something), then we can 'suck' that block
     into us. If that gold block is on a switch,
     even better!

   - if we can move next to a movable block, like
     this:

        [floor][player][block]

     .. then we can transition to:

        [player][block][floor]

     if the block is originally attacked by a
     laser, even better. if as a result of the
     retraction, it is NOT attacked, that's
     better still.

   - if we see any 1-width corridor (defined:
     a single space that separates two connected
     areas of the graph), and we are in one
     of those two areas, then put an "obstacle"
     disconnecting them:

      - a hole in the bottleneck, with a grey
        block right next to it, and the player
        immediately ready to push that block
        in the hole

      - put a panel under a block
        targeting a (stop sign) at the
        bottleneck

      -

   - if there is electricity,
     'pull' a block from it, or
     'suck' a gold block.

   - low priority local moves:
       - press a 0/1 toggle
       - "come from" any transporter
         in a disconnected area
         that targets this one.

   we prune out any move that leaves the
   player dead.

*/

/* we can retract any gold-like block (including spheres)
   that's against a gold-stopping tile by 'sucking' it
   into a space from which we can kick it.
*/
bool Editor::retract_gold() {
  /* XX could also be at edge of map -- might want a way to
     specify that pattern */
  std::unique_ptr<Pattern<void>> bgold = Pattern<void>::Create("\\0 \\1GS\n");

  if (bgold.get() != nullptr) {

    /* XX could include panels */
    bgold->settile(' ', T_FLOOR);
    bgold->setpredicate('G', pred_gold_or_sphere);
    bgold->setpredicate('S', pred_stops_gold);

    std::unique_ptr<UnionFind> reach = Analysis::Reachable(level.get());

    std::unique_ptr<Match::stream> matches{
      bgold->findall(level.get(), 0)};

    // XXX upgrading extent; this can be done a better way
    while (Match *mtmp = matches->next()) {
      std::unique_ptr<Match> m{mtmp};

      int x = 0, y = 0;
      m->getindex(1, x, y);

      bool sph = false;
      int oldtile = level->tileat(x, y);
      if (Level::issphere(oldtile)) sph = true;

      /* now try going left as far as possible
         from the gold block */

      const int left = m->left();

      /*
      printf("match at %d/%d, left: %s\n",
              x, y, dirstring(left).c_str());
      */

      /* place to put the gold block. man must be to its
         left. */
      int targx = 0, targy = 0;
      m->getindex(0, targx, targy);

      /* these are the best targets so far */
      int bestx = -1, besty = -1;
      int bgx = -1, bgy = -1;
      bool best_has_ricochet = false;

      int gx, gy;
      while (level->travel(targx, targy, left, gx, gy)) {
        /* as long as there's even a place for the guy to
           stand... */
        /* printf("try targ %d/%d\n", targx, targy); */

        /* we're done if target is non-floor */
        /* XX could include panels */
        if (level->tileat(targx, targy) != T_FLOOR) break;

        /* this is a candidate if we can place the guy in
           a 'pushing position' that he can reach. this
           is in gx,gy if !sph, otherwise there may be 0
           or more spheres in between */

        if (sph) {
          /* move guy left until first non-sphere space */
          while (Level::issphere(level->tileat(gx, gy))) {
            if (!level->travel(gx, gy, left, gx, gy)) goto no_more_motion;
          }
        }

        /* the space has to be reachable by the guy. but we
           can still potentially continue if this space is
           not occupiable. */
        if (reach->Find(level->index(level->guyx, level->guyy)) ==
            reach->Find(level->index(gx, gy))) {

          /*
          printf("candidate! guy: %d/%d gold: %d/%d\n",
                 gx, gy, targx, targy);
          */

          /* is this a ricochet? (that means we can expect
             to repeat this process for this new location) */
          int ux, uy;
          int dx, dy;
          if (level->travel(targx, targy, m->up(), ux, uy) &&
              level->travel(targx, targy, m->down(), dx, dy) &&
              ((pred_stops_gold(level.get(), 0, ux, uy) &&
                level->tileat(dx, dy) == T_FLOOR) ||
               (pred_stops_gold(level.get(), 0, dx, dy) &&
                level->tileat(ux, uy) == T_FLOOR))) {

            /* then this new place is definitely better */
            /* printf("  ... ricocheting!\n"); */
            best_has_ricochet = true;

            bgx = gx;
            bgy = gy;
            bestx = targx;
            besty = targy;

            /* small chance of exiting now */
            if (! (util::random() & 7)) goto no_more_motion;

          } else {
            /* only beats previous non-ricochet */
            if (!best_has_ricochet) {
              bgx = gx;
              bgy = gy;
              bestx = targx;
              besty = targy;
              /* small chance of exiting now */
              if (! (util::random() & 7)) goto no_more_motion;
            }
          }
        }
        /* now try next spot ... */
        if (!level->travel(targx, targy, left, targx, targy)) break;
      }

    no_more_motion:;

      /* printf("looked at all candidates\n"); */

      if (bestx >= 0) {
        /* ... */
        level->settile(x, y, T_FLOOR);
        level->settile(bestx, besty, oldtile);
        level->guyx = bgx;
        level->guyy = bgy;
        return true;

      } else {
        /* printf("no motion possible.\n"); */

      }
    }
    /* no match */
    return false;
  } else return false;
}

/* moves any grey style block.
   we find a block with an empty space
   next to it that we can reach. we then
   randomly walk it somewhere that we can
   reach.
*/
bool Editor::retract_grey() {

  return false;
}

/*
   If we have the following:

   ..S

   where S is a separator for the current player location,
   and both .s end up in the separated region, then we
   can make this:

   [player] [grey] [hole]

 */
bool Editor::retract_hole() {
  /* FIXME Crash on empty level (pattern code) */

  /* XXX it should be possible to create test functions that depend on
     the registers. (before running test functions we set all regs,
     and pass that to test function each time.) Then we can write this
     pattern directly */
  /* this is pretty slow. but it seems hard to test for \2 being
     a separator, since we can't know what it separates (maybe
     it is cheaper anyway, since this will have a LOT of matches?)

     XXX we should at least make sure that each of these are reachable
     from the current player's position; otherwise they definitely
     won't be separators
  */
  std::unique_ptr<Pattern<void>> findsep =
    Pattern<void>::Create("\\0 \\1 \\2 \n");

  /* XXX only need 'empty' for first space */
  if (findsep.get() != nullptr) {
    findsep->settile(' ', T_FLOOR);

    Match::stream *matches =
      findsep->findall(level.get(), 0);

    Match *mtmp;
    while ((mtmp = matches->next())) {
      std::unique_ptr<Match> m{mtmp};

      int x = 0, y = 0;
      m->getindex(2, x, y);
      int x0 = 0, y0 = 0;
      m->getindex(0, x0, y0);
      int x1 = 0, y1 = 0;
      m->getindex(1, x1, y1);

      /*
        printf("match! r2 at %d/%d up: %s right: %s\n", x, y,
        dirstring(m->up()).c_str(), dirstring(m->right()).c_str());
      */

      if (Analysis::DoesSep(level.get(), x, y,
                            level->guyx,
                            level->guyy, x0, y0, T_HOLE) &&
          Analysis::DoesSep(level.get(), x, y,
                            level->guyx,
                            level->guyy, x1, y1, T_HOLE)) {

        /*
        printf("fullmatch! at %d/%d up: %s right: %s\n", x, y,
               dirstring(m->up()).c_str(), dirstring(m->right()).c_str());
        */

        level->settile(x, y, T_HOLE);
        level->settile(x1, y1, T_GREY);

        /* maybe check we're not dead here? I'm not sure this is
           correct in every case. */
        level->guyx = x0;
        level->guyy = y0;

        /* FIXME cleanup */
        return true;
      }
    }
  }
  return false;

}


void Editor::retract1() {

  Message::No(this, "disabled--retract1 is buggy!");
  return;

  /* perhaps rank these by priority; right now we just choose
     randomly. */

  /* choose a random path through our retraction operations */
  for (Generator g{2}; g.anyleft(); g.next()) {
    switch (g.item()) {
    default:
    case 0: if (retract_gold()) goto done; else break;
    case 1: if (retract_hole()) goto done; else break;
    }
  }

  dr.message = RED "no retraction applies";
  return;
 done:

  dr.message = "retracted!";
  return;

#if 0
  /* build a graph of the level as-is.

  the graph is a set of equivalence classes
  (representing direct reachability via walking
  without dying) */

  std::unique_ptr<UnionFind> rr = Analysis::Reachable(level.get());

# if 0
  {
    for (int y = 0; y < level->h; y++) {
      for (int x = 0; x < level->w; x++) {
        printf("%4d ", rr->Find(level->index(x, y)));
      }
      printf("\n");
    }
  }
# endif

  /* dumb first attempt */
  /* int here = rr->find(level->index(x, y)); */

  /* for any spot in 'here' that is
     a separator, separate! */

  for (Generator g{level->w * level->h};
       g.anyleft();
       g.next()) {
    int xx, yy;
    level->where(g.item(), xx, yy);

    int ox, oy;
    if (Analysis::issep(level.get(), xx, yy,
                        level->guyx,
                        level->guyy, ox, oy)) {

      /*
        printf("separates xx,yy = %d/%d, ox,oy %d/%d\n",
        xx, yy, ox, oy);
      */

      level->settile(xx, yy, T_STOP);
      level->guyx = ox;
      level->guyy = oy;
      return;
    }
  }


  /* XXX testing */
  Pattern<void> *pat_test = Pattern<void>::create("...\n"
                                                   ".GB\n"
                                                   "...\n");
  if (pat_test) {
    pat_test->settile('G', T_GREY);
    pat_test->settile('B', T_BLUE);
    pat_test->setpredicate('.', pred_any);

    Match::stream *ms_test =
      pat_test->findall(level.get(), 0);

    Match *m;
    while ((m = ms_test->next())) {
      int x, y;
      level->where(m->top_left(), x, y);
      /*
      printf("match! at %d/%d up: %s right: %s\n", x, y,
             dirstring(m->up()).c_str(), dirstring(m->right()).c_str());
      */

      m->destroy();
    }
  }
#endif
}

string Editor::ainame(int i) {
  switch (i) {
  case RT_MAZE: return YELLOW "Maze" POP BLUE ": Creates a maze using the foreground tile." POP;
  case RT_MAZE2: return YELLOW "Maze 2" POP BLUE ": Like maze, but passages may cross." POP;
  case RT_CORRIDORS: return YELLOW "Corridors" POP BLUE ": Makes unconnected width-1 corridors." POP;
  case RT_MAZEBUG1: return RED "Maze bug" POP BLUE ": Disabled." POP;
  case RT_MAZEBUG2: return RED "Maze bug 2" POP BLUE ": Disabled." POP;
  case RT_ROOMS: return YELLOW "Rooms" POP BLUE ": Creates \"rooms\" with the foreground tile." POP;
  case RT_CRAZY: return RED "Crazy" POP BLUE ": For testing." POP;
  case RT_RETRACT1: return YELLOW "Retract 1" POP BLUE ": Disabled." POP; /* Retracts one move in a variety of ways." POP; */
  case RT_RETRACTGOLD:
    return YELLOW "Retract Gold" POP BLUE ": Retracts motion of a gold block (or sphere)." POP;
  default: return (string)RED "No Description for Randtype #" + itos(i);
  }
}
