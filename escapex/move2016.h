
// New move header. Include just once!
#ifndef __MOVE2016_H
#define __MOVE2016_H

#include "level-base.h"
#include "aevent.h"
#include "ptrlist.h"

// Notes for templatizing this thing, 2016:
// Two goals overall:
//   - efficient version of move() that doesn't pay for animation
//   - ability to compile level without dependency on SDL.
//
// ...
// Before I started on 27 Aug 2016, tom7 had 1282/2509 Triage levels solved,
// (but then I solved a few and it's 1284)
// and 485/803 minor leagues, plus obviously 34/34 regressions and 22/22
// official levels.

using AList = PtrList<aevent>;

// TODO: Make a lexical distinction between the macros that do something
// that affects the level, and those that are purely animation.

// TODO: instead of comparing enti against -1, use B_PLAYER.

// TODO: Migrate these macros to functions and templates as much as
// possible. This approach is better than the old move.h, but it still
// sucks to have to know what variables are in scope and avoid certain
// symbols inside the lambdas...

// TODO: No need to wrap *calls* to these macros in if (ANIMATING) ...

#define AFFECT2016(x, y) do {                   \
    (void)ctx->affect((x), (y), this, etail);   \
  } while (0)

#define AFFECTI2016(i) do {                     \
    (void)ctx->affecti((i), this, etail);       \
  } while (0)

// This new version of the macro looks more like a function call.
// It requires the following to be in scope:
//     etail   (XXX docs)
//     ctx, which is a Disamb* or NullDisamb*.
//     ANIMATING (a compile-time bool, indicating whether animation is
//                turned on).
// It should be used like this:
//
// PUSHMOVE2016(bombsplosion, ([&](bomsplosion_t *e) {
//   e->x = x;
//   e->y = y;
// }));
//
// Note the need for parentheses around the lambda argument if it
// contains a comma outside of parentheses. It also seems to fail to
// parse (or somehow parses wrong?) if there are ternary operators
// inside the lambda. (??)
//
// No code is executed (lambda is dead) if ANIMATING is false. Scope
// of fn should be as expected and the macro should behave like a
// normal statement.
#define PUSHMOVE2016(type, fn) do {             \
    if (ANIMATING) {                            \
      (fn)([&]() -> type ## _t * {              \
        aevent *a = new aevent;                 \
        *etail = new AList(a, nullptr);         \
        etail = &((*etail)->next);              \
        a->serial = ctx->Serial();              \
        a->t = tag_ ## type;                    \
        return &(a->u. type );                  \
      }());                                     \
    };                                          \
  } while (0)

// Expected in scope: ctx, ANIMATING
// fn is a lambda that takes no args. It is executed whether animation
// on or not!
#define AFFECTENT2016(entid, fn) do {                                   \
    const int affectent_ei = (entid);                                   \
    if (ANIMATING) {                                                    \
      if (affectent_ei == -1) ctx->preaffectplayer(this, etail);        \
      else ctx->preaffectbot(affectent_ei, this, etail);                \
    }                                                                   \
    (fn)();                                                             \
    if (ANIMATING) {                                                    \
      if (affectent_ei == -1) ctx->postaffectplayer();                  \
      else ctx->postaffectbot(affectent_ei);                            \
    }                                                                   \
  } while (0)

#define BOTEXPLODE2016(botidx) do {                   \
    if (ANIMATING) {                                  \
      const int botexplode_botidx = (botidx);         \
      PUSHMOVE2016(botexplode, [&](botexplode_t *e) { \
        where(boti[botexplode_botidx], e->x, e->y);   \
      });                                             \
    }                                                 \
  } while (0)

#define WALKEDEX2016(d, ex, ey, ei, push) do {                          \
    if (ANIMATING) {                                                    \
      const dir walked_d = (d);                                         \
      const int walked_ex = (ex), walked_ey = (ey);                     \
      const int walked_ei = (ei);                                       \
      const bool walked_push = (push);                                  \
      const int walked_under = tileat(walked_ex, walked_ey);            \
      const bot walked_entt =                                           \
        (walked_ei == -1) ? B_PLAYER : bott[walked_ei];                 \
      const int walked_data =                                           \
        (walked_ei == -1) ? 0 : bota[walked_ei];                        \
      PUSHMOVE2016(walk, ([&](walk_t *e) {                              \
        e->srcx = walked_ex;                                            \
        e->srcy = walked_ey;                                            \
        e->d = walked_d;                                                \
        e->pushing = walked_push;                                       \
        e->whatunder = walked_under;                                    \
        e->entt = walked_entt;                                          \
        e->data = walked_data;                                          \
      }));                                                              \
    }                                                                   \
  } while (0)

#define WALKED2016(d, push) WALKEDEX2016(d, entx, enty, enti, push)

#define TELEPORTOUT2016(ei, xx, yy) do {                                \
    if (ANIMATING) {                                                    \
      const int teleportout_ei = (ei);                                  \
      const int teleportout_x = (xx);                                   \
      const int teleportout_y = (yy);                                   \
      const bot teleportout_entt =                                      \
        (teleportout_ei == -1) ? B_PLAYER : bott[teleportout_ei];       \
      PUSHMOVE2016(teleportout, ([&](teleportout_t *e) {                \
        e->x = teleportout_x;                                           \
        e->y = teleportout_y;                                           \
        e->entt = teleportout_entt;                                     \
      }));                                                              \
    }                                                                   \
  } while (0)

#define TELEPORTIN2016(ei, xx, yy) do {                                 \
    if (ANIMATING) {                                                    \
      const int teleportin_ei = (ei);                                   \
      const int teleportin_x = (xx);                                    \
      const int teleportin_y = (yy);                                    \
      const bot teleportout_entt =                                      \
        (teleportin_ei == -1) ? B_PLAYER : bott[teleportin_ei];         \
      PUSHMOVE2016(teleportin, [&](teleportin_t *e) {                   \
        e->x = teleportin_x;                                            \
        e->y = teleportin_y;                                            \
        e->entt = teleportout_entt;                                     \
      });                                                               \
    }                                                                   \
  } while (0)

#define TRAP2016(xx, yy, tt) do {               \
    if (ANIMATING) {                            \
      const int trap_xx = (xx), trap_yy = (yy); \
      const int trap_tt = (tt);                 \
      PUSHMOVE2016(trap, ([&](trap_t *e) {      \
        e->x = trap_xx;                         \
        e->y = trap_yy;                         \
        e->whatold = trap_tt;                   \
      }));                                      \
    }                                           \
  } while (0)

#define TOGGLE2016(xx, yy, t, d) do {                   \
    if (ANIMATING) {                                    \
      const int toggle_x = (xx), toggle_y = (yy);       \
      const int toggle_wh = (t);                        \
      const int toggle_delay = (d);                     \
      PUSHMOVE2016(toggle, ([&](toggle_t *e) {          \
        e->x = toggle_x;                                \
        e->y = toggle_y;                                \
        e->whatold = toggle_wh;                         \
        e->delay = toggle_delay;                        \
      }));                                              \
    }                                                   \
  } while (0)

#define BUTTON2016(xx, yy, t) do {                \
    if (ANIMATING) {                              \
      const int button_x = (xx), button_y = (yy); \
      const int button_wh = (t);                  \
      PUSHMOVE2016(button, ([&](button_t *e) {    \
        e->x = button_x;                          \
        e->y = button_y;                          \
        e->whatold = button_wh;                   \
      }));                                        \
    }                                             \
  } while (0)

#define LITEWIRE2016(xx, yy, wh, pd, cc) do {           \
    if (ANIMATING) {                                    \
      const int lw_xx = (xx), lw_yy = (yy);             \
      const int lw_wh = (wh), lw_pd = (pd);             \
      const int lw_cc = (cc);                           \
      PUSHMOVE2016(litewire, ([&](litewire_t *e) {      \
        e->x = lw_xx;                                   \
        e->y = lw_yy;                                   \
        e->what = lw_wh;                                \
        e->dir = lw_pd;                                 \
        e->count = lw_cc;                               \
      }));                                              \
    }                                                   \
  } while (0)

// Helper functions -- these actually have an effect on the level state,
// in addition to animating.

// Modifies the in-scope entity position variables, as well as updating
// the permanent location. XXX maybe this is a bad idea?
#define SETENTPOS2016(xx, yy) do {   \
    entx = xx;                       \
    enty = yy;                       \
    SetEntPos(enti, entx, enty);     \
  } while (0)

// Note that this macro has the side-effect of actually doing the
// tile swap!
#define SWAPO2016(idx) do {                   \
    const int swapo_idx = (idx);              \
    PUSHMOVE2016(swap, ([&](swap_t *e) {      \
      int xx, yy;                             \
      where(swapo_idx, xx, yy);               \
      e->x = xx;                              \
      e->y = yy;                              \
      e->was = tileat(xx, yy);                \
      e->now = otileat(xx, yy);               \
    }));                                      \
    swapo(swapo_idx);                         \
  } while (0)

#define SWAPTILES2016(t1, t2, d) do {                 \
    const int st_t1 = (t1), st_t2 = (t2), st_d = (d); \
    for (int y = 0; y < h; y++) {                     \
      for (int x = 0; x < w; x++) {                   \
        const int t = tileat(x, y);                   \
        if (t == st_t1) {                             \
          AFFECT2016(x, y);                           \
          TOGGLE2016(x, y, t, st_d);                  \
          settile(x, y, st_t2);                       \
        } else if (t == st_t2) {                      \
          AFFECT2016(x, y);                           \
          TOGGLE2016(x, y, t, st_d);                  \
          settile(x, y, st_t1);                       \
        }                                             \
      }                                               \
    }                                                 \
  } while (0)

/* after stepping off a tile, deactivate a panel
   if there was one there. */
/* nb: only for regular panels */
#define CHECKLEAVEPANEL2016(xx, yy) do {                                \
    const int checkleavepanel_xx = (xx);                                \
    const int checkleavepanel_yy = (yy);                                \
    if (tileat(checkleavepanel_xx, checkleavepanel_yy) == T_PANEL) {    \
      AFFECTI2016(destat(checkleavepanel_xx, checkleavepanel_yy));      \
      SWAPO2016(destat(checkleavepanel_xx, checkleavepanel_yy));        \
    }                                                                   \
  } while (0)

#define CHECKTRAP2016(xx, yy) do {                                      \
    const int checktrap_xx = (xx), checktrap_yy = (yy);                 \
    if (tileat(checktrap_xx, checktrap_yy) == T_TRAP1) {                \
      AFFECT2016(checktrap_xx, checktrap_yy);                           \
      settile(checktrap_xx, checktrap_yy, T_HOLE);                      \
      TRAP2016(checktrap_xx, checktrap_yy, T_TRAP1);                    \
    } else if (tileat(checktrap_xx, checktrap_yy) == T_TRAP2) {         \
      AFFECT2016(checktrap_xx, checktrap_yy);                           \
      settile(checktrap_xx, checktrap_yy, T_TRAP1);                     \
      TRAP2016(checktrap_xx, checktrap_yy, T_TRAP2);                    \
    }                                                                   \
  } while (0)

/* actions on the player stepping off of a tile */
/* generally, you should only call this once per
   motion, at the very end. that's because it may
   install new panels (by swapping), and panel swaps
   are supposed to happen at the end. */
#define CHECKSTEPOFF2016(xx, yy) do {                                  \
    const int checkstepoff_xx = (xx), checkstepoff_yy = (yy);          \
    CHECKTRAP2016(checkstepoff_xx, checkstepoff_yy);                   \
    CHECKLEAVEPANEL2016(checkstepoff_xx, checkstepoff_yy);             \
  } while (0)

/* must call this whenever a bot steps
   onto a tile where there might be other
   bots. (walking atop one, or teleporting)

   we assume that there is at most ONE other
   bot, by invariant (which this function restores).
*/
#define CHECKBOTDEATH2016(xx, yy, me) do {                  \
    const int cbd_xx = (xx), cbd_yy = (yy);                 \
    const int cbd_me = (me);                                \
    if (cbd_me != B_PLAYER) { /* checked at end of turn */  \
      const int mei = index(cbd_xx, cbd_yy);                \
      for (int b = 0; b < nbots; b++) {                     \
        if (cbd_me != b && bott[b] != B_DELETED &&          \
            bott[b] != B_BOMB_X &&                          \
            mei == boti[b]) {                               \
          /* yes! delete other bot and make me */           \
          /* broken. */                                     \
          AFFECTI2016(mei);                                 \
          bott[b] = B_DELETED;                              \
          bott[cbd_me] = B_BROKEN;                          \
          /* AFFECTENT;  */                                 \
          BOTEXPLODE2016(b);                                \
        }                                                   \
      }                                                     \
    }                                                       \
  } while (0)


template<bool ANIMATING, class DAB>
void Level::Bombsplode(int now,
                       int b, DAB *ctx, AList *&events,
                       AList **&etail) {
  bott[b] = B_BOMB_X;
  int x, y;
  where(boti[b], x, y);

  /* animate first */
  if (ANIMATING) {
    for (dir dd = FIRST_DIR_SELF; dd < LAST_DIR; dd++) {
      int bx, by;
      if (travel(x, y, dd, bx, by)) {
        AFFECT2016(bx, by);
      }
    }

    PUSHMOVE2016(bombsplosion, [&](bombsplosion_t *e) {
      e->x = x;
      e->y = y;
    });
  }

  for (dir dd = FIRST_DIR_SELF; dd <= LAST_DIR; dd++) {
    int bx, by;
    if (travel(x, y, dd, bx, by)) {
      if (Bombable(tileat(bx, by))) {
        /* animate? */
        settile(bx, by, T_FLOOR);
        /* clear flags */
        setflag(bx, by, flagat(bx, by) & ~(TF_HASPANEL |
                                           TF_RPANELL  |
                                           TF_RPANELH));
      }


      int z = index(bx, by);
      for (int bdie = 0; bdie < nbots; bdie++) {
        if (boti[bdie] == z) {
          AFFECTENT2016(bdie, []{});

          bot bd = bott[bdie];
          if (bd == B_DELETED ||
              bd == B_BOMB_X) /* ignore */ continue;

          if (Level::isbomb(bd)) {
            /* chain reaction */
            if (bdie < now) {
              Bombsplode<ANIMATING, DAB>(now, bdie, ctx, events, etail);
              break;
            } else {
              /* will explode this turn (unless a bot
                 pushes it??) */
              bota[bdie] = 0;
              break;
            }
          } else {
            /* non-bomb, so just kill it */
            /* FIXME: This isn't correct in the case that
               some bots move after the bomb explodes; they
               might move *onto* the explosion. See
               "revenge of the malformed levels 2". Of
               course, this can only happen if there are
               bots with higher numbers than bombs, which
               levels can't be made by the editor. Possibly
               they should be rejected by the sanitizer as
               well. */
            BOTEXPLODE2016(bdie);
            bott[bdie] = B_DELETED;
            break;
          } /* isbomb? */
        } /* bot here? */
      } /* loop over bots */

    } /* adjacent? */
  } /* loop adjacent */
}


// Assumes target = T_TRANSPORT.
template<bool ANIMATING, class DAB>
bool Level::MoveEntTransport(dir d, int enti, Capabilities cap,
                             int entx, int enty, int newx, int newy,
                             DAB *ctx, AList *&events,
                             AList **&etail) {
  /* not if there's an entity there */
  if (playerat(newx, newy) ||
      botat(newx, newy)) return false;

  if (cap & (CAP_CANTELEPORT | CAP_ISPLAYER)) {
    AFFECT2016(newx, newy);

    AFFECTENT2016(enti, []{});
    WALKED2016(d, true);
    AFFECTENT2016(enti, []{});

    TELEPORTOUT2016(enti, newx, newy);

    int targx, targy;
    where(dests[w * newy + newx], targx, targy);

    /* cache this, since stepping off might change it */
    const int targ = tileat(targx, targy);

    CHECKSTEPOFF2016(entx, enty);
    SETENTPOS2016(targx, targy);

    AFFECT2016(targx, targy);
    AFFECTENT2016(enti, []{});

    /* teleporting always faces the player down;
       there's just one animation. */
    SetEntDir(enti, DIR_DOWN);

    TELEPORTIN2016(enti, targx, targy);

    switch (targ) {
    case T_PANEL:
      AFFECTI2016(destat(targx, targy));
      SWAPO2016(destat(targx, targy));
      break;
    default:;
    }

    CHECKBOTDEATH2016(targx, targy, enti);

    return true;
  }
  return false;
}

// Assumes target == T_EXIT.
template<bool ANIMATING, class DAB>
bool Level::MoveEntExit(dir d, int enti, Capabilities cap,
                        int entx, int enty, int newx, int newy,
                        DAB *ctx, AList *&events,
                        AList **&etail) {
  /* XXX check cap */

  /* can't push bots off exits */
  if (playerat(newx, newy) ||
      botat(newx, newy)) return false;

  CHECKSTEPOFF2016(entx, enty);

  AFFECTENT2016(enti, []{});
  WALKED2016(d, false);

  SETENTPOS2016(newx, newy);

  /* open door */
  AFFECT2016(newx, newy);

  PUSHMOVE2016(opendoor, [&](opendoor_t *e) {
    e->x = newx;
    e->y = newy;
  });

  return true;
}

// Assumes target == T_ON.
template<bool ANIMATING, class DAB>
bool Level::MoveEntOn(dir d, int enti, Capabilities cap,
                      int entx, int enty, int newx, int newy,
                      DAB *ctx, AList *&events,
                      AList **&etail) {
  /* can't activate if someone stands on it */
  if (playerat(newx, newy) ||
      botat(newx, newy)) return false;

  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      if (tileat(x, y) == T_ELECTRIC) {
        AFFECT2016(x, y);
        TOGGLE2016(x, y, T_ELECTRIC, abs(x - newx) + abs(y - newy));
        settile(x, y, T_FLOOR);
      }
    }
  }

  AFFECT2016(newx, newy);
  settile(newx, newy, T_OFF);
  /* XXX animate pushing, but not moving */
  BUTTON2016(newx, newy, T_ON);
  return true;
}

// Assumes target is one of RSPHERE, GSPHERE, BSPHERE, SPHERE, or GOLD.
template<bool ANIMATING, class DAB>
bool Level::MoveEntGoldlike(int target, dir d, int enti, Capabilities cap,
                            int entx, int enty, int newx, int newy,
                            DAB *ctx, AList *&events,
                            AList **&etail) {
  /* spheres allow pushing in a line:
     ->OOOO  becomes OOO   ---->O

     so keep traveling while the tile
     in the destination direction is
     a sphere of any sort.
  */
  const int firstx = newx, firsty = newy;
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

  if (ANIMATING) {
    /* if we passed through any spheres,
       'jiggle' them (even if we don't ultimately push). */
    if (firstx != newx || firsty != newy) {
      /* affect whole row */
      for (int ix = firstx, iy = firsty;
           (firstx != newx && firsty != newy);
           travel(ix, iy, d, ix, iy)) {
        AFFECT2016(ix, iy);
      }

      const int num = abs(newx - firstx) + abs(newy - firsty);
      PUSHMOVE2016(jiggle, ([&](jiggle_t *e) {
        e->startx = firstx;
        e->starty = firsty;
        e->d = d;
        e->num = num;
      }));
    }
  }

  int goldx = newx, goldy = newy;

  /* remove gold block */
  AFFECT2016(goldx, goldy);
  const int replacement = (flagat(goldx, goldy) & TF_HASPANEL) ?
    realpanel(flagat(goldx, goldy)) :
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
    if (ANIMATING) {
      const bool did = ctx->affect(goldx, goldy, this, etail);
      if (false)
        printf("%d %d: %s %d %d\n", goldx, goldy, did ? "did" : "not",
               ctx->serialat(goldx, goldy),
               ctx->Serial());
    }
  }

  /* goldx is dest, newx is source */
  if (goldx != newx ||
      goldy != newy) {
    const int landon = tileat(goldx, goldy);
    const bool doswap = ([&]{
      /* untrigger from source */
      if (flagat(newx, newy) & TF_HASPANEL) {
        const int pan = realpanel(flagat(newx,newy));
        /* any */
        /* XXX2016? use Level::triggers here */
        if (pan == T_PANEL ||
            /* colors */
            (target == T_GSPHERE &&
             pan == T_GPANEL) ||
            (target == T_RSPHERE &&
             pan == T_RPANEL) ||
            (target == T_BSPHERE &&
             pan == T_BPANEL)) {
          return true;
        }
      }
      return false;
    }());

    /* only the correct color sphere can trigger
       the colored panels */
    const bool doswapt = Level::triggers(target, landon);

    /* XXX may have already affected this
       if the gold block didn't move at all */
    AFFECT2016(goldx, goldy);
    settile(goldx, goldy, target);

    const bool zapped = ([&]{
      if (landon == T_ELECTRIC) {
        /* gold zapped. however, if the
           electric was the target of a panel
           that we just left, the electric has
           been swapped into the o world (along
           with the gold). So swap there. */
        settile(goldx, goldy, T_ELECTRIC);
        return true;
      }
      return false;
    }());

    if (ANIMATING) {
      const int distance = abs((goldx - newx) + (goldy - newy));
      PUSHMOVE2016(fly, ([&](fly_t *e) {
        e->what = target;
        e->whatunder = replacement;
        e->srcx = newx;
        e->srcy = newy;
        e->d = d;
        /* number of tiles traveled */
        e->distance = distance;
        e->zapped = zapped;
      }));
    }

    if (doswapt) {
      AFFECTI2016(destat(goldx, goldy));
      SWAPO2016(destat(goldx, goldy));
    }

    if (doswap) {
      AFFECTI2016(destat(newx, newy));
      SWAPO2016(destat(newx, newy));
    }

    return true;

  } else {
    /* didn't move; put it back */
    settile(newx, newy, target);

    /* TODO: animate it when 'stuck' */
    return false;
  }
}


// assumes target is T_BUTTON.
template<bool ANIMATING, class DAB>
bool Level::MoveEntButton(dir d, int enti, Capabilities cap,
                          int entx, int enty, int newx, int newy,
                          DAB *ctx, AList *&events,
                          AList **&etail) {
  /* careful: the order in which swaps happen
     matters, because an earlier pulse could
     block beams for a later one (by raising floor).
     So we have to save up the effects and then
     perform them all at once at the end. (Panel
     swaps from remotes are last as always.)
  */

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
  // PERF can probably just use vector<>?
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
  AFFECT2016(newx, newy);
  BUTTON2016(newx, newy, T_BUTTON);

  for (dir dd = FIRST_DIR; dd <= LAST_DIR; dd++) {
    // if animation is off, then don't pre-scan
    if (!ANIMATING ||
        isconnected(newx, newy, dd)) {

      /* send a pulse in that direction. */
      int pulsex = newx, pulsey = newy;
      dir pd = dd;

      int dist = 0;

      while (pd != DIR_NONE &&
             travel(pulsex, pulsey, pd, pulsex, pulsey)) {
        const int targ = tileat(pulsex, pulsey);

        /* liteup animation if a wire. note: this code
           would lite up any tile (floor, exit, etc.) except
           that those are avoided by the pre-scan above. */
        if (ANIMATING) {
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
            if (ctx->affect(pulsex, pulsey, this, etail)) {
              dist = 0;
            }
            LITEWIRE2016(pulsex, pulsey, targ, pd, dist);
            break;
          }
        }

        dist++;

        switch (targ) {
        case T_REMOTE:
          if (ANIMATING) {
            AFFECT2016(pulsex, pulsey);
            PUSHMOVE2016(liteup, ([&](liteup_t *e) {
              e->x = pulsex;
              e->y = pulsey;
              e->what = targ;
              e->delay = dist;
            }));
          }
          remotes = new SwapList(destat(pulsex, pulsey), remotes);

          /* since this counts as being connected so far, we want to
             make sure that the circuit continues before animating
             it ... */
          if (ANIMATING) {
            if (!isconnected(pulsex, pulsey, pd)) pd = DIR_NONE;
          }
          continue;

        case T_BLIGHT:
        case T_RLIGHT:
        case T_GLIGHT:
          if (ANIMATING) {
            AFFECT2016(pulsex, pulsey);
            PUSHMOVE2016(liteup, ([&](liteup_t *e) {
              e->x = pulsex;
              e->y = pulsey;
              e->what = targ;
              e->delay = dist;
            }));
          }

          if (targ == T_BLIGHT) bswaps++;
          if (targ == T_RLIGHT) rswaps++;
          if (targ == T_GLIGHT) gswaps++;
          pd = DIR_NONE;
          break;

        case T_TRANSPONDER: {
          // printf("transponder at %d/%d\n", pulsex, pulsey);
          const int transx = pulsex;
          const int transy = pulsey;
          if (!travel(pulsex, pulsey, pd, pulsex, pulsey)) {
            pd = DIR_NONE;
          } else {
            /* keep going until we hit another transponder. */
            do {
              const int ta = tileat(pulsex, pulsey);
              // printf(" ... at %d/%d: %d\n", pulsex, pulsey, ta);
              if (!allowbeam(ta) ||
                  botat(pulsex, pulsey) ||
                  playerat(pulsex, pulsey)) {
                /* hit something. is it a transponder? */
                if (ta == T_TRANSPONDER) {
                  /* okay, then we are on the 'old' tile with
                     the direction set, so we're ready to continue
                     the pulse loop */
                  LITEWIRE2016(pulsex, pulsey, targ, pd, dist);
                  PUSHMOVE2016(transponderbeam, ([&](transponderbeam_t *e) {
                    e->x = transx;
                    e->y = transy;
                    e->lx = pulsex;
                    e->ly = pulsey;
                    e->from = pd;
                    e->count = dist;
                  }));

                } else {
                  /* didn't hit transponder! stop. */
                  pd = DIR_NONE;
                }
                break;
              } else {
                /* in preparation for beam across these squares... */
                AFFECT2016(pulsex, pulsey);
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
    SWAPTILES2016(T_BUP, T_BDOWN, 0);
  }

  while (rswaps--) {
    SWAPTILES2016(T_RUP, T_RDOWN, 0);
  }

  while (gswaps--) {
    SWAPTILES2016(T_GUP, T_GDOWN, 0);
  }

  while (remotes != nullptr) {
    SwapList *t = remotes;
    remotes = remotes->next;
    if (ANIMATING) {
      int x, y;
      where(t->target, x, y);
      if (false) printf("(was %d %d) ",
                        ctx->serialat(x, y),
                        ctx->Serial());
      bool did = ctx->affecti(t->target, this, etail);
      if (false) printf("%d=%d,%d: %s %d %d\n",
                        t->target, x, y, did ? "did" : "not",
                        ctx->serialat(x, y),
                        ctx->Serial());
      // AFFECTI(t->target);
    }
    SWAPO2016(t->target);
    delete t;
  }

  return true;
}

// Assumes target == T_HEARTFRAMER.
template<bool ANIMATING, class DAB>
bool Level::MoveEntHeartframer(dir d, int enti, Capabilities cap,
                               int entx, int enty, int newx, int newy,
                               DAB *ctx, AList *&events,
                               AList **&etail) {
  if (botat(newx, newy) ||
      playerat(newx, newy)) return false;

  /* only the player can pick up
     heart framers */
  if (cap & CAP_HEARTFRAMERS) {
    AFFECT2016(newx, newy);
    AFFECTENT2016(enti, []{});
    WALKED2016(d, true);

    /* snag heart framer */
    settile(newx, newy, T_FLOOR);

    PUSHMOVE2016(getheartframer, [&](getheartframer_t *e) {
      e->x = newx;
      e->y = newy;
    });

    /* any heart framers left? */

    if (!hasframers()) {
      for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
          const int t = tileat(x, y);
          if (t == T_SLEEPINGDOOR) {
            AFFECT2016(x, y);
            PUSHMOVE2016(wakeupdoor, [&](wakeupdoor_t *e) {
              e->x = x; e->y = y;
            });
            settile(x, y, T_EXIT);
          }
        }
      }

      /* also bots */
      for (int i = 0; i < nbots; i++) {
        const int orig_bott = bott[i];
        switch (orig_bott) {
        case B_DALEK_ASLEEP:
        case B_HUGBOT_ASLEEP:
          AFFECTENT2016(i, [&]() {
            AFFECTI2016(boti[i]);
          });

          bott[i] =
            (orig_bott == B_DALEK_ASLEEP) ?
            B_DALEK : B_HUGBOT;
          PUSHMOVE2016(stand, [&](stand_t *e) {
            where(boti[i], e->x, e->y);
            e->d = botd[i];
            e->entt = bott[i];
            e->data = bota[i];
          });

          if (ANIMATING) {
            int xx, yy;
            where(boti[i], xx, yy);

            PUSHMOVE2016(wakeup, [&](wakeup_t *e) {
              e->x = xx;
              e->y = yy;
            });
          }
          break;

        default:;
        }
      }
    }

    /* panel actions are last */
    CHECKSTEPOFF2016(entx, enty);

    SETENTPOS2016(newx, newy);

    return true;
  }
  return false;
}

// Assumes target == T_ELECTRIC.
template<bool ANIMATING, class DAB>
bool Level::MoveEntElectric(dir d, int enti, Capabilities cap,
			    int entx, int enty, int newx, int newy,
			    DAB *ctx, AList *&events,
			    AList **&etail) {
  if (botat(newx, newy) ||
      playerat(newx, newy)) return false;
  /* some bots are stupid enough
     to zap themselves */
  if (enti != B_PLAYER &&
      (cap & CAP_ZAPSELF)) {

    AFFECT2016(newx, newy);
    AFFECTENT2016(enti, []{});
    WALKED2016(d, false);

    /* change where it is */
    boti[enti] = index(newx, newy);

    /* then kill it */
    bott[enti] = B_DELETED;
    AFFECTENT2016(enti, []{});
    BOTEXPLODE2016(enti);

    /* might have stepped off a panel/trap */
    CHECKSTEPOFF2016(entx, enty);

    return true;
  } 
  return false;
}

// Assumes target == T_BROKEN.
template<bool ANIMATING, class DAB>
bool Level::MoveEntBroken(dir d, int enti, Capabilities cap,
			  int entx, int enty, int newx, int newy,
			  DAB *ctx, AList *&events,
			  AList **&etail) {
  if (playerat(newx, newy) ||
      botat(newx, newy)) return false;

  AFFECT2016(newx, newy);
  settile(newx, newy, T_FLOOR);

  PUSHMOVE2016(breaks, ([&](breaks_t *e) {
    e->x = newx;
    e->y = newy;
  }));
  return true;
}

// Assumes target == T_GREEN.
template<bool ANIMATING, class DAB>
bool Level::MoveEntGreen(dir d, int enti, Capabilities cap,
			 int entx, int enty, int newx, int newy,
			 DAB *ctx, AList *&events,
			 AList **&etail) {
  if (playerat(newx, newy) ||
      botat(newx, newy)) return false;

  int destx, desty;
  if (travel(newx, newy, d, destx, desty) &&
      tileat(destx, desty) == T_FLOOR &&
      !botat(destx, desty) &&
      !playerat(destx, desty)) {
    settile(destx, desty, T_BLUE);
    settile(newx, newy, T_FLOOR);

    AFFECT2016(destx, desty);
    AFFECT2016(newx, newy);
    AFFECTENT2016(enti, []{});

    PUSHMOVE2016(pushgreen, ([&](pushgreen_t *e) {
      e->srcx = newx;
      e->srcy = newy;
      e->d = d;
    }));
      
    WALKED2016(d, true);

    CHECKSTEPOFF2016(entx, enty);

    SETENTPOS2016(newx, newy);

    return true;
  }

  return false;
}


// Assumes target is one of RPANEL, GPANEL, BPANEL, TRAP1, TRAP2,
// FLOOR, ROUGH, RDOWN, GDOWN, BDOWN, or PANEL.
template<bool ANIMATING, class DAB>
bool Level::MoveEntFloorlike(int target, dir d, int enti, Capabilities cap,
			     int entx, int enty, int newx, int newy,
			     DAB *ctx, AList *&events,
			     AList **&etail) {
  bool pushing = false;
  int pushent = 0;
  if (playerat(newx, newy)) {
    /* if player is on bot, can't push */
    if (botat(newx, newy)) return false;
    /* push? */
    if (cap & CAP_PUSHPLAYER) {
      pushing = true;
      pushent = B_PLAYER;
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
    /* OK, we know that the next spot contains a bot (or player), and
       we have the capability to push that entity. But we have to
       check what lives beyond this spot. */
    int farx, fary;
    if (travel(newx, newy, d, farx, fary)) {
      int ftarget = tileat(farx, fary);
      switch (ftarget) {
      case T_ELECTRIC:
	/* only bots pushed into electric */
	if (pushent == B_PLAYER) return false;
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
      /* also check bot -- can't push two in a row */
      if (botat(farx, fary)) return false;
      if (playerat(farx, fary)) return false;

      /* affect first, then make anims */

      AFFECTENT2016(enti, ([&]{
	AFFECTENT2016(pushent, ([&]{
	  AFFECT2016(farx, fary);
	  AFFECT2016(newx, newy);

	  /* if a bomb, light and reset fuse */
	  /* XXX animate? */
	  if (pushent != -1 &&
	      isbomb(bott[pushent]))
	    bota[pushent] = ((int)bott[pushent] - (int)B_BOMB_0);

	  // XXX2016: This inverts the order that postaffect
	  // ran vs the move.h approach. (But it looks like that
	  // shouldn't matter; they both just change serial?)
	}));
      }));


      /* XXX should be "waspushed" or whatever */
      WALKEDEX2016(d, newx, newy, pushent, false);
      /* okay, now move the pushing ent */
      WALKED2016(d, true);

      /* okay, push! */
      if (pushent == B_PLAYER) {
	guyx = farx;
	guyy = fary;
      } else {
	int id = index(farx, fary);
	boti[pushent] = id;
      }

      /* handle leaving current (pusher) pos */
      CHECKTRAP2016(entx, enty);
      /* but still need to check panels, later... */
      const int srcx = entx, srcy = enty;
      const bool swapsrc = tileat(entx, enty) == T_PANEL;

      /* then move me. */
      SETENTPOS2016(newx, newy);

      /* now deal with zapping */
      if (ftarget == T_ELECTRIC &&
	  pushent != -1) {
	/* can't be player: kill bot */
	bott[pushent] = B_DELETED;
	AFFECT2016(farx, fary);
	BOTEXPLODE2016(pushent);
      }

      /* the tile in the middle is being stepped off
	 and stepped on. if it's a panel, don't do anything.
	 (to avoid a double swap) */
      if (target == T_PANEL) {
	/* do nothing */
      } else {
	CHECKTRAP2016(newx, newy);
      }

      /* -- panel phase -- */

      /* first, if pusher stepped off a panel, it swaps */
      if (swapsrc) {
	AFFECTI2016(destat(srcx, srcy));
	SWAPO2016(destat(srcx, srcy));
      }

      /* pushed ent is stepping onto new panel, perhaps */
      if (ftarget == T_PANEL) {
	AFFECTI2016(destat(farx, fary));
	SWAPO2016(destat(farx, fary));
      }

      return true;
    } else {
      return false;
    }

  } else {
    // Not pushing.
    
    /* XXX also affect source? */
    AFFECTENT2016(enti, ([&]{
      AFFECT2016(newx, newy);
    }));
    WALKED2016(d, false);

    /* might have stepped onto bot */
    CHECKBOTDEATH2016(newx, newy, enti);

    /* panel actions */
    CHECKSTEPOFF2016(entx, enty);

    SETENTPOS2016(newx, newy);

    if (target == T_PANEL) {
      // printf(" %d   step on panel...\n", enti);
      AFFECTI2016(destat(newx, newy));
      SWAPO2016(destat(newx, newy));
    }

    return true;
  }
}


#endif
