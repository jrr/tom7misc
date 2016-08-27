
// New move header. Include just once!
#ifndef __MOVE2016_H
#define __MOVE2016_H

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

// This new version of the macro looks more like a function call.
// It requires the following to be in scope:
//     etail   (XXX docs)
//     ctx, which is a Disamb* or NullDisamb*.
//     ANIMATING (a compile-time bool, indicating whether animation is
//                turned on).
// It should be used like this:
//
// PUSHMOVE2016(bombsplosion, [&](bomsplosion_t *e) {
//   e->x = x;
//   e->y = y;
// });
//
// No code is executed (lambda is dead) if ANIMATING is false. Scope
// of fn should be as expected and the macro should behave like a
// normal statement.
#define PUSHMOVE2016(type, fn) do {		\
    if (ANIMATING) {				\
      (fn)([&]() -> type ## _t * {		\
	aevent *a = new aevent;			\
	*etail = new AList(a, nullptr);		\
        etail = &((*etail)->next);		\
        a->serial = ctx->Serial();		\
        a->t = tag_ ## type;			\
        return &(a->u. type );			\
      }());					\
    };						\
  } while (0)

// Expected in scope: ctx, ANIMATING
// fn is a lambda that takes no args. It is executed whether animation
// on or not!
#define AFFECTENT2016(entid, fn) do {					\
    const int affectent_ei = (entid);					\
    if (ANIMATING) {							\
      if (affectent_ei == -1) ctx->preaffectplayer(this, etail);	\
      else ctx->preaffectbot(affectent_ei, this, etail);		\
    }									\
    (fn)();								\
    if (ANIMATING) {							\
      if (affectent_ei == -1) ctx->postaffectplayer();			\
      else ctx->postaffectbot(affectent_ei);				\
    }									\
  } while (0)

# define BOTEXPLODE2016(botidx) do {		      \
    if (ANIMATING) {				      \
      const int botexplode_botidx = (botidx);	      \
      PUSHMOVE2016(botexplode, [&](botexplode_t *e) { \
        where(boti[botexplode_botidx], e->x, e->y);   \
      });					      \
    }						      \
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
        // XXX2016 should be macro?
        (void)ctx->affect(bx, by, this, etail);
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

      
#endif
