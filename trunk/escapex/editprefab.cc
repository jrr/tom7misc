
#include "SDL.h"
#include <math.h>
#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"

#include "escapex.h"
#include "play.h"
#include "prompt.h"

#include "escape-util.h"
#include "edit.h"

#include "loadlevel.h"

#include "message.h"
#include "menu.h"

enum { PF_NONE = 0, PF_TIMER, PF_FILE, };

/* move him out of the selection */
bool Editor::moveplayersafe() {
  if (level->guyx >= selection.x &&
      level->guyy >= selection.y &&
      level->guyx < (selection.x + selection.w) &&
      level->guyy < (selection.y + selection.h)) {
    for (int y = 0; y < level->h; y++)
      for (int x = 0; x < level->w; x++) {
        if (x >= selection.x &&
            y >= selection.y &&
            x < (selection.x + selection.w) &&
            y < (selection.y + selection.h)) continue;

        if (level->botat(x, y)) continue;

        level->guyx = x;
        level->guyy = y;

        return true;
      }
    return false;
  } else return true;
}

void Editor::prefab() {
  if (selection.w) {
    /* display menu to select. */
    int what = 0;

    Label message;
    message.text =
      PICS QICON POP
      " Select a prefab to insert.";

    VSpace spacer(fon->height);

    Okay timer("Timer", &what, PF_TIMER);
    timer.explanation = "Insert a bot-based countdown timer\n"
                        "for a specified number of moves.";

    Okay file("File", &what, PF_FILE);
    file.explanation = "Paste the contents of another level\n"
                       "in a file you choose.";

    Cancel can;

    PtrList<MenuItem> *l = nullptr;

    PtrList<MenuItem>::push(l, &can);
    PtrList<MenuItem>::push(l, &spacer);

    PtrList<MenuItem>::push(l, &file);
    PtrList<MenuItem>::push(l, &timer);
    PtrList<MenuItem>::push(l, &spacer);

    PtrList<MenuItem>::push(l, &message);

    /* display menu */
    std::unique_ptr<Menu> mm = Menu::Create(this, "Which prefab?", l, false);
    InputResultKind res = mm->menuize();
    PtrList<MenuItem>::diminish(l);
    mm.reset();

    if (res == InputResultKind::OK) {
      switch (what) {
      case PF_TIMER:
        if (!moveplayersafe()) {
          Message::No(this, "There's nowhere to put the player!");
        } else {
          pftimer();
        }
        break;
      case PF_FILE:
        pffile();
        break;
      default:;
        Message::Bug(this, "no prefab was selected..??");
      }
    }
  } else {
    Message::No(this, "You must select a region first.\n"
                "   " GREY "(drag with the right mouse button held)" POP);
  }
  Redraw();
}


/* XXX should allow me to specify the prompt for loading */
void Editor::pffile() {
  std::unique_ptr<Level> small =
    [this]() -> std::unique_ptr<Level> {
    std::unique_ptr<LoadLevel> ll{
      LoadLevel::Create(plr, EDIT_DIR, true, true)};
    if (ll.get() == nullptr) {
      Message::Quick(this, "Can't open load screen!",
                     "Ut oh.", "", PICS XICON POP);
      Redraw();
      return nullptr;
    }
    string res = ll->SelectLevel();
    string ss = readfile(res);

    /* allow corrupted files */
    return Level::FromString(ss, true);
  }();

  if (!small) {
    dr.message = ((string) RED "error loading file to place" POP);
    return;
  }

  /* now menu */
  /* XXX should show preview of level we're about to place */
  Label message;
  message.text =
    PICS QICON POP
    " "   "This will place the level " BLUE + small->title + POP;
  Label message2;
  message2.text =
    "   " "within the selected region.";

  VSpace spacer(fon->height);

  Okay ok("OK");

  Cancel can;

  TextInput xoff;
  xoff.explanation = "Ignore this many columns in the input.";
  xoff.question = "X offset: ";
  xoff.input = "0";
  TextInput yoff;
  yoff.explanation = "Ignore this many rows in the input.";
  yoff.question = "Y offset: ";
  yoff.input = "0";

  /* XXX (don't) constrain to selection option? */


  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spacer);

  PtrList<MenuItem>::push(l, &yoff);
  PtrList<MenuItem>::push(l, &xoff);

  PtrList<MenuItem>::push(l, &message2);
  PtrList<MenuItem>::push(l, &message);

  /* display menu */
  std::unique_ptr<Menu> mm = Menu::Create(this, "Inserting file", l, false);
  InputResultKind res = mm->menuize();
  PtrList<MenuItem>::diminish(l);
  mm.reset();

  if (res == InputResultKind::OK) {
    int xo = EscapeUtil::stoi(xoff.input);
    int yo = EscapeUtil::stoi(yoff.input);
    if (xo < 0 || yo < 0) {
      Message::No(this, "bad offsets");
      return;
    }

    /* remove existing bots/player in selection */
    {
      moveplayersafe();
      for (int x = selection.x; x < selection.x + selection.w; x++)
        for (int y = selection.y; y < selection.y + selection.h; y++)
          clearbot(x, y);
    }


    /* tiles and dests */
    for (int y = 0; y < small->h; y++)
      for (int x = 0; x < small->w; x++) {

        int srcx = x + xo;
        int srcy = y + yo;
        int dstx = x + selection.x;
        int dsty = y + selection.y;

        /* has to be in both source and destination. */
        if (srcx >= 0 && srcx < small->w &&
            srcy >= 0 && srcy < small->h &&

            dstx >= selection.x && dstx < selection.x + selection.w &&
            dsty >= selection.y && dsty < selection.y + selection.h

            /*
              or for no cropping...
            dstx >= 0 && dstx < level->w &&
            dsty >= 0 && dsty < level->h
            */
            ) {

          level->settile(dstx, dsty,
                          small->tileat(srcx, srcy));
          level->osettile(dstx, dsty,
                            small->otileat(srcx, srcy));
          if (Level::needsdest(level->tileat(dstx, dsty)) ||
              Level::needsdest(level->otileat(dstx, dsty))) {

            int sdx, sdy;
            small->getdest(srcx, srcy, sdx, sdy);
            sdx += (dstx - srcx);
            sdy += (dsty - srcy);
            /* then make sure it's in bounds... */
            if (sdx < 0) sdx = 0;
            if (sdx >= level->w) sdx = level->w - 1;
            if (sdy < 0) sdy = 0;
            if (sdy >= level->w) sdy = level->w - 1;
            level->setdest(dstx, dsty, sdx, sdy);
          }
        }

      }

    /* bots */
    for (int i = 0; i < small->nbots; i++) {
      int xx, yy;
      small->where(small->boti[i], xx, yy);

      xx += selection.x - xo;
      yy += selection.y - yo;
      if (xx >= selection.x && yy >= selection.y &&
          xx < selection.x + selection.w &&
          yy < selection.y + selection.h) {
        if (level->nbots < LEVEL_MAX_ROBOTS)
          this->addbot(xx, yy, small->bott[i]);
      }
    }

    /* definitely need to fix up (canonicalize bot order
       especially) */
    FixUp();
  }

  Redraw();
}

/* checks the condition at time t, for pftimer below */
static bool timer_check(int *M, int *A, int i, int t) {
  for (int z = 0; z < i; z++) {
    if (((A[z] + t) % M[z]) != 0) return false;
  }
  return true;
}

/* insert a timer. Prompt for the number of moves, then try to use
   the minimum number of bots within the given space to form the
   timer. */
void Editor::pftimer() {
  Label message;
  message.text =
    PICS QICON POP
    " " RED "Note:" POP " Please use timers in good taste; leave a";
  Label message2;
  message2.text =
    "   " "little slack for the player!";

  VSpace spacer(fon->height);

  Okay ok("OK");

  Cancel can;

  TextInput nmoves;
  nmoves.explanation = "The player will have this many moves\n"
    "before the bot reaches the stop sign.";
  nmoves.question = "Number of moves: ";

  /* XX could check automatically based on location of selection */
  Toggle reverse;
  reverse.question = "Player on left";
  reverse.explanation = "The timer must be on either the left side\n"
                        "or right side of the level. Check this to\n"
                        "put it on the right side (with the player)\n"
                        "on the left.";
  reverse.checked = false;

  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spacer);

  PtrList<MenuItem>::push(l, &reverse);
  PtrList<MenuItem>::push(l, &nmoves);

  PtrList<MenuItem>::push(l, &message2);
  PtrList<MenuItem>::push(l, &message);

  /* display menu */
  std::unique_ptr<Menu> mm = Menu::Create(this, "Inserting timer", l, false);
  InputResultKind res = mm->menuize();
  PtrList<MenuItem>::diminish(l);
  mm.reset();

  if (res == InputResultKind::OK) {
    int n = EscapeUtil::stoi(nmoves.input);
    if (n <= 0) {
      Message::No(this, "bad number of moves");
      return;
    }

    /* the general idea here is to form a series of rows, each
       containing a dalek that walks left and into a transporter that
       resets him to his "home" position (offset 0). the length of
       this cycle is M_i, the modulus.

       each bot starts at an offset from the home position, which is
       A_i.

       in the home position there is a panel that targets a series
       of transporters for the trigger bot. These transporters all
       target the trigger bot's home position (offset 0), such that
       the trigger bot can only reach the end of his row at time t,
       where t is the least such time where
       (A_i + t) mod M_i = 0
       for every i.

       The problem, then, is to compute the best i, M_i, and A_i,
       subject to the following constraints:

       i < MAX_BOTS - existing_bots      (1 is needed for the
       trigger bot as well)
       i < selection.h - 2               (3 rows for trigger bot)
       max(M_i) < selection.w            (1 needed for teleport)

       We want to minimize the solution, according to the following
       measure (lexicographically),
         - smallest i
         - smallest max(M_i)
    */

    /* There's of course a much faster way to do this, using the Chinese
       Remainder Theorem. But given how small the values of t that we'll
       encounter will be, looping makes it easier to make sure we get it
       right. */

    int M[LEVEL_MAX_WIDTH];
    int A[LEVEL_MAX_WIDTH];

    for (int i = 1; (i < LEVEL_MAX_ROBOTS - level->nbots) &&
                    i < (selection.w - 2) &&
                    i < (selection.h - 2); i++) {
      if (timer_try(M, A, 0, i, n, reverse.checked)) return;
    }

    Message::No(this, "Can't find a timer; try increasing the width,\n"
                "   " "height, and number of available bots.");

  }
}

/* compute all timers of length i where
   M[x],A[x] are fixed for 0<=x<j.
   if successful, modify the level to insert
   the timer and then return true.
*/
bool Editor::timer_try(int *M, int *A, int j, int i, int n, bool rev) {

  if (j < 0 || j > i) abort();

  if (j < i) {
    /* wlog we will assume that M_i > M_i+1, also
       M_i of 0 ill-defined, and 1 pointless,
       so start at 2. */

    for (M[j] = 2; M[j] < (j?M[j-1]:selection.w); M[j]++) {
      for (A[j] = 0; A[j] < M[j]; A[j]++) {
        if (timer_try(M, A, j + 1, i, n, rev)) return true;
      }
    }

    return false;
  } else {

    /* must be correct at time t */
    if (!timer_check(M, A, i, n)) return false;

    /* also must not be satisfied for any smaller t */
    for (int u = 0; u < n; u++) {
      if (timer_check(M, A, i, u)) return false;
    }

    /* good! */
    #if 0
    printf("okay: i=%d\n", i);
    for (int z = 0; z < i; z++) {
      printf("  M[%d] = %d, A[%d] = %d\n",
	     z, M[z], z, A[z]);
    }
    #endif
    /* write it... */

    /* black out selection */
    for (int x = selection.x; x < selection.x + selection.w; x++) {
      for (int y = selection.y; y < selection.y + selection.h; y++) {
	level->settile(x, y, T_BLACK);
	clearbot(x, y);
      }
    }

    // printf("cleared..\n");

    int dx = rev ? -1 : 1;

    int rootx = rev ? (selection.x + selection.w - 1) : selection.x;

    /* XXX could do better on trigger bot; only need
       to block it on top and bottom at the first column.
       (usually this cell is not used above because M[j] is
       strictly decreasing. */
    {
      int trigx = rootx + (dx * selection.w) - (dx * (i + 2));
      int trigy = i + 1;

      /* set up bot for trigger */
      addbot(trigx, trigy, B_DALEK);
      level->settile(trigx + dx * (i + 1), trigy, T_RPANEL);
      /* target self, I guess? */
      level->setdest(trigx + dx * (i + 1), trigy,
		     trigx + dx * (i + 1), trigy);

      for (int z = 0; z < i; z++) {
        int home = (rootx + dx * (selection.w - 1)) - (dx * M[z]);

        #if 0
        printf("home: %d\n", home);
        printf(" set %d/%d\n", selection.x + selection.w - 1,
               selection.y + z);
        #endif

        int y = selection.y + z;

        /* clear a path */
        for (int x = home; x != rootx + dx * selection.w; x += dx) {
          level->settile(x, y, T_ROUGH);
        }

        /* teleporter at far right. */
        level->settile(rootx + dx * (selection.w - 1), y, T_TRANSPORT);
        level->setdest(rootx + dx * (selection.w - 1), y,
		       home, y);

        /* make panel */
        level->settile(home, y, T_PANEL);
        level->setdest(home, y, trigx + dx * (1 + z), trigy);

        level->settile(trigx + dx * (1 + z), trigy, T_BLACK);
        level->osettile(trigx + dx * (1 + z), trigy, T_RSTEEL);
        /* do a pre-emptive swap if bot starts home, since he'll be
           on the panel */
        if (A[z] == 0)
	  level->swapo(level->index(trigx + dx * (1 + z), trigy));

        if (level->nbots >= LEVEL_MAX_ROBOTS) {
          Message::Bug(this, "oops, exceeded bots! bug!");
          return true;
        }

        addbot(home + dx * A[z], y, B_DALEK);
      }
    }

    changed = 1;
    return true;
  }
}
