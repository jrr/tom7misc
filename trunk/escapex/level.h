#ifndef __LEVEL_H
#define __LEVEL_H

#include <string>
#include <cstring>
#include <string.h>

// n.b. Don't let this module depend on SDL. The server needs to be
// able to link it in.
#include "aevent.h"
#include "level-base.h"
#include "disamb.h"
#include "util.h"
#include "aevent.h"
#include "solution.h"

using namespace std;

template<class T> class PtrList;

inline dir turnleft(dir d) {
  switch (d) {
  case DIR_UP: return DIR_LEFT;
  case DIR_DOWN: return DIR_RIGHT;
  case DIR_RIGHT: return DIR_UP;
  case DIR_LEFT: return DIR_DOWN;
  default:
  case DIR_NONE: return DIR_NONE; /* ? */
  }
}

inline dir turnright(dir d) {
  switch (d) {
  case DIR_UP: return DIR_RIGHT;
  case DIR_DOWN: return DIR_LEFT;
  case DIR_RIGHT: return DIR_DOWN;
  case DIR_LEFT: return DIR_UP;
  default:
  case DIR_NONE: return DIR_NONE; /* ? */
  }
}

inline void dirchange(dir d, int &dx, int &dy) {
  switch (d) {
  case DIR_UP:
    dx = 0;
    dy = -1;
    break;
  case DIR_LEFT:
    dx = -1;
    dy = 0;
    break;
  case DIR_RIGHT:
    dx = 1;
    dy = 0;
    break;
  case DIR_DOWN:
    dx = 0;
    dy = 1;
    break;
  }
}

inline string dirstring(dir d) {
  switch (d) {
  case DIR_UP: return "up";
  case DIR_LEFT: return "left";
  case DIR_RIGHT: return "right";
  case DIR_DOWN: return "down";
  case DIR_NONE: return "none";
  default:
    return "??";
  }
}

inline dir dir_reverse(dir d) {
  switch (d) {
  case DIR_UP: return DIR_DOWN;
  case DIR_LEFT: return DIR_RIGHT;
  case DIR_DOWN: return DIR_UP;
  case DIR_RIGHT: return DIR_LEFT;
  default:
  case DIR_NONE: return DIR_NONE;
  }
}

/* panel colors */
enum {
  PANEL_REGULAR = 0,
  PANEL_BLUE = 1,
  PANEL_GREEN = 2,
  PANEL_RED = 3,
};

enum tflag {
  TF_NONE = 0,

  /* panel under tile (ie, pushable block) */
  /* if HASPANEL is set,
     then TF_RPANELH * 2 + TF_RPANELL
     says what kind (see panel colors above) */
  TF_HASPANEL = 1,

  TF_RPANELL = 4,
  TF_RPANELH = 8,

  /* panel under tile in bizarro world */
  /* same refinement */
  TF_OPANEL = 2,

  TF_ROPANELL = 16,
  TF_ROPANELH = 32,

  /* reserved for various purposes during
     move */
  TF_TEMP = 64,
};

enum tile {
  T_FLOOR, T_RED, T_BLUE, T_GREY, T_GREEN, T_EXIT, T_HOLE, T_GOLD,
  T_LASER, T_PANEL, T_STOP, T_RIGHT, T_LEFT, T_UP, T_DOWN, T_ROUGH,
  T_ELECTRIC, T_ON, T_OFF, T_TRANSPORT, T_BROKEN, T_LR, T_UD, T_0,
  T_1, T_NS, T_NE, T_NW, T_SE, T_SW, T_WE, T_BUTTON, T_BLIGHT,
  T_RLIGHT, T_GLIGHT, T_BLACK, T_BUP, T_BDOWN,
  T_RUP, T_RDOWN, T_GUP, T_GDOWN,
  T_BSPHERE, T_RSPHERE, T_GSPHERE, T_SPHERE,
  T_TRAP2, T_TRAP1,

  T_BPANEL, T_RPANEL, T_GPANEL,

  T_STEEL, T_BSTEEL, T_RSTEEL, T_GSTEEL,

  T_HEARTFRAMER, T_SLEEPINGDOOR,

  T_TRANSPONDER, T_NSWE, T_REMOTE,

  /*
  T_DIRRIGHT, T_DIRUP, T_DIRDOWN, T_DIRLEFT,
  */

  NUM_TILES,
};

struct Level {
  string title;
  string author;

  int w;
  int h;

  int guyx;
  int guyy;
  dir guyd;

  /* robots */
  int nbots;
  /* locations (as indices) */
  int *boti;
  /* bot type */
  bot *bott;
  /* not saved with file; just presentational. putting the player
     direction in drawing just barely works; it should probably
     be here, too. */
  dir *botd;
  /* not presentational, but also not saved with the file;
     intialized to -1 on load. (e.g., current bomb timers) */
  int *bota;

  /* shown */
  int *tiles;
  /* "other" (tiles swapped into bizarro world by panels) */
  int *otiles;
  /* destinations for transporters and panels (as index into tiles) */
  int *dests;
  /* has a panel (under a pushable block)? etc. */
  int *flags;

  /* true if corrupted on load. never saved */
  bool corrupted;

  bool iscorrupted() {
    return corrupted;
  }

  /* go straight to the target. no animation */
  void warp(int &entx, int &enty, int targx, int targy) {
    int target = tileat(targx, targy);

    CheckStepOff(entx, enty);
    /* XXX can be incorrect --
       need to use SETENTPOS, since we need to reflect
       this immediately in the boti array
       (so that serialup knows where the bot is standing) */
    entx = targx;
    enty = targy;

    switch (target) {
    case T_PANEL:
      swapo(destat(targx,targy));
      break;
    default:;
    }
  }

  void where(int idx, int &x, int &y) {
    x = idx % w;
    y = idx / w;
  }

  int index(int x, int y) {
    return (y * w) + x;
  }

  int tileat(int x, int y) {
    return tiles[y * w + x];
  }

  int otileat(int x, int y) {
    return otiles[y * w + x];
  }

  void settile(int x, int y, int t) {
    tiles[y * w + x] = t;
  }

  void osettile(int x, int y, int t) {
    otiles[y * w + x] = t;
  }

  void setdest(int x, int y, int xd, int yd) {
    dests[y * w + x] = yd * w + xd;
  }

  int destat(int x, int y) {
    return dests[y * w + x];
  }


  void getdest(int x, int y, int &xd, int &yd) {
    xd = dests[y * w + x] % w;
    yd = dests[y * w + x] / w;
  }

  void swapo(int idx);

  int flagat(int x, int y) {
    return flags[y * w + x];
  }

  void setflag(int x, int y, int f) {
    flags[y * w + x] = f;
  }

  bool iswon() {
    return tileat(guyx, guyy) == T_EXIT;
  }

  bool travel(int x, int y, dir d, int &nx, int &ny) {
    switch (d) {
      /* sometimes useful, for instance looping over all
	 affected tiles when bombing */
    case DIR_NONE:
      nx = x;
      ny = y;
      return true;
    case DIR_UP:
      if (y == 0) return false;
      nx = x;
      ny = y - 1;
      break;
    case DIR_DOWN:
      if (y == (h - 1)) return false;
      nx = x;
      ny = y + 1;
      break;
    case DIR_LEFT:
      if (x == 0) return false;
      nx = x - 1;
      ny = y;
      break;
    case DIR_RIGHT:
      if (x == (w - 1)) return false;
      nx = x + 1;
      ny = y;
      break;
    default: return false; /* ?? */
    }
    return true;
  }

  /* shot by laser at (tilex, tiley) in direction (dir),
     or standing on a non-deleted bot. */
  bool isdead(int &tilex, int &tiley, dir &d);

  /* returns true if move had effect. */
  bool Move(dir d);

  /* see animation.h for documentation */
  bool MoveAnimate(dir d, Disamb *ctx, PtrList<aevent> *&events);

  /* create clone of current state. */
  Level *clone() const;

  /* writes current state into a string */
  string tostring();

  /* null on error. if allow_corrupted is true, it returns a
     valid level with as much data from the original as
     possible (but may still return null) */
  static Level *fromstring(string s, bool allow_corrupted = false);

  static Level *blank(int w, int h);
  static Level *defboard(int w, int h);

  /* correct a level (bad tiles, bad destinations, overlapping
     bots, etc.) for saving or loading. In-progress levels
     (blocks on panels, lit bombs, etc.) are not considered
     sane; only levels that one could create in the editor.
     returns true if the level was already sane. */
  bool sanitize();

  /* Rearrange the bots in place to be in canonical order
     (bombs must be last). */
  void fixup_botorder();

  void destroy();

  /* play to see if it wins, does not modify level or sol */
  static bool Verify(const Level *lev, const Solution &s);
  /* Replaces out with a simplified solution, if s solves lev
     somewhere along the way. If the return is false, then out is in
     an unspecified state. */
  static bool VerifyPrefix(const Level *lev, const Solution &s,
			    Solution *out);

  /* execute solution. returns early (# moves set in moves)
     if we die (return false) or win (return true). false upon
     completing without winning or dying. */
  bool Play(const Solution &s, int &moves);
  /* only 'length' moves of the solution, starting from move 'start' 
     length must be <= s.Length(). */
  // XXX2016 was play_subsol
  bool PlayPrefix(const Solution &s, int &moves, int start, int length);

  void resize(int neww, int newh);

  static bool issphere(int t);
  static bool issteel(int t);
  static bool ispanel(int t);
  static bool triggers(int tile, int panel);
  static bool allowbeam(int tile);

  /* return the lowest index bot at a specific location
     (if there's one there). We count B_DELETED and B_BOMB_X
     as not bots. */
  bool botat(int x, int y, int &i) {
    int z = index(x, y);
    for (int m = 0; m < nbots; m++) {
      if (boti[m] == z &&
	  bott[m] != B_DELETED &&
	  bott[m] != B_BOMB_X) {
	i = m;
	return true;
      }
    }
    return false;
  }

  bool isconnected(int x, int y, dir d);

  bool botat(int x, int y) {
    int dummy;
    return botat(x, y, dummy);
  }

  bool playerat(int x, int y) {
    return (x == guyx) && (y == guyy);
  }

  static bool isbomb(bot b) {
    return ((int)b >= (int)B_BOMB_0 &&
	    (int)b <= (int)B_BOMB_MAX);
  }

  /* pre: b is bomb */
  static int bombmaxfuse(bot b) {
    return (int)b - (int)B_BOMB_0;
  }

  static bool ispanel(int t, int &ref) {
    if (t == T_PANEL) { ref = PANEL_REGULAR; return true; }
    if (t == T_BPANEL) { ref = PANEL_BLUE; return true; }
    if (t == T_GPANEL) { ref = PANEL_GREEN; return true; }
    if (t == T_RPANEL) { ref = PANEL_RED; return true; }
    return false;
  }

  static bool needsdest(int t) {
    int dummy;
    return (t == T_REMOTE || t == T_TRANSPORT || ispanel(t, dummy));
  }

 private:

  void CheckStepOff(int x, int y);
  void ClearMap();
  void clearflag(int fl) {
    for (int i = 0; i < w * h; i++) {
      flags[i] &= ~fl;
    }
  }

  /* is the tile bombable? */
  static bool Bombable(int t) {
    switch (t) {
      /* some level of danger */
    case T_EXIT:
    case T_SLEEPINGDOOR:
      /* useful */
    case T_LASER:

      /* obvious */
    case T_BROKEN:
    case T_GREY:
    case T_RED:
    case T_GREEN:

      /* a soft metal. ;) */
    case T_GOLD:

    case T_NS:
    case T_WE:
    case T_NW:
    case T_NE:
    case T_SW:
    case T_SE:
    case T_NSWE:
    case T_TRANSPONDER:
    case T_BUTTON:
    case T_BLIGHT:
    case T_GLIGHT:
    case T_RLIGHT:
    case T_REMOTE:

      /* ?? sure? */
    case T_BLUE:
      /* don't want walls made of this
	 ugly thing */
    case T_STOP:

      /* but doesn't count as picking it up */
    case T_HEARTFRAMER:


      /* ?? easier */
    case T_PANEL:
    case T_RPANEL:
    case T_GPANEL:
    case T_BPANEL:

      return true;

    case T_FLOOR:

      /* obvious */
    case T_HOLE:
    case T_ELECTRIC:
    case T_BLACK:
    case T_ROUGH:

      /* for symmetry with holes.
	 maybe could become holes,
	 but that is just more
	 complicated */
    case T_TRAP1:
    case T_TRAP2:


      /* useful for level designers */
    case T_LEFT:
    case T_RIGHT:
    case T_UP:
    case T_DOWN:

      /* Seems sturdy */
    case T_TRANSPORT:
    case T_ON:
    case T_OFF:
    case T_1:
    case T_0:

      /* made of metal */
    case T_STEEL:
    case T_BSTEEL:
    case T_RSTEEL:
    case T_GSTEEL:

    case T_LR:
    case T_UD:

    case T_SPHERE:
    case T_BSPHERE:
    case T_RSPHERE:
    case T_GSPHERE:

      /* shouldn't bomb the floorlike things,
	 so also their 'up' counterparts */
    case T_BUP:
    case T_BDOWN:
    case T_GUP:
    case T_GDOWN:
    case T_RUP:
    case T_RDOWN:

      return false;
    }

    /* illegal tile */
    abort();
    return false;
  }

  bool hasframers() {
    for (int i = 0; i < w * h; i++) {
      if (tiles[i] == T_HEARTFRAMER) return true;
    }
    return false;
  }

  void SetEntDir(int enti, dir d) {
    if (enti == B_PLAYER) {
      guyd = d;
    } else {
      botd[enti] = d;
    }
  }

  void SetEntPos(int enti, int x, int y) {
    if (enti == B_PLAYER) {
      guyx = x;           
      guyy = y;           
    } else {               
      boti[enti] = index(x, y);
    }
  }
  
  using AList = PtrList<aevent>;

  template<bool ANIMATING, class DAB>
  void PostAnimate(DAB *ctx, AList *&events, AList **&etail);
  
  /* pass the entity index, or -1 for the player */
  template<bool ANIMATING, class DAB>
  bool MoveEnt(dir d, int enti, Capabilities cap,
	       int entx, int enty,
	       DAB *ctx, AList *&events, AList **&etail);
  
  template<bool ANIMATING, class DAB>
  void Bombsplode(int now,
		  int bombi, DAB *ctx, AList *&events,
		  AList **& etail);

  // n.b. Not all of these have the same signature!
  
  template<bool ANIMATING, class DAB>
  bool MoveEntTransport(dir d, int enti, Capabilities cap,
			int entx, int enty, int newx, int newy,
			DAB *ctx, AList *&events,
			AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntExit(dir d, int enti, Capabilities cap,
		   int entx, int enty, int newx, int newy,
		   DAB *ctx, AList *&events,
		   AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntOn(dir d, int enti, Capabilities cap,
		 int entx, int enty, int newx, int newy,
		 DAB *ctx, AList *&events,
		 AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntGoldlike(int target, dir d, int enti, Capabilities cap,
		       int entx, int enty, int newx, int newy,
		       DAB *ctx, AList *&events,
		       AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntButton(dir d, int enti, Capabilities cap,
		     int entx, int enty, int newx, int newy,
		     DAB *ctx, AList *&events,
		     AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntHeartframer(dir d, int enti, Capabilities cap,
			  int entx, int enty, int newx, int newy,
			  DAB *ctx, AList *&events,
			  AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntElectric(dir d, int enti, Capabilities cap,
		       int entx, int enty, int newx, int newy,
		       DAB *ctx, AList *&events,
		       AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntBroken(dir d, int enti, Capabilities cap,
		     int entx, int enty, int newx, int newy,
		     DAB *ctx, AList *&events,
		     AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntGreen(dir d, int enti, Capabilities cap,
		    int entx, int enty, int newx, int newy,
		    DAB *ctx, AList *&events,
		    AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntFloorlike(int target, dir d, int enti, Capabilities cap,
			int entx, int enty, int newx, int newy,
			DAB *ctx, AList *&events,
			AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntSteel(int target, dir d, int enti, Capabilities cap,
		    int entx, int enty, int newx, int newy,
		    DAB *ctx, AList *&events,
		    AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEnt01(int target, dir d, int enti, Capabilities cap,
		 int entx, int enty, int newx, int newy,
		 DAB *ctx, AList *&events,
		 AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveEntPushable(int target, dir d, int enti, Capabilities cap,
		       int entx, int enty, int newx, int newy,
		       DAB *ctx, AList *&events,
		       AList **&etail);

  template<bool ANIMATING, class DAB>
  bool MoveMaybeAnimate(dir d, DAB *ctx, AList *&events, AList **&etail);
};

#endif
