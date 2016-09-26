
#ifndef __EDIT_H
#define __EDIT_H

#include "escapex.h"
#include "play.h"
#include "level.h"
#include "player.h"
#include "draw.h"

#define EDIT_DIR "mylevels"

enum { RT_MAZE, RT_MAZE2, RT_CORRIDORS, RT_MAZEBUG1, RT_MAZEBUG2,
       RT_ROOMS, RT_CRAZY, RT_RETRACT1, RT_RETRACTGOLD, NUM_RANDTYPES, };

struct Editor : public Drawable {

  /* takes ownership */
  void SetLevel(std::unique_ptr<Level> l) {
    level = std::move(l);
    dr.lev = level.get();
  }

  void Edit(const Level *origlev = nullptr);

  /* Drawable */
  void draw() override;
  void screenresize() override;

  virtual ~Editor();

  static Editor *Create(Player *p);

 private:

  void Redraw() {
    draw();
    SDL_Flip(screen);
  }

  bot currentbomb = B_BOMB_0;

  int changed = 0;

  Player *plr = nullptr;

  // We persist the solution across playing sessions while editing.
  Solution saved;

  void tmenurotate(int n);
  void saveas();
  void settitle();
  void setauthor();
  void playerstart();
  void placebot(bot);
  void sleepwake();
  void erasebot();
  void firstbot();
  void save();
  void load();
  void resize();
  void clear(tile bg, tile fg);
  void playlev();
  void prefab();
  void pftimer();
  void pffile();
  void next_bombtimer();
  bool clearbot(int x, int y);
  bool moveplayersafe();

  void FixUp();
  bool timer_try(int *, int *, int, int, int n, bool rev);
  void addbot(int x, int y, bot b);

  void dorandom();
  void fullclear(tile);

  /* stuff for ai */
  void retract1();
  bool retract_grey();
  bool retract_hole();
  bool retract_gold();

  string ainame(int a);

  void videoresize(SDL_ResizeEvent *eventp);

  void setlayer(int x, int y, int t) {
    if (layer) {
      level->osettile(x, y, t);
    } else {
      level->settile(x, y, t);
    }
  }

  int layerat(int x, int y) const {
    return layer ? level->otileat(x, y) : level->tileat(x, y);
  }

  void blinktile(int, int, Uint32);

  bool getdest(int &x, int &y, string msg);

  bool showdests = false;

  int tmenuscroll = 0;
  /* 0 = real, 1 = bizarro */
  int layer = 0;

  int olddest = 0;

  int mousex = 0, mousey = 0;

  int current = 0;

  string filename;

  // Representation invariant: dr.lev aliases
  // level.get().
  Drawing dr;
  std::unique_ptr<Level> level;
  
  int randtype = 0;
  /* if this is set, then dragging should
     not drop tiles */
  bool donotdraw = false;

  /* if w=0 or h=0, then no selection */
  SDL_Rect selection;
  void clearselection() {
    selection.w = 0;
    selection.h = 0;
    selection.x = 0;
    selection.y = 0;
  }
};

#endif
