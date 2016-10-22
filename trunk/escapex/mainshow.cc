
#include "mainshow.h"

#include <memory>

#define EXIT_FREQ 10
#define GUY_FREQ 40
#define LEVEL_FREQ 300

namespace {

struct MainShow_ : public MainShow {
  /* width and height given in tiles */
  MainShow_(int w, int h, int zf = 1) {
    level = Level::DefBoard(w, h);
    dr.lev = level.get();
    dr.width = (TILEW >> zf) * w;
    dr.height = (TILEH >> zf) * h;
    dr.margin = 0;
    dr.scrollx = 0;
    dr.scrolly = 0;
    dr.zoomfactor = zf;

    tx.reset(TextScroll::Create(fonsmall));
    tx->width = 100;
    tx->height = (TILEH >> zf) * h;

    newlevel();
  }

  /* take a step. this can be about anything */
  void step() override;

  /* draw to x,y on the supplied surface. if surface is
     0, then draw to the screen */
  void draw(int x, int y, SDL_Surface *surf = 0) override;

  int width() const override {
    return dr.width;
  }

 private:

  // Representation invariant: dr.lev aliases level
  Drawing dr;
  std::unique_ptr<Level> level;
  
  void newlevel();
  void newexit();
  void newguy();

  void trymove();
  void randomspot(int &x, int &y);

  int exitx;
  int exity;

  int leveltime;
  int exittime;
  int guytime;

  std::unique_ptr<TextScroll> tx;
};

void MainShow_::draw(int x, int y, SDL_Surface *surf) {
  if (!surf) surf = screen;
  dr.posx = x;
  dr.posy = y;
  dr.DrawLev(0, surf, true);

  tx->posx = 4 + x + (TILEW >> dr.zoomfactor) * level->w;
  tx->posy = y;

  tx->DrawTo(surf);
}

void MainShow_::step() {
  int dumb, dumby;
  dir dummy;
  if (level->isdead(dumb, dumby, dummy)) {
    tx->Say(RED "Drat!");
    newguy();
  }

  if (level->iswon()) {
    tx->Say(GREEN "Yeah!");
    newexit();
  }

  if (1 && !leveltime--) newlevel();
  if (1 && !exittime--)  newexit();
  if (1 && !guytime--)   newguy();

  trymove();

  tx->Say("");
}

void MainShow_::trymove() {
  /* walk towards goal */
  int dx = 0, dy = 0;
  if (level->guyx < exitx) dx = 1;
  else if (level->guyx > exitx) dx = -1;

  if (level->guyy < exity) dy = 1;
  else if (level->guyy > exity) dy = -1;

  if (util::random() & 1) {
    /* prefer x */
    if (dx > 0 && level->Move(DIR_RIGHT)) return;
    if (dx < 0 && level->Move(DIR_LEFT)) return;

    if (dy > 0 && level->Move(DIR_DOWN)) return;
    if (dy < 0 && level->Move(DIR_UP)) return;

  } else {
    /* prefer y */

    if (dy > 0 && level->Move(DIR_DOWN)) return;
    if (dy < 0 && level->Move(DIR_UP)) return;

    if (dx > 0 && level->Move(DIR_RIGHT)) return;
    if (dx < 0 && level->Move(DIR_LEFT)) return;
  }

  /* if we can't make any move, reset faster */
  if (guytime > 1) guytime--;
}

void MainShow_::randomspot(int &x, int &y) {
  int idx = util::random() % ((level->w - 2) * (level->h - 2));
  x = 1 + idx % (level->w - 2);
  y = 1 + idx / (level->w - 2);
}

void MainShow_::newexit() {
  level->settile(exitx, exity, T_FLOOR);

  randomspot(exitx, exity);

  level->settile(exitx, exity, T_EXIT);

  exittime = 5 + (util::random() % EXIT_FREQ);
}

void MainShow_::newguy() {
  randomspot(level->guyx, level->guyy);

  guytime = 8 + (util::random() % GUY_FREQ);
}

void MainShow_::newlevel() {
  /* XXX make a more interesting random level!!
     (we had better improve the AI, then)

     actually, the AI gets a big bonus by being
     randomly placed (and having a moving exit),
     so even if we just pulled random levels from
     the triage collection (at 18x10 or smaller),
     we'd probably win eventually. */
  for (int y = 0; y < level->h; y++) {
    for (int x = 0; x < level->w; x++) {
      level->settile(x, y, T_FLOOR);

      /* sometimes put something other than floor */
      if (!(util::random() & 127))
        level->settile(x, y, T_LASER);

      if (!(util::random() & 63))
        level->settile(x, y, T_BLUE);

      if (!(util::random() & 63))
        level->settile(x, y, T_GREY);

      if (!(util::random() & 63))
        level->settile(x, y, T_HOLE);

      if (!(util::random() & 63)) {
        level->settile(x, y, T_TRANSPORT);
        int dx = 1, dy = 1;
        randomspot(dx, dy);
        level->setdest(x, y, dx, dy);
      }


      if (x == 0 || x == (level->w - 1) ||
          y == 0 || y == (level->h - 1))
        level->settile(x, y, T_BLUE);
    }
  }

  newguy();

  /* this clears the spot where the guy lives */
  exitx = level->guyx;
  exity = level->guyy;
  newexit();

  leveltime = 30 + (util::random() % LEVEL_FREQ);
}

}  // namespace

MainShow::~MainShow() {}

std::unique_ptr<MainShow> MainShow::Create(int x, int y, int zf) {
  return std::make_unique<MainShow_>(x, y, zf);
}
