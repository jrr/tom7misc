#include "edit.h"

#include <math.h>
#include <time.h>

#include "SDL.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/lines.h"
#include "../cc-lib/crypt/md5.h"

#include "draw.h"
#include "escapex.h"
#include "play.h"
#include "prompt.h"
#include "escape-util.h"
#include "loadlevel.h"
#include "message.h"
#include "menu.h"
#include "chars.h"

#define EDITORBGCOLOR 0x11, 0x11, 0x22, 0xFF
#define SELRECTCOLOR 0x76, 0x12, 0xAA
#define SELRECTCOLOR2 0xAA, 0x40, 0xFF

#define LEVYSKIP 12

#define XOFF 0
#define YOFF (TILEH * 2 + LEVYSKIP)

#define XW (screen->w - XOFF)
#define YH (screen->h - YOFF)

static constexpr int edit_menuitem[] = {
  0 /* skip - show foreground tile */,
  0 /* skip - layer normal/alt */,
  0 /* skip - changed/not */,
  TU_SAVE, TU_SAVEAS, TU_LOAD,
  TU_TITLE, TU_AUTHOR, TU_SIZE,
  TU_PLAYERSTART, TU_CLEAR,
  TU_PLAY, TU_RANDOM,
  TU_RANDTYPE,
  TU_ERASE_BOT,
  TU_FIRST_BOT,
  TU_DALEK,
  TU_HUGBOT,
  TU_BROKEN,
  TU_BOMB,
  TU_BOMBTIMER,
  TU_SLEEPWAKE,
  TU_PREFAB,
};

static constexpr int tileorder[] = {
  T_FLOOR, T_ROUGH, T_EXIT, T_BLUE, T_RED, T_GREY, T_GREEN,
  T_GOLD,
  T_BLACK,
  T_TRAP2, T_TRAP1, T_HOLE,
  T_LASER, T_PANEL,
  T_BPANEL, T_RPANEL, T_GPANEL,
  T_STOP, T_RIGHT, T_LEFT, T_UP, T_DOWN,
  T_ELECTRIC, T_ON, T_OFF, T_TRANSPORT, T_BROKEN, T_LR, T_UD,
  T_0, T_1,
  T_BUTTON,
  T_NS, T_NE, T_NW, T_SE, T_SW, T_WE, T_NSWE,
  T_TRANSPONDER,

  T_REMOTE,
  T_BLIGHT, T_RLIGHT, T_GLIGHT, T_BUP, T_BDOWN,
  T_RUP, T_RDOWN, T_GUP, T_GDOWN,

  T_BSPHERE, T_RSPHERE, T_GSPHERE, T_SPHERE,
  T_STEEL, T_BSTEEL, T_RSTEEL, T_GSTEEL,

  T_HEARTFRAMER, T_SLEEPINGDOOR,
};

#define NTILEORDER ((int)(sizeof (tileorder) / sizeof (int)))

#define POS_CURRENT 0
#define POS_LAYER 1
#define POS_CHANGED 2
#define POS_RANDTYPE 13
#define POS_SAVE 3
#define POS_RANDOM 12
#define POS_ERASE_BOT 14
#define POS_FIRST_BOT 15
#define POS_BOMBTIMER 20
#define POS_SLEEPWAKE 21
#define POS_PREFAB 22
#define NUM_MENUITEMS 23

void Editor::ScreenResize() {
  dr.width = XW;
  dr.height = YH;

  /* XXX not sure how to recalculate this ... */
  /* perhaps scroll so that the foreground tile is
     visible */
  tmenuscroll = 0;
}

void Editor::FullClear(Tile t) {
  for (int x = 0; x < level->w; x++) {
    for (int y = 0; y < level->h; y++) {
      level->settile(x, y, t);
    }
  }
  level->nbots = 0;
}

/* XXX limit to selection */
void Editor::Clear(Tile bg, Tile fg) {
  if (changed &&
      !Message::Quick(this,
                      "Clearing will destroy your unsaved changes.",
                      "Clear anyway",
                      "Don't clear")) {
    Redraw();
    return;
  }

  /* don't allow fill with panels or transporters */
  if (Level::needsdest(bg)) bg = T_FLOOR;
  if (Level::needsdest(fg)) fg = T_BLUE;

  FullClear(bg);
  for (int x = 0; x < level->w; x++) {
    level->settile(x, 0, fg);
    level->settile(x, level->h - 1, fg);
  }
  for (int y = 0; y < level->h; y++) {
    level->settile(0, y, fg);
    level->settile(level->w - 1, y, fg);
  }

  changed = 0;
  Redraw();
}

/* for debugging, since it pauses everything */
void Editor::blinktile(int destx, int desty, Uint32 color) {
  int xx, yy;
  if (dr.OnScreen(destx, desty, xx, yy)) {
    SDL_Rect dst;
    dst.x = xx;
    dst.y = yy;
    dst.w = TILEW;
    dst.h = TILEH;
    SDL_FillRect(screen, &dst, color);
  }
  SDL_Flip(screen);
  SDL_Delay(500);
}

void Editor::Draw() {

  /* check if we need to highlight a destination */
  int tx, ty;
  int sdx = -1, sdy = -1;
  if (dr.InMap(mousex, mousey, tx, ty)) {
    if (Level::needsdest(layerat(tx, ty))) {
      level->getdest(tx, ty, sdx, sdy);
    }
  }

  sdlutil::ClearSurface(screen, EDITORBGCOLOR);
  /* draw black for menu */
  {
    SDL_Rect dst;
    dst.x = 0;
    dst.y = 0;
    dst.w = screen->w;
    dst.h = TILEH * 2;
    SDL_FillRect(screen, &dst, BGCOLOR);
  }

  int showw = (screen->w / TILEW) - 1;

  /* draw menu */

  /* could be showw + 1 */
  for (int j = 0; j < showw; j++) {

    if (j == POS_CURRENT) {
      dr.DrawTile(j * TILEW, 0, current, 0);
    } else if (j == POS_LAYER) {
      Drawing::DrawTileU(j * TILEW, 0, layer ? TU_LAYERALT : TU_LAYERNORMAL, 0);
    } else if (j == POS_CHANGED) {
      if (changed) Drawing::DrawTileU(j * TILEW, 0, TU_CHANGED, 0);
    } else if (j < NUM_MENUITEMS && edit_menuitem[j]) {
      Drawing::DrawTileU(j * TILEW, 0, edit_menuitem[j], 0);
    }

    /* draw extra info */
    if (j == POS_RANDTYPE) {
      fon->draw(j * TILEW + 14, 12, GREY + itos(randtype) + POP);
    } else if (j == POS_BOMBTIMER) {
      fon->draw(j * TILEW + 14, 12,
                GREY + itos((int)currentbomb - (int)B_BOMB_0) + POP);
    }
  }

  /* disable menu items where appropriate */

  if (filename == "") {
    Drawing::DrawTileU(POS_SAVE * TILEW, 0, TU_DISABLED, 0);
  }

  if (!selection.w) {
    Drawing::DrawTileU(POS_PREFAB * TILEW, 0, TU_DISABLED, 0);
  }

  if (!level->nbots) {
    Drawing::DrawTileU(POS_FIRST_BOT * TILEW, 0, TU_DISABLED, 0);
    Drawing::DrawTileU(POS_ERASE_BOT * TILEW, 0, TU_DISABLED, 0);
    Drawing::DrawTileU(POS_SLEEPWAKE * TILEW, 0, TU_DISABLED, 0);
  }

  /* draw tile menu */
  int i;
  for (i = 0; i < showw; i++) {
    int tt = i + (showw * tmenuscroll);
    if (tt < NTILEORDER)
      dr.DrawTile(i * TILEW, TILEH, tileorder[tt], 0);
  }

  Drawing::DrawTileU(i * TILEW, TILEH, TU_TILESUD, 0);

  /* always point him down. */
  dr.DrawLev(layer);
  /* ? */
  dr.DrawExtra();

  /* always draw bot numbers */
  dr.DrawBotNums();

  /* draw bomb timers */
  if (!dr.zoomfactor) {
    for (int b = 0; b < level->nbots; b++) {
      if (Level::isbomb(level->bott[b])) {
        int bx, by;
        level->where(level->boti[b], bx, by);
        int bsx, bsy;
        if (dr.OnScreen(bx, by, bsx, bsy)) {
          string ss = RED + itos(Level::bombmaxfuse(level->bott[b]));
          fon->draw(bsx + ((TILEW - fon->sizex(ss))>>1),
                    bsy + ((TILEH - fon->height)>>1),
                    ss);
        }
      }
    }
  }

  /* draw destination, if it exists */
  /* XX maybe not if showdests? */
  {
    int px, py;
    if (sdx >= 0 && dr.OnScreen(sdx, sdy, px, py)) {
      Drawing::DrawTileU(px, py, TU_TARGET, dr.zoomfactor);
    }
  }

  /* could be better at stacking arrows so that multiple ones
     on the same line could be distinguished */
  if (showdests) {
    dr.DrawDests();
  }

  /* draw selection rectangle, if it exists */
  /* XXX this should allow the rectangle to be partially
     off-screen */
  if (selection.w > 0 &&
      selection.h > 0) {

    int px, py, pdx, pdy;
    if (dr.OnScreen(selection.x, selection.y, px, py) &&
        dr.OnScreen(selection.x + selection.w - 1,
                    selection.y + selection.h - 1,
                    pdx, pdy)) {

      pdx += (TILEW >> dr.zoomfactor) - 1;
      pdy += (TILEH >> dr.zoomfactor) - 1;

      sdlutil::drawline(screen, px, py, px, pdy, SELRECTCOLOR);
      sdlutil::drawline(screen, px, py, pdx, py, SELRECTCOLOR);
      sdlutil::drawline(screen, pdx, py, pdx, pdy, SELRECTCOLOR);
      sdlutil::drawline(screen, px, pdy, pdx, pdy, SELRECTCOLOR);


      sdlutil::drawline(screen, px + 1, py + 1, px + 1, pdy - 1,
                        SELRECTCOLOR2);
      sdlutil::drawline(screen, px + 1, py + 1, pdx - 1, py + 1,
                        SELRECTCOLOR2);
      sdlutil::drawline(screen, pdx - 1, py + 1, pdx - 1, pdy - 1,
                        SELRECTCOLOR2);
      sdlutil::drawline(screen, px + 1, pdy - 1, pdx - 1, pdy - 1,
                        SELRECTCOLOR2);



      sdlutil::drawline(screen, px + 2, py + 2, px + 2, pdy - 2,
                        SELRECTCOLOR);
      sdlutil::drawline(screen, px + 2, py + 2, pdx - 2, py + 2,
                        SELRECTCOLOR);
      sdlutil::drawline(screen, pdx - 2, py + 2, pdx - 2, pdy - 2,
                        SELRECTCOLOR);
      sdlutil::drawline(screen, px + 2, pdy - 2, pdx - 2, pdy - 2,
                        SELRECTCOLOR);


    }
  }

  /* coordinates of scroll */
  if (dr.scrollx > 0 ||
      dr.scrolly > 0) {

    fonsmall->draw(dr.posx + dr.margin + 1,
                   dr.posy + dr.margin - fonsmall->height,
                   itos(dr.scrollx) + ", " +
                   itos(dr.scrolly));
  }

}

void Editor::tmenurotate(int n) {

  tmenuscroll += n;

  int showw = (screen->w / TILEW) - 1;

  if (tmenuscroll * showw >= NTILEORDER) tmenuscroll = 0;

  /* largest possible */
  if (tmenuscroll < 0) {
    int tt = 0;
    while ((tt * showw) < NTILEORDER) {
      tt++;
    }
    tmenuscroll = tt - 1;
  }

  Redraw();
}

void Editor::settitle() {
  string nt = Prompt::ask(this, "Title for level: ",
                          level->title);

  level->title = nt;

  if (level->title.empty()) level->title = "Untitled";


  changed = 1;
  Redraw();
}

void Editor::setauthor() {

  string s = Prompt::ask(this,
			 "Author of level: ",
			 level->author);
  if (!s.empty()) level->author = s;

  changed = 1;
  Redraw();
}

/* XXX warn if saving into managed directory */
void Editor::saveas() {

  string fn;
  if (filename == "") fn = (string)EDIT_DIR + (string) DIRSEP;
  else                fn = filename;

  string nfn = Prompt::ask(this, "Filename: ", fn);

  /* if cancelled, don't do anything */
  if (nfn == "") {
    Redraw();
    return;
  }

  /* if level is untitled, get the title from this */
  if (level->title == "Untitled")
    level->title = EscapeUtil::fileof(nfn);

  filename = EscapeUtil::ensureext(nfn, ".esx");

  save();
}

void Editor::PlayerStart() {
  ClearSelection();

  int x, y;

  if (getdest(x, y, "Click to choose player start.")) {
    if (!level->botat(x, y)) {
      level->guyx = x;
      level->guyy = y;
      changed = 1;
    } else {
      Message::No(this, "Can't put player on bots!");
    }
  }

  Redraw();
}

/* XXX target selection? */
void Editor::erasebot() {
  ClearSelection();

  int x, y;
  if (getdest(x, y, (string)"Click on bot to erase.")) {

    if (!clearbot(x, y))
      dr.message = "No bot there!";

    changed = 1;
  }

  level->fixup_botorder();
  Redraw();
}

bool Editor::clearbot(int x, int y) {

    int i;
    if (level->botat(x, y, i)) {
      /* reduce number of bots and shift every one bigger
         than this one down */
      level->nbots--;
      for (int m = i; m < level->nbots; m++) {
        level->bott[m] = level->bott[m + 1];
        level->boti[m] = level->boti[m + 1];
        level->botd[m] = level->botd[m + 1];
        level->bota[m] = level->bota[m + 1];
      }
      /* not necessary to make arrays smaller. */
      return true;
    } else return false;
}

void Editor::firstbot() {
  int x, y;
  if (getdest(x, y, (string)"click on bot to make #1")) {

    int i;
    if (level->botat(x, y, i)) {
      std::unique_ptr<Level> l = level->Clone();

      level->bott[0] = l->bott[i];
      level->botd[0] = l->botd[i];
      level->boti[0] = l->boti[i];
      level->bota[0] = l->bota[i];

      /* now copy rest */
      int r = 1;
      for (int b = 0; b < l->nbots; b++) {
        if (b != i) {
          level->bott[r] = l->bott[b];
          level->botd[r] = l->botd[b];
          level->boti[r] = l->boti[b];
          level->bota[r] = l->bota[b];
          r++;
        }
      }
    } else dr.message = "No bot there!";
  }
  Redraw();
}

void Editor::sleepwake() {
  int x, y;
  if (getdest(x, y, (string)"click on bot sleep/wake")) {

    int i;
    if (level->botat(x, y, i)) {

      bot old = level->bott[i];
      bot dest = B_BROKEN;
      switch (old) {
      default:
        if (Level::isbomb(old)) {
          dr.message = RED "Can't sleep/wake bombs\n";
          Redraw();
          return;
        } else {
          Message::Bug(this, "sleep/wake not implemented for this bot");
          Redraw();
          return;
        }
      case B_BROKEN:
        dr.message = RED "Can't sleep/wake broken bots\n";
        Redraw();
        return;
      case B_DALEK: dest = B_DALEK_ASLEEP; break;
      case B_DALEK_ASLEEP: dest = B_DALEK; break;
      case B_HUGBOT: dest = B_HUGBOT_ASLEEP; break;
      case B_HUGBOT_ASLEEP: dest = B_HUGBOT; break;
      }

      level->bott[i] = dest;

    } else dr.message = "No bot there!";
  }

  level->fixup_botorder();
  Redraw();
}

void Editor::placebot(bot b) {
  ClearSelection();

  int x, y;

  string msg =
    (level->nbots < LEVEL_MAX_ROBOTS) ?
    ("Click to place bot of type " + itos((int)b)) :
    (RED "Out of bots!" POP " -- click existing bot to change type.");

  if (getdest(x, y, msg)) {

    int ai;
    if (level->playerat(x, y)) {
      Message::No(this, "Can't put a bot on the player!");

    } else if (level->botat(x, y, ai)) {

      level->bott[ai] = b;
      changed = 1;

    } else {
      if (level->nbots < LEVEL_MAX_ROBOTS) {
        addbot(x, y, b);
      } else {
        Message::No(this,
                    "Maximum robots (" + itos(LEVEL_MAX_ROBOTS) +
                    ") reached!");
        Redraw();
      }
    }
  }

  Redraw();
}

/* must ensure there is space, and that
   there is not a bot or player on this spot already */
void Editor::addbot(int x, int y, bot b) {
  int n = level->nbots + 1;

  /* all clear; add new one */
  int *ni = (int*)malloc(sizeof(int) * n);
  bot *nt = (bot*)malloc(sizeof(bot) * n);

  for (int i = 0; i < level->nbots; i++) {
    ni[i] = level->boti[i];
    nt[i] = level->bott[i];
  }

  ni[n - 1] = level->index(x, y);
  nt[n - 1] = b;
  free(level->boti);
  free(level->bott);
  level->boti = ni;
  level->bott = nt;
  level->nbots = n;

  /* need to update directions, too */
  free(level->botd);
  level->botd = (dir*)malloc(sizeof (dir) * n);
  for (int z = 0; z < n; z++) level->botd[z] = DIR_DOWN;


  /* attribs always -1 except when playing */
  free(level->bota);
  level->bota = (int*)malloc(sizeof (int) * n);
  for (int z = 0; z < n; z++) level->bota[z] = -1;

  level->fixup_botorder();
  changed = 1;
}

void Editor::save() {
  ClearSelection();

  FixUp();

  if (!filename.empty()) {
    string old = readfile(filename);

    string nstr = level->tostring();

    if (writefile(filename, nstr)) {
      dr.message = "Wrote " GREEN + filename + POP;
    } else {
      dr.message = RED "error writing to " + filename + POP;
      filename = "";
    }

    if (!old.empty()) {
      /* if player has solution for the
         level existing in the file that we're
         overwriting, try it out on the new
         file. (We may just be changing something
         cosmetic!) */

      /* XXX should keep around a list of
         candidate md5s. We get these whenever
         we play or save. */

      const string omd5 = MD5::Hash(old);
      const string nmd5 = MD5::Hash(nstr);
      /* only try if level changed */
      if (omd5 != nmd5) {

        int rs = 0, rb = 0;
        for (const NamedSolution &ns : plr->SolutionSet(omd5)) {
          if (!ns.bookmark &&
              Level::Verify(level.get(), ns.sol)) {
            string name = ns.name;
            if (name.find("(recovered)") == string::npos)
              name = (string)"(recovered) " + name;
            /* It still works! */
            NamedSolution ns2(ns.sol, name, ns.author, time(0), false);
            plr->AddSolution(nmd5, std::move(ns2), true);
            rs++;
          } else if (ns.bookmark) {
            plr->AddSolution(nmd5, ns, true);
            rb++;
          }
        }

        if (rs + rb > 0) {
          string s = rs?((string)YELLOW + itos(rs) + POP " solution"
                         + ((rs == 1) ? (string)"" : (string)"s")
                         + (rb ? (string)", " : (string)"")
                         ) : "";
          string b = rb?((string)YELLOW + itos(rb) + POP " bookmark"
                         + ((rb == 1) ? (string)"" : (string)"s")
                         ) : "";
          dr.message += (string)ALPHA50 " (" BLUE "recovered " + s + b +
                        (string)POP ")" POP;
          plr->WriteFile();
        }

      } else {
        dr.message += (string)" again " ALPHA50 GREY "(" +
           MD5::Ascii(nmd5) + ")" POP POP;
      }
    }

    /* on success, we clear changed flag */
    changed = 0;

  } else {
    Message::Bug(this,
                 "shouldn't be able to save with empty filename");
  }

  Redraw();
}

/* XXX should warn if you load from a managed directory. */
/* target selection? */
void Editor::load() {
  ClearSelection();

  std::unique_ptr<LoadLevel> ll{
    LoadLevel::Create(plr, EDIT_DIR, true, true)};
  if (!ll.get()) {
    Message::Quick(this, "Can't open load screen!",
                   "Ut oh.", "", PICS XICON POP);
    Redraw();
    return;
  }
  string res = ll->SelectLevel();
  string ss = readfile(res);

  /* allow corrupted files */
  std::unique_ptr<Level> l = Level::FromString(ss, true);

  if (l.get() != nullptr) {
    SetLevel(std::move(l));
    filename = res;
    dr.message = ((string)"Loaded " + filename);

    FixUp();
    changed = 0;
  } else {
    dr.message = ((string) RED "error loading " + res + POP);
  }

  Redraw();
}

Editor::~Editor() {}

void Editor::playlev() {
  /* XXX check result for 'exit' */
  FixUp();

  std::unique_ptr<Play> pla{Play::Create(level.get())};

  /* grab md5 in case player makes bookmarks */
  string md5 = MD5::Hash(level->tostring());

  /* PlayResult res = */
  (void)pla->DoPlaySave(plr, &saved, md5);

  /* has a different loop; might have resized */
  ScreenResize();
  Redraw();
}

void Editor::Resize() {
  ClearSelection();

  string nw = itos(level->w);
  string nh = itos(level->h);

  TextInput twidth;
  twidth.question = "Width";
  twidth.input = nw;
  twidth.explanation =
    "Width of the level in tiles.";

  TextInput theight;
  theight.question = "Height";
  theight.input = nh;
  theight.explanation =
    "Height of the level in tiles.";

  Okay ok;
  ok.text = "Change Size";

  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &theight);
  PtrList<MenuItem>::push(l, &twidth);

  std::unique_ptr<Menu> mm = Menu::Create(this, "Level Size", l, false);

  if (mm->menuize() == InputResultKind::OK) {
    int nnw = atoi(twidth.input.c_str());
    int nnh = atoi(theight.input.c_str());

    if (nnw > 0 && nnw <= LEVEL_MAX_WIDTH &&
        nnh > 0 && nnh <= LEVEL_MAX_HEIGHT &&
        (nnw * nnh <= LEVEL_MAX_AREA)) {
      level->resize(nnw, nnh);

      /* reset scroll position */
      dr.scrollx = 0;
      dr.scrolly = 0;

      changed = 1;
    } else {
      Message::Quick(this,
                     "Size too large/small",
                     "Sorry", "");

    }
  } /* XXX else InputResultKind::QUIT */

  PtrList<MenuItem>::diminish(l);

  Redraw();
}

void Editor::Edit(const Level *origlev) {
  if (origlev != nullptr) {
    SetLevel(origlev->Clone());
  } else {
    SetLevel(Level::DefBoard(18, 10));
  }

  dr.scrollx = 0;
  dr.scrolly = 0;
  dr.posx = XOFF;
  dr.posy = YOFF;
  dr.width = XW;
  dr.height = YH;
  dr.margin = 12;

  ClearSelection();

  olddest = -1;

  mousex = 0;
  mousey = 0;
  changed = 0;

  SDL_Event event;

  dr.message = "";

  saved.Clear();

  FixUp();

  Redraw();

  for (;;) {
    while (SDL_PollEvent(&event)) {
      if (HandleVideoEvent(this, event)) continue;

      switch (event.type) {

      case SDL_MOUSEMOTION: {
        /* some more efficient way to do this?? */
        /* we only need to redraw if something
           has changed like mousing over a tile
           with a destination */

        SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;

        /* we do a lot of stuff here. set this flag if
           we need to redraw at the end. */
        int yesdraw = 0;

        int omx = mousex;
        int omy = mousey;

        mousex = e->x;
        mousey = e->y;

        /* if mouse down, draw line of tiles.
           don't do it if they need destinations. */
        int otx, oty;
        int ntx, nty;

        /* XXX how do I set this? */
        bool ctrl_held = false;

        /* right mouse button, or ctrl held ... */
        if (((e->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) ||
             ((e->state & SDL_BUTTON(SDL_BUTTON_LEFT)) &&
              ctrl_held)) &&
            selection.w > 0 &&
            dr.InMap(mousex, mousey, ntx, nty)) {

          /* change selection rectangle */
          int nw = 1 + (ntx - selection.x);
          int nh = 1 + (nty - selection.y);

          if (nw != selection.w ||
              nh != selection.h) {

            if (nw <= 0 || nh <= 0) { nw = nh = 0; }

            selection.w = nw;
            selection.h = nh;
            Redraw();
          }

          /* left mouse button ... */
          /* XXX if I start in the map, then I should
             always draw to the edge, even if the end
             is not in the map */
        } else if (!Level::needsdest(current) &&
                   !donotdraw &&
                   e->state & SDL_BUTTON(SDL_BUTTON_LEFT) &&
                   dr.InMap(omx, omy, otx, oty) &&
                   dr.InMap(mousex, mousey, ntx, nty)) {

          if (otx != ntx ||
              oty != nty) {
            /* draw line. */
	    for (const std::pair<int, int> point :
		   Line<int>{otx, oty, ntx, nty}) {
	      int cx = point.first, cy = point.second;
	      setlayer(cx, cy, current);
	    }

            changed = 1;
            /* always draw */
            yesdraw = 1;
          } else {
            /* draw pixel */

            if ((layer ? level->otileat(ntx, nty) :
		 level->tileat(ntx, nty)) != current) {
              setlayer(ntx, nty, current);
              yesdraw = 1;
              changed = 1;
            }
          }
        }

        /* calculate the destination to draw,
           if any. */
        int tx, ty;
        if (dr.InMap(mousex, mousey, tx, ty)) {
          if (Level::needsdest(layerat(tx, ty))) {
            if (level->destat(tx, ty) != olddest) {
              olddest = level->destat(tx, ty);
              yesdraw = 1;
            }
            /* XXX unify with next case */
          } else {
            if (olddest != -1) {
              olddest = -1;
              yesdraw = 1;
            }
          }
        } else {
          if (olddest != -1) {
            olddest = -1;
            yesdraw = 1;
          }
        }

        if (yesdraw) Redraw();

        break;
      }
      case SDL_MOUSEBUTTONDOWN: {
        SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;

        /* any click in this state puts us in drawing mode. */
        donotdraw = false;

        if (e->button == SDL_BUTTON_MIDDLE) {

          /* XXX would be nice if we could pair this with the event
             and not the current status of the keyboard, alas... */
          if (SDL_GetModState() & KMOD_CTRL) {
            /* swap */
            int tx, ty;
            if (dr.InMap(e->x, e->y, tx, ty)) {
              level->swapo(level->index(tx, ty));
              changed = 1;
              Redraw();
            }
          } else {
            /* eyedropper */
            int tx, ty;
            if (dr.InMap(e->x, e->y,
                         tx, ty)) {

              current = layer ? level->otileat(tx, ty) : level->tileat(tx, ty);

              Redraw();
            }
          }

        } else if (e->button == SDL_BUTTON_RIGHT) {
          /* Start drawing selection rectangle */
          int tx, ty;
          if (dr.InMap(e->x, e->y, tx, ty)) {
            selection.x = tx;
            selection.y = ty;

            selection.w = 1;
            selection.h = 1;
          } else selection.w = 0;
          Redraw();

        } else if (e->button == SDL_BUTTON_LEFT) {

          int showw = (screen->w / TILEW) - 1;

          /* on tile menu? */
          if (e->y >= TILEH &&
              e->y < (2 * TILEH)) {

            if (e->x >= (TILEW * showw) &&
                e->x < (TILEW * (showw + 1))) {

              if (e->y < TILEH + (TILEH >> 1))
                tmenurotate(-1);
              else tmenurotate(1);

            } else if (/* e->x always >= 0 && */
                       e->x < (showw * TILEW)) {

              int tt = (e->x / TILEW) + (tmenuscroll * showw);

              if (tt >= 0 && tt < NTILEORDER) current = tileorder[tt];

              Redraw();

            } /* else nothing. */



          } else if (/* e->y always >= 0 && */
                     e->y < TILEH) {
            /* menu */

            int n = e->x / TILEW;

            dr.message = itos(n);

            Redraw();

            if (n < NUM_MENUITEMS) {

              /*
                TU_SAVE, TU_SAVEAS, TU_LOAD,
                TU_TITLE, TU_AUTHOR, TU_SIZE,
                TU_PLAYERSTART, TU_CLEAR,
                TU_PLAY, TU_RANDOM,
              */

              if (n == POS_LAYER) {
                layer = !layer;
                Redraw();
              } else switch (edit_menuitem[n]) {

              case TU_SIZE:
                Resize();
                break;

              case TU_PLAYERSTART:
                PlayerStart();
                break;

              case TU_DALEK:
                placebot(B_DALEK);
                break;

              case TU_BOMB:
                placebot(currentbomb);
                break;

              case TU_HUGBOT:
                placebot(B_HUGBOT);
                break;

              case TU_BROKEN:
                placebot(B_BROKEN);
                break;

              case TU_ERASE_BOT:
                erasebot();
                break;

              case TU_FIRST_BOT:
                firstbot();
                break;

              case TU_LOAD:
                load();
                break;

              case TU_AUTHOR:
                setauthor();
                break;

              case TU_TITLE:
                settitle();
                break;

              case TU_RANDOM:
                DoRandom();
                break;

              case TU_RANDTYPE:
                randtype++;
                randtype %= NUM_RANDTYPES;
                dr.message = ainame(randtype);
                Redraw();
                break;

              case TU_BOMBTIMER:
                next_bombtimer();
                break;

              case TU_SAVEAS:
                saveas();
                break;

              case TU_SLEEPWAKE:
                sleepwake();
                break;

              case TU_PREFAB:
                prefab();
                break;

              case TU_SAVE:
                if (filename.empty()) saveas();
                else save();
                break;

              case TU_PLAY:
                playlev();
                break;

              case TU_CLEAR:
                Clear(T_FLOOR, (Tile)current);
                break;
              default: ;

              }

            } /* else outside menu */


          } else {
            int tx, ty;

            ClearSelection();

            if (dr.InMap(e->x, e->y,
                         tx, ty)) {
              /* drawing area */

              int old = layerat(tx, ty);
              setlayer(tx, ty, current);

              if (Level::needsdest(current)) {

                int xx, yy;
                if (getdest(xx, yy, "Click to set destination.")) {
                  level->setdest(tx, ty, xx, yy);
                } else {
                  setlayer(tx, ty, old);
                }
                olddest = level->destat(tx, ty);

              }

              changed = 1;
              Redraw();
            }
          }


        } else if (e->button == SDL_BUTTON_RIGHT) {
          /* on tile menu? if so, rotate. */

          if (e->y > TILEH &&
              e->y <= (2 * TILEH)) {
            tmenurotate(1);
          }

        }

        break;
      }

      case SDL_QUIT: goto edit_quit;
      case SDL_KEYDOWN:
        switch (event.key.keysym.sym) {

        case SDLK_F2:
          settitle();
          break;

        case SDLK_KP_MINUS:
        case SDLK_MINUS:
        case SDLK_PAGEUP:
          tmenurotate(-1);
          break;

        case SDLK_KP_PLUS:
        case SDLK_PLUS:
        case SDLK_EQUALS:
        case SDLK_PAGEDOWN:
          tmenurotate(1);
          break;

        case SDLK_UP:
        case SDLK_DOWN:
        case SDLK_RIGHT:
        case SDLK_LEFT: {

          dir d = DIR_NONE;
          switch (event.key.keysym.sym) {
          case SDLK_DOWN: d = DIR_DOWN; break;
          case SDLK_UP: d = DIR_UP; break;
          case SDLK_RIGHT: d = DIR_RIGHT; break;
          case SDLK_LEFT: d = DIR_LEFT; break;
          default: ; /* impossible - lint */
          }

          if ((event.key.keysym.mod & KMOD_SHIFT) &&
              selection.w > 0) {

            /* extend selection in that direction by one
               row/column. */

            /* the theory here is to "do the right thing,"
               but to explain the "right thing" with the
               simplest possible rule as to what "right"
               is. */

            /* for the discussion here we will assume the
               pattern is being extended on its right side. */

            /* the first step is to detect a pattern.
               this is done only with respect to the tiles
               (not destinations) in the foreground and
               background. bots and players are ignored as
               well. Assume the sequence of tiles is S.
               We then find the shortest prefix Sp such
               that

                S = Sp,...,Sp,S'
                    `---v---'
                      j occurrences (j >= 1)

               and S' is a (possibly empty) proper prefix
               of Sp. The pattern is then Sp. Since S' may
               be empty, a unique shortest Sp exists for
               any S. */

            /* to extend, we simply place a column (#n) of
               tiles aside the selection so that S'Sp[n] is
               still a prefix of Sp. (this is also always
               possible because S' is a *proper* prefix of
               Sp). */

            /* this explains everything except for the
               destination of panels, teleports, and remotes.
               For a panel at index i within the new column Sp[n],
               we look at the destinations of the occurrences
               of that panel in Sp_0[n], Sp_1[n], .. Sp_j-1[n],
               which we call D0..Dj-1.

               If the delta d1-d0, d2-d1, ... is constant,
               then the destination of the new panel is
               j*delta + d0. Otherwise, we arbitrarily choose
               d0. (we could instead perhaps reject this
               patterning, and extend--we can always make
               delta constant by making the whole thing one
               pattern! but this may be a bit too crazy).
               (Or, we could just put a stopsign or something
               here to indicate that we couldn't make a
               sensible panel--maybe that is better. */

            dir right = d;
            dir down;
            /* could use level::turnright? */
            switch (right) {
            default:
            case DIR_RIGHT: down = DIR_DOWN; break;
            case DIR_LEFT: down = DIR_UP; break;
            case DIR_DOWN: down = DIR_LEFT; break;
            case DIR_UP: down = DIR_RIGHT; break;
            }

            int width, height;
            switch (right) {
            default:
            case DIR_LEFT:
            case DIR_RIGHT: width = selection.w; height = selection.h; break;
            case DIR_DOWN:
            case DIR_UP: width = selection.h; height = selection.w; break;
            }

            /* the "top-left" according to the coordinate system */
            int startx, starty;
            switch (right) {
            default:
            case DIR_UP:
              startx = selection.x;
              starty = selection.y + selection.h - 1;
              break;
            case DIR_DOWN:
              startx = selection.x + selection.w - 1;
              starty = selection.y;
              break;
            case DIR_LEFT:
              startx = selection.x + selection.w - 1;
              starty = selection.y + selection.h - 1;
              break;
            case DIR_RIGHT:
              startx = selection.x;
              starty = selection.y;
              break;
            }

            int drightx = 0, drighty = 0, ddownx = 0, ddowny = 0;
            dirchange(right, drightx, drighty);
            dirchange(down,  ddownx,  ddowny);


            /* this is the beginning of the new column */
            int destx = startx + (drightx * width);
            int desty = starty + (drighty * width);

            /* check that there's room to extend */
            if (destx >= level->w ||
                desty >= level->h ||
                destx < 0 ||
                desty < 0) continue;

            /* blinktile(destx, desty, 0xFF884499); */


            /* determine pattern length "plen"
               (must be at least 1, since selection is
               non-empty) */
            int plen = 1;
            for (; plen <= width; plen++) {
              /* check that this is a legal pattern. if so,
                 it is the shortest, since we are checking
                 in order */

              /* printf("try plen %d...\n", plen); */

              /* number of pattern (prefixes) we need to
                 check. If it evenly divides plen, then
                 there are width/plen of them. But if there
                 is any partial pattern, we add that in. */
              int max = (width / plen) + !!(width % plen);
              for (int n = 0; n < max; n++) {
                /* first column of pattern occurrence to check */
                int checkx = startx + (drightx * plen * n);
                int checky = starty + (drighty * plen * n);

                /*
                printf("  (n:%d) checkx %d, checky %d\n",
                       n, checkx, checky);
                */

                /* now check the pattern here. */
                /* since it may be a prefix, we only go up
                   to "maxcol": */
                int maxcol = std::min(plen, width - (n * plen));
                /* printf("  ... maxcol = %d\n", maxcol); */
                for (int col = 0; col < maxcol; col++) {
                  for (int row = 0; row < height; row++) {
                    int offx =
                      (col * drightx) +
                      (row * ddownx);

                    int offy =
                      (col * drighty) +
                      (row * ddowny);

                    #if 0
                    printf("   compare %d/%d to %d/%d\n",
                           startx + offx, starty + offy,
                           checkx + offx, checky + offy);
                    blinktile(startx + offx, starty + offy,
                              0xFF0000FF);
                    blinktile(checkx + offx, checky + offy,
                              0xFFFF0000);
                    Redraw();
                    #endif

                    /* must be equal in bg and fg */
                    if (
                        ! ((level->tileat(startx + offx,
					  starty + offy) ==
                            level->tileat(checkx + offx,
					  checky + offy)) &&

                           (level->otileat(startx + offx,
					   starty + offy) ==
                            level->otileat(checkx + offx,
                                           checky + offy)))) {
                      /* printf("NOT EQUAL.\n"); */
                      /* then we fail; check next */
                      goto next_size;

                    }

                  }
                }

                /* check pattern occurrence #n */
              }
              /* success! */
              break;
            next_size: ;
            }
            /* printf("OK: plen is: %d\n", plen); */

            /* make new column */
            {
              /* column offset within pattern */
              int col = width % plen;
              for (int row = 0; row < height; row++) {
                int offx = (row * ddownx);
                int offy = (row * ddowny);

                int p =
                  level->tileat(startx + (col * drightx) + offx,
				starty + (col * drighty) + offy);

                level->settile(destx + offx,
			       desty + offy, p);

                int op =
                  level->otileat(startx + (col * drightx) + offx,
				 starty + (col * drighty) + offy);

                level->osettile(destx + offx,
				desty + offy, op);

                if (Level::needsdest(p) || Level::needsdest(op)) {
                  int delta = 0;
                  /* need to find delta. */
                  /* even simpler: assume that delta is
                     constant (from the first gap). */

                  int origdest =
                    level->destat(startx + (col * drightx) + offx,
				  starty + (col * drighty) + offy);

                  if (width < (plen * 2)) delta = 0;
                  else {
                    int seconddest =
                      level->destat(startx + ((col + plen) * drightx) +
				    offx,
				    starty + ((col + plen) * drighty) +
				    offy);

                    delta = seconddest - origdest;
                  }

                  /* then the new dest is delta * n + origdest */
                  int n = (width) / plen;

                  /* printf("delta: %d, n: %d\n", delta, n); */


                  int de = delta * n + origdest;
                  if (de < 0) de = 0;
                  if (de >= level->w * level->h)
                    de = (level->w * level->h) - 1;

                  int xxx, yyy;
                  level->where(de, xxx, yyy);
                  level->setdest(destx + offx, desty + offy, xxx, yyy);
                }

              }
            }

            /* and extend pattern */
            switch (d) {
            default: break;
            case DIR_RIGHT: selection.w++; break;
            case DIR_DOWN: selection.h++; break;
            case DIR_UP: selection.y--; selection.h++; break;
            case DIR_LEFT: selection.x--; selection.w++; break;
            }

            Redraw();

          } else if ((event.key.keysym.mod & KMOD_CTRL) &&
                     selection.w > 0) {

            int dx = 0, dy = 0;
            dirchange(d, dx, dy);

            /* tx, ty are the start of the destination
               of this block move */
            int tx, ty;
            /* but first check that the far corner will
               also be on the screen */
            if (!level->travel(selection.x + selection.w - 1,
			       selection.y + selection.h - 1,
			       d, tx, ty)) break;


            if (level->travel(selection.x, selection.y, d,
			      tx, ty)) {
              /* easier if we clone. */
	      std::unique_ptr<Level> cl = level->Clone();

              /* then blank out the region */
	      for (int y = selection.y; y < selection.y + selection.h; y++) {
		for (int x = selection.x; x < selection.x + selection.w; x++) {
		  level->settile(x, y, T_FLOOR);
		  level->osettile(x, y, T_FLOOR);
		  level->setdest(x, y, 0, 0);
		  level->setflag(x, y, 0);
		}
	      }

              for (int y = selection.y; y < selection.y + selection.h; y++) {
                for (int x = selection.x; x < selection.x + selection.w; x++) {

                  /* copy all the parts */
                  level->settile(x + dx, y + dy,
				 cl->tileat(x, y));

                  level->osettile(x + dx, y + dy,
				  cl->otileat(x, y));

                  {
		    int ddx, ddy;
		    cl->where(cl->destat(x, y), ddx, ddy);

		    /* if the destination is inside the
		       thing we're moving, then preserve it */
		    if ((Level::needsdest(cl->tileat(x, y)) ||
			 Level::needsdest(cl->otileat(x, y))) &&
			ddx >= selection.x &&
			ddx < (selection.x + selection.w) &&
			ddy >= selection.y &&
			ddy < (selection.y + selection.h)) {
		      ddx += dx;
		      ddy += dy;
		    }

		    /* anyway copy dest */
		    level->setdest(x + dx, y + dy, ddx, ddy);
                  }

                  /* finally, flags */
                  level->setflag(x + dx, y + dy,
				 cl->flagat(x, y));
                }
	      }

              /* move player, bots */
              if (level->guyx >= selection.x &&
                  level->guyy >= selection.y &&
                  level->guyx < (selection.x + selection.w) &&
                  level->guyy < (selection.y + selection.h)) {
                level->guyx += dx;
                level->guyy += dy;

                /* if moving over bot (on edge), delete it */
                if (level->guyx < selection.x ||
                    level->guyy < selection.y ||
                    level->guyx >= (selection.x + selection.w) ||
                    level->guyy >= (selection.y + selection.h)) {
                  int bi;
                  if (level->botat(level->guyx, level->guyy, bi)) {
                    level->bott[bi] = B_DELETED;
                  }
                }
              }


	      for (int i = 0; i < level->nbots; i++) {
		int bx, by;
		level->where(level->boti[i], bx, by);
		if (bx >= selection.x &&
		    by >= selection.y &&
		    bx < (selection.x + selection.w) &&
		    by < (selection.y + selection.h)) {
		  bx += dx;
		  by += dy;

		  /* destroy any bot we're overwriting
		     (but not if it's in the selection, because
		     then it will move, too) */
		  int bi;
		  if (bx < selection.x ||
		      by < selection.y ||
		      bx >= (selection.x + selection.w) ||
		      by >= (selection.y + selection.h)) {

		    if (level->botat(bx, by, bi)) {
		      /* overwrite bot */
		      level->bott[bi] = B_DELETED;
		    } else if (level->playerat(bx, by)) {
		      /* Delete self if trying to
			 overwrite player! */
		      level->bott[i] = B_DELETED;
		    }
		  }
		}

		/* move bot (even if deleted) */
		level->boti[i] = level->index(bx, by);
	      }

              /* move selection with it, but don't change size */
              selection.x = tx;
              selection.y = ty;

              FixUp();
            } /* would move stay on screen? */

            Redraw();
          } else {
            /* move scroll window */

            int dx = 0, dy = 0;
            dirchange(d, dx, dy);
            dr.scrollx += dx;
            dr.scrolly += dy;

            dr.MakeScrollReasonable();

            Redraw();
          }
          break;
        }
        default:
          /* no match; try unicode */

        switch (event.key.keysym.unicode) {

        case SDLK_ESCAPE:
          if (changed) {
            if (Message::Quick(this,
                               "Quitting will destroy your unsaved changes.",
                               "Quit anyway.",
                               "Don't quit.")) {
              goto edit_quit;
            } else {
              Redraw();
            }
          } else goto edit_quit;

        case SDLK_RETURN:
          /* XXX center scroll on mouse */
          break;

        case SDLK_r:
          DoRandom();
          break;

        case SDLK_z:
          Resize();
          break;

        case SDLK_c:
          Clear(T_FLOOR, (Tile)current);
          break;

        case SDLK_v: /* XXX paste */
          break;

        case SDLK_e:
          PlayerStart();
          break;

        case SDLK_o:
          erasebot();
          break;

        case SDLK_f:
          firstbot();
          break;

        case SDLK_i:
          prefab();
          break;

        case SDLK_w:
          sleepwake();
          break;

        case SDLK_m:
          next_bombtimer();
          break;

        case SDLK_k:
          placebot(B_DALEK);
          break;

        case SDLK_b:
          placebot(currentbomb);
          break;

        case SDLK_n:
          placebot(B_BROKEN);
          break;

        case SDLK_h:
          placebot(B_HUGBOT);
          break;

        case SDLK_u:
          setauthor();
          break;

        case SDLK_d:
          if (event.key.keysym.mod & KMOD_CTRL) {
            ClearSelection();
            Redraw();
          } else {
            showdests = !showdests;
            Redraw();
          }
          break;

        case SDLK_a:
          if (event.key.keysym.mod & KMOD_CTRL) {
            selection.x = 0;
            selection.y = 0;
            selection.w = level->w;
            selection.h = level->h;
            Redraw();
          } else {
            saveas();
          }
          break;

        case SDLK_t:
          settitle();
          break;

        case SDLK_s:
          if (filename.empty()) saveas();
          else save();
          break;

        case SDLK_p:
          playlev();
          break;

        case SDLK_l:
          load();
          break;

        case SDLK_y:
          layer = !layer;
          Redraw();
          break;

          /* zoom */
        case SDLK_LEFTBRACKET:
        case SDLK_RIGHTBRACKET:
          dr.zoomfactor += (event.key.keysym.unicode == SDLK_LEFTBRACKET)? +1 : -1;
          if (dr.zoomfactor < 0) dr.zoomfactor = 0;
          if (dr.zoomfactor >= DRAW_NSIZES) dr.zoomfactor = DRAW_NSIZES - 1;

          /* scrolls? */
          dr.MakeScrollReasonable();

          Redraw();
          break;

        default:
          break;
        } /* switch unicode */
        } /* switch sym */
        break;
      default:;
      }
    }

    SDL_Delay(25);
  }
 edit_quit:

  return;
}

void Editor::next_bombtimer() {
  currentbomb = (bot)((int)currentbomb + 1);
  if ((int)currentbomb > (int)B_BOMB_MAX)
    currentbomb = B_BOMB_0;
  Redraw();
}

bool Editor::getdest(int &x, int &y, string msg) {
  ClearSelection();

  SDL_Event event;

  dr.message = msg;

  Redraw();

  for (;;) {
    while (SDL_PollEvent(&event)) {
      if (HandleVideoEvent(this, event)) continue;

      switch (event.type) {

      case SDL_MOUSEBUTTONDOWN: {
        SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;

        /* we don't want this click (and drag) to result in
           drawing. */
        donotdraw = true;

        if (e->button == SDL_BUTTON_LEFT) {

          int tx, ty;
          if (dr.InMap(e->x, e->y,
                       tx, ty)) {
            x = tx;
            y = ty;

            dr.message = itos(tx) + (string)", " + itos(ty);
            return 1;
          }
          /* other clicks cancel */
        } else {
          dr.message = "";
          return 0;
        }
        break;
      }

      case SDL_QUIT: return 0;
      case SDL_KEYDOWN:
        switch (event.key.keysym.sym) {
        case SDLK_UP: dr.scrolly--; break;
        case SDLK_DOWN: dr.scrolly++; break;
        case SDLK_LEFT: dr.scrollx--; break;
        case SDLK_RIGHT: dr.scrollx++; break;
        case SDLK_ESCAPE:
          dr.message = "";
          return 0;
        default:
          break;
        }
        dr.MakeScrollReasonable();
        Redraw();
        break;
      default:;
      }
    }

    SDL_Delay(25);
  }
  /* XXX unreachable */
  return 0;
}

/* fix various things before playing or saving. */
void Editor::FixUp() {
  /* XXX should fon->parens the texts */

  if (level->title.empty()) level->title = "Untitled";
  if (level->author.empty()) level->author = plr->name;
  if (level->author.empty()) level->author = "Anonymous";

  for (int i = 0; i < level->w * level->h; i++) {
    /* always clear 'temp' flag. */
    level->flags[i] &= ~(TF_TEMP);

    /* make sure panel flag is set if tile is a panel.
       we have to also set the flag refinement: is it
       a regular, blue, green, or red panel?
    */
    /* first remove the flags no matter what. (we don't
       want to accumulate *extra* flags) */
    level->flags[i] &= ~(TF_HASPANEL | TF_RPANELL | TF_RPANELH);
    level->flags[i] &= ~(TF_OPANEL | TF_ROPANELL | TF_ROPANELH);

    /* restore them where appropriate */
    int ref;
    if (Level::ispanel(level->tiles[i], ref)) {
      level->flags[i] |= TF_HASPANEL |
                     ((ref & 1) * TF_RPANELL) |
                    (((ref & 2) >> 1) * TF_RPANELH);
    }

    if (Level::ispanel(level->otiles[i], ref)) {
      level->flags[i] |= TF_OPANEL |
                     ((ref & 1) * TF_ROPANELL) |
                    (((ref & 2) >> 1) * TF_ROPANELH);
    }

    /* unset destination if not needed (makes
       smaller files because of longer runs) */
    if (!(Level::needsdest(level->tiles[i]) ||
          Level::needsdest(level->otiles[i])))
      level->dests[i] = 0;
  }

  /* bots: remove deleted ones, normalize
     direction and attributes */
  {
    int bdi = 0;
    for (int bi = 0; bi < level->nbots; bi++) {
      if (level->bott[bi] >= 0 &&
          level->bott[bi] < NUM_ROBOTS) {
        /* save bot */
        level->bott[bdi] = level->bott[bi];
        level->boti[bdi] = level->boti[bi];
        level->botd[bdi] = DIR_DOWN;
        /* always -1 */
        level->bota[bdi] = -1;
        bdi++;
      }
    }

    level->nbots = bdi;
  }

  level->fixup_botorder();

  // XXX At this point the level should always
  // pass sanitize test. Check?
}

Editor *Editor::Create(Player *p) {
  std::unique_ptr<Editor> ee{new Editor};

  ee->randtype = RT_MAZE;
  ee->plr = p;

  ee->current = T_BLUE;
  ee->layer = 0;
  ee->tmenuscroll = 0;

  ee->showdests = false;

  ee->changed = 0;

  ee->currentbomb = (bot)((int)B_BOMB_0 + 3);

  if (!ee->plr) return 0;

  return ee.release();
}

