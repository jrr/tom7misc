
#include "SDL.h"
#include <math.h>
#include "time.h"
#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"
#include "ptrlist.h"

#include "escapex.h"
#include "play.h"

#include "extent.h"
#include "message.h"
#include "chars.h"
#include "util.h"
#include "dirindex.h"
#include "../cc-lib/md5.h"
#include "prefs.h"
#include "prompt.h"

#include "aevent.h"
#include "animation.h"
#include "dirt.h"
#include "optimize.h"

#include "menu.h"
#include "solutionuploading.h"
#include "client.h"
#include "../cc-lib/base64.h"

#define POSTDRAW ;

namespace {

/* for observing frame by frame -- slooow */
// #define POSTDRAW SDL_Delay (300);

/* medium speed */
// #define POSTDRAW SDL_Delay (100);

struct BookmarkItem;

enum class PlayState {
  OKAY,
  DEAD,
  WON,
};

struct Play_ : public Play {
  bool waitenter();
  void draw() override;
  void screenresize() override;

  PlayResult DoPlaySave(Player *plr, Level *lev,
                        Solution *saved, const string &md5) override;

  PlayResult DoPlay(Player *plr, Level *lev, const string &md5) override {
    Solution unused;
    PlayResult res = DoPlaySave(plr, lev, &unused, md5);
    return res;
  }

  ~Play_() override {}

  /* debugging */
  int layer;
  bool showdests;
  bool showdestsshuffle;
  bool showbotnums;

  Drawing dr;
  /* current solution.
     Its lifetime is within a call to DoPlaySave.
     Don't call redraw when not inside a call to DoPlaySave! */
  Solution sol;
  /* current position in the solution. This is usually the same as
     sol.Length(), but if it is not, then we support the VCR (soon) and
     redo. */
  int solpos = 0;

  static Play_ *Create();
  void redraw();

  void videoresize(SDL_ResizeEvent *eventp);

  /* hand closure-converted, ugh */
  bool Redo();
  void Undo(const Level *start, Extent<Level> &ec, int nmoves);
  void Restart(const Level *start, Extent<Level> &ec);
  void Checkpoint(Solution *saved_sol);
  void Restore(Extent<Level> &ec,
               Level *start,
               const Solution *saved_sol);
  // sol is a pointer to the current solution; the bookmarks menu may
  // replace it.
  void Bookmarks(Level *start,
                 Extent<Level> &ec,
                 Player *plr, string md5,
                 Solution *sol);
  void bookmark_download(Player *plr, string lmd5, Level *lev);


  void drawmenu();

 private:
  bool getevent(SDL_Event *e, bool &fake);

  bool watching;
  Uint32 nextframe;
  PlayState CurState();

  static void SetSolsFromBookmarkItems(Player *plr, const string &md5,
                                       BookmarkItem **books,
                                       int n);
};

Play_ *Play_::Create() {
  Play_ *pr = new Play_();
  pr->watching = false;
  pr->layer = 0;
  pr->showdests = false;
  pr->showdestsshuffle = false;
  pr->showbotnums = false;
  pr->dr.margin = 12;
  return pr;
}

// XXX to local, or to class?
SDL_Event dummy[256];

/* idea: scrolling move history? */
#define MI_NODRAW -1
static int play_menuitem[] = {
  TU_SAVESTATE,
  TU_RESTORESTATE,
  TU_BOOKMARKS,
  /* TU_QUIT */
  TU_RESTART,
  TU_UNDO,
  TU_REDO,
  TU_FUNDO,
  TU_PLAYPAUSE,
  TU_FREDO,
  MI_NODRAW, MI_NODRAW, /* skip -- nmoves */
};

#define POS_RESTORESTATE 1
#define POS_RESTART 3
#define POS_UNDO 4
#define POS_REDO 5
#define POS_MOVECOUNTER 9
#define POS_PLAYPAUSE 7
#define POS_FUNDO 6
#define POS_FREDO 8
#define NUM_PLAYMENUITEMS (sizeof (play_menuitem) / sizeof (int))

#define THUMBW 180
#define THUMBH 110

enum bmaction {
  BMA_NONE,
  BMA_SELECT,
  BMA_RENAME,
  BMA_DELETE,
  BMA_SETDEFAULT,
  BMA_OPTIMIZE,
  BMA_WATCH,
};

static constexpr int bmi_zoomf = 2;
struct BookmarkItem : public MenuItem {
  /* XX should base on size of level, number of bookmarks
     (base what? the size of the display? - tom) */

  /* with solution executed */
  Level *lev = nullptr;

  /* unsolved level, for communication with server */
  string levmd5;
  /* not owned */
  Player *plr = nullptr;

  /* the solution */
  NamedSolution ns;
  bool solved = false;

  Drawable *below = nullptr;

  /* XXX */
  virtual string helptext() {
    return "Press " BLUE "enter" POP " to load this bookmark.";
  }

  string bookmenu;
  string solmenu;

  virtual void draw(int x, int y, int f) {
    Drawing dr;
    dr.posx = x;
    dr.posy = y + 2;
    dr.width = THUMBW;
    dr.height = THUMBH;
    dr.margin = 0;
    dr.zoomfactor = bmi_zoomf;
    dr.scrollx = 0;
    dr.scrolly = 0;

    dr.lev = lev;
    dr.setscroll();
    dr.drawlev();

    if (f) {
      fon->draw(x + THUMBW + 4, y + 4, YELLOW + ns.name);
    } else {
      fon->draw(x + THUMBW + 4, y + 4, ns.name);
    }

    char da[256];
    const time_t t = ns.date;
    strftime(da, 255, "%H:%M:%S  %d %b %Y", localtime(&t));
    if (ns.author != "")
      fon->draw(x + THUMBW + 4, y + 4 + fon->height, "by " + ns.author);
    fon->draw(x + THUMBW + 4, y + 4 + (fon->height * 2), da);
    fon->draw(x + THUMBW + 4, y + 4 + (fon->height * 3),
              (string)(solved ? PICS THUMBICON " " POP : PICS BOOKMARKPIC POP) +
              itos(ns.sol.Length()) + " moves");

    if (f)
      fonsmall->draw(x + THUMBW + 4, 2 + y + 4 + (fon->height * 4),
                     BLUE +
                     (solved ? solmenu : bookmenu) + POP);

    // + (string)(solved?" " GREEN "(solved)":""));
  }

  virtual void size(int &w, int &h) {

    /* XXX also author, date.. */
    w = THUMBW + 8 +
      util::maximum(fon->sizex(ns.name),
                    fonsmall->sizex(solmenu));

    /* at least 4 lines for text, plus menu,
       but then the minimum of the thumbnail height and the
       actual level's height at this zoom */
    h = util::maximum(8 + fonsmall->height + fon->height * 4,
                      util::minimum(THUMBH, 4 + lev->h * (TILEH >> bmi_zoomf)));

  }

  virtual inputresult key(SDL_Event e) {

    switch (e.key.keysym.sym) {
    case SDLK_RETURN:
      /* XXX if solution, maybe go straight to watching? */
      action.a = BMA_SELECT;
      return inputresult(MR_OK);

    case SDLK_d:
    case SDLK_DELETE:
      /* XXX warn especially if it is the last solution? */
      if (Message::quick(container,
                         "Really delete '" YELLOW +
                         ns.name + POP "'?",
                         "Delete",
                         "Cancel")) {

        action.a = BMA_DELETE;
        return inputresult(MR_OK);
      } else return inputresult(MR_UPDATED);

    case SDLK_INSERT: {
      /* only if it is solved */
      if (solved) {
        action.a = BMA_SETDEFAULT;
        return inputresult(MR_OK);
      } else {
        Message::no(container, "Only a solution can be the default.");
        return inputresult(MR_UPDATED);
      }
    }
    case SDLK_u: {
      if (solved) {
        SolutionUploading::PromptUpload(
            below, plr, levmd5,
            ns.sol,
            "Please only upload interesting solutions or speedruns.",
            ns.name,
            false);
      } else {
        Message::no(container, "You can only upload full solutions.");
      }
      return inputresult(MR_UPDATED);
    }

    case SDLK_o: {
      /* only if it is solved */
      if (solved) {
        action.a = BMA_OPTIMIZE;
        return inputresult(MR_OK);
      } else {
        Message::no(container, "You can only optimize solutions.");
        return inputresult(MR_UPDATED);
      }
    }

    case SDLK_r:
    case SDLK_F2: {
      action.s = Prompt::ask(container,
                             "New name: ", ns.name);
      if (action.s != "") {
        action.a = BMA_RENAME;
        return inputresult(MR_OK);
      } else {
        /* redraw */
        return inputresult(MR_UPDATED);
      }
    }

    case SDLK_w:
      action.a = BMA_WATCH;
      return inputresult(MR_OK);

    default:
      return MenuItem::key(e);
    }
  }

  virtual inputresult click(int, int) {
    SDL_Event e;
    /* XXX is this enough to make it a legal key? */
    /* XXX should be a library call */
    e.type = SDL_KEYDOWN;
    e.key.keysym.sym = SDLK_RETURN;
    e.key.keysym.unicode = SDLK_RETURN;
    e.key.keysym.mod = (SDLMod) 0;
    return key(e);
  }

  struct act {
    bmaction a;
    string s;
  } action;

  /* copies lev, solution */
  BookmarkItem(Level *l, const NamedSolution *n, Player *p,
               string md, Drawable *b) {
    bookmenu =
                   "[" YELLOW "r" POP WHITE "ename" POP "]"
                   "[" YELLOW "d" POP WHITE "elete" POP "]"
                   "[" YELLOW "w" POP WHITE "atch"  POP "]";

    solmenu =
                   bookmenu +
                   "[" YELLOW "u" POP WHITE "pload" POP "]"
                   "[" YELLOW "o" POP WHITE "ptimize" POP "]";

    lev = l->clone();
    ns = *n;
    int unused = 0;
    lev->Play(ns.sol, unused);
    solved = lev->iswon() && ns.sol.Length();
    action.a = BMA_NONE;
    plr = p;
    levmd5 = md;
    below = b;
  }

  void destroy() {
    lev->destroy();
  }
};


void Play_::drawmenu() {
  int showw = (screen->w / TILEW) - 1;

  /* could be showw + 1 */
  for (int j = 0; j < (showw + 1) && j < NUM_PLAYMENUITEMS; j++) {
    if (j == POS_MOVECOUNTER) {
      string count;
      if (solpos != sol.Length()) {
        count = itos(solpos) + GREY "/" POP BLUE +
          itos(sol.Length()) + POP;
      } else {
        count = itos(sol.Length());
      }
      fon->draw(2 + j * TILEW + 4, 2 + (TILEH>>1) - (fon->height>>1),
                count);
    } else if (play_menuitem[j] == MI_NODRAW) {
      /* nothing */
    } else if (play_menuitem[j] == TU_PLAYPAUSE) {
      /* if currently playing, draw this as a pause button, not a
         play button */
      if (watching) {
        Drawing::drawtileu(2 + j * TILEW, 2, TU_PLAYPAUSE_PLAY, 0);
      } else {
        Drawing::drawtileu(2 + j * TILEW, 2, TU_PLAYPAUSE, 0);
      }
    } else {
      Drawing::drawtileu(2 + j * TILEW, 2, play_menuitem[j], 0);
    }
  }

  /* disable menu items where appropriate (undo/redo) */
  if (solpos == 0) {
    /* nb. important that these two share disabled
       state, since the graphics overlap */
    Drawing::drawtileu(POS_RESTART * TILEW, 2, TU_DISABLED, 0);
    Drawing::drawtileu(POS_UNDO * TILEW, 2, TU_DISABLED, 0);
    Drawing::drawtileu(POS_FUNDO * TILEW, 2, TU_DISABLED, 0);
  }

  if (solpos == sol.Length()) {
    Drawing::drawtileu(POS_REDO * TILEW, 2, TU_DISABLED, 0);
    Drawing::drawtileu(POS_FREDO * TILEW, 2, TU_DISABLED, 0);
    Drawing::drawtileu(POS_PLAYPAUSE * TILEW, 2, TU_DISABLED, 0);
  }
}

void Play_::draw() {
  dr.setscroll();

  Uint32 color =
    SDL_MapRGBA(screen->format, 0x22, 0x22, 0x44, 0xFF);

  /* clear back */
  sdlutil::clearsurface(screen, BGCOLOR);

  /* draw highlights and stuff */
  {
    SDL_Rect dst;
    dst.x = 2;
    dst.w = screen->w - 4;
    dst.y = 4 + TILEH;
    dst.h = fon->height + 4;
    SDL_FillRect(screen, &dst, color);
  }


  drawmenu();

  dr.drawlev(layer);

  if (showdests) dr.drawdests(0, showdestsshuffle);
  if (showbotnums) dr.drawbotnums();

  fon->drawto(screen, 4, TILEH + 6,
              dr.lev->title + (string)" " GREY "by " POP BLUE +
              dr.lev->author + POP);

  switch (CurState()) {
  case PlayState::OKAY: break;
  case PlayState::DEAD:
    Message::drawonlyv(screen->h - fon->height*8,
                       "You've died.",
                       "Try again",
                       "Quit", PICS SKULLICON);
    break;
  case PlayState::WON:
    Message::drawonlyv(screen->h - fon->height*8,
                       "You solved it!!",
                       "Continue", "", PICS THUMBICON);
    break;
  }

  /* XXX wrong, should use height,posy */
  // fon->drawto(surf, posx + 2, (surf->h) - (fon->height + 1), Message);

  // dr.drawextra();
}

PlayState Play_::CurState() {
  int unused;
  dir unusedd;
  if (solpos != 0 && dr.lev->isdead(unused, unused, unusedd))
    return PlayState::DEAD;
  else if (solpos != 0 && dr.lev->iswon())
    return PlayState::WON;
  else return PlayState::OKAY;
}

void Play_::redraw() {
  draw();
  SDL_Flip(screen);
}

void Play_::screenresize() {
  dr.width = screen->w - dr.posx;
  dr.height = screen->h - dr.posy;
}

void Play_::videoresize(SDL_ResizeEvent *eventp) {
  screen = sdlutil::makescreen(eventp->w,
                                eventp->h);
  screenresize();
  redraw();
}

using elist = PtrList<aevent>;
using alist = PtrList<Animation>;

bool Play_::Redo() {
  watching = false;
  if (CurState() == PlayState::OKAY &&
      solpos < sol.Length()) {
    if (dr.lev->Move(sol.At(solpos))) {
      solpos++;
      return true;
    } else {
      Message::no(this,
                  "Can't redo! (Illegal move!)");
      return false;
    }
  } else return false;
}

/* oh how I yearn for nested functions
   well now you have them! So what? -2016
 */
void Play_::Undo(const Level *start, Extent<Level> &ec, int nm) {
  if (solpos > 0) {
    dr.lev->destroy();
    dr.lev = start->clone();
    ec.replace(dr.lev);

    /* move position backwards */
    solpos -= nm;
    if (solpos < 0) solpos = 0;

    int moves;
    dr.lev->PlayPrefix(sol, moves, 0, solpos);
    watching = false;
    redraw();
  }
}

void Play_::Restart(const Level *start, Extent<Level> &ec) {
  dr.lev->destroy();
  solpos = 0;
  dr.lev = start->clone();
  ec.replace(dr.lev);
  watching = false;
  redraw();
}

/* set the Player's solution set to the given array
   (of bookmarkitems) */

void Play_::SetSolsFromBookmarkItems(Player *plr, const string &md5,
                                     BookmarkItem **books,
                                     int n) {
  vector<NamedSolution> newsols;
  newsols.reserve(n);

  for (int i = 0; i < n; i++)
    newsols.push_back(books[i]->ns);

  plr->SetSolutionSet(md5, newsols);
  plr->writefile();
}

void Play_::Bookmarks(Level *start,
                      Extent<Level> &ec,
                      Player *plr, string md5,
                      Solution *sol) {
  enum okaywhat_t { OKAYWHAT_HUH, OKAYWHAT_NEW=10, OKAYWHAT_DOWNLOAD, };

  bool show_menu_again;
  do {
    okaywhat_t okay_what = OKAYWHAT_HUH;
    show_menu_again = false;
    if (md5 == "") {
      Message::bug(this,
                   "Bookmarks aren't available because \n"
                   "I can't figure out what level this is!");
      redraw();
      return;
    }

    label nettitle;
    nettitle.text = PICS BARLEFT BAR BAR BARRIGHT POP " Server bookmarks "
      PICS BARLEFT BAR BAR BARRIGHT POP;

    label seltitle;
    seltitle.text = PICS BARLEFT BAR BAR BARRIGHT POP " Existing bookmarks "
      PICS BARLEFT BAR BAR BARRIGHT POP;

    label newtitle;
    newtitle.text = PICS BARLEFT BAR BAR BARRIGHT POP " Add a new bookmark "
      PICS BARLEFT BAR BAR BARRIGHT POP;

    textinput defname;
    defname.question = "New bookmark name:";
    /* XXX maybe generate in serial? */
    defname.input = "Bookmark";
    defname.explanation = "The bookmark will be saved with this name.";

    okay book_current;
    book_current.ptr = (int*)&okay_what;
    book_current.myval = OKAYWHAT_NEW;
    book_current.text = "Bookmark current state";
    book_current.explanation =
      "Bookmarks are saved in your player file,\n"
      "and allow you to come back to a place in\n"
      "solving the level where you left off.";

    cancel can;
    can.text = "Cancel";

    PtrList<MenuItem> *l = nullptr;

    PtrList<MenuItem>::push(l, &can);
    PtrList<MenuItem>::push(l, &book_current);
    PtrList<MenuItem>::push(l, &defname);
    PtrList<MenuItem>::push(l, &newtitle);

    /* initialize bmset with current bookmarks. */
    bool didsolve = false;

    const vector<NamedSolution> &existing_solutions = plr->SolutionSet(md5);
    const int bmnum = existing_solutions.size();
    BookmarkItem **books =
      (BookmarkItem**) malloc(sizeof (BookmarkItem *) * bmnum);

    // Build up the list in reverse so that it's the same order as
    // existing_solutions. We rely on these being parallel below
    // (which is sorta bad).
    for (int i = bmnum - 1; i >= 0; i--) {
      if (!existing_solutions[i].bookmark) didsolve = true;
      BookmarkItem *bi =
        new BookmarkItem(start, &existing_solutions[i], plr, md5, this);

      bi->explanation =
        "Selecting this bookmark will load it,\n"
        "losing your current progress.\n";

      PtrList<MenuItem>::push(l, bi);
      books[i] = bi;
    }

    /* only place 'existing' header if there are bookmarks */
    if (bmnum > 0) PtrList<MenuItem>::push(l, &seltitle);

    /* then if any bookmark is an actual solution, allow net access */
    okay netbutton;
    netbutton.ptr = (int*)&okay_what;
    netbutton.myval = OKAYWHAT_DOWNLOAD;
    netbutton.text = "Download solutions";
    netbutton.explanation =
      "Download all the solutions stored on the server,\n"
      "if you don't have them already.";

    if (didsolve) {
      PtrList<MenuItem>::push(l, &netbutton);
      PtrList<MenuItem>::push(l, &nettitle);
    }

    Menu *mm = Menu::create(this, "Bookmarks", l, false);
    Extent<Menu> em(mm);
    PtrList<MenuItem>::diminish(l);

    mm->yoffset = fon->height + 4;
    mm->alpha = 230;


    resultkind k = mm->menuize();

    switch (k) {
    case MR_NEXT: case MR_PREV:
    case MR_REJECT: case MR_UPDATED:
    case MR_NOTHING: break;
    case MR_OK: {
      /* MR_OK could come from hitting the OK button,
         or hitting one of the bookmarks. so look
         to see if it was a bookmark first. */

      for (int i = 0; i < bmnum; i++) {
        bmaction a = books[i]->action.a;
        switch (a) {
        case BMA_NONE: break;

        case BMA_SETDEFAULT: {
          /* XXX does this work right??
             should indicate the default bookmark here.
             (or else maybe try to set the default
             automatically, since it isn't very important) */

          vector<NamedSolution> newsolutions;
          newsolutions.reserve(existing_solutions.size());
          newsolutions.push_back(existing_solutions[i]);
          for (int j = 0; j < existing_solutions.size(); j++) {
            if (i != j) newsolutions.push_back(existing_solutions[j]);
          }
          plr->SetSolutionSet(md5, std::move(newsolutions));
          plr->writefile();

          show_menu_again = true;
          goto found_action;
        }

        case BMA_DELETE: {
          vector<NamedSolution> newsolutions;
          newsolutions.reserve(existing_solutions.size() - 1);
          for (int j = 0; j < existing_solutions.size(); j++) {
            if (i != j) newsolutions.push_back(existing_solutions[j]);
          }
          plr->SetSolutionSet(md5, std::move(newsolutions));
          plr->writefile();

          show_menu_again = true;
          goto found_action;
        }

        case BMA_OPTIMIZE: {
          vector<NamedSolution> newsolutions = existing_solutions;
          Solution opt = Optimize::Opt(start, books[i]->ns.sol);

          books[i]->ns.sol = std::move(opt);
          SetSolsFromBookmarkItems(plr, md5, books, bmnum);

          show_menu_again = true;
          goto found_action;
        }

        case BMA_RENAME:
          /* rename... */
          books[i]->ns.name = books[i]->action.s;

          /* save... */
          SetSolsFromBookmarkItems(plr, md5, books, bmnum);
          show_menu_again = true;

          /* and restart... */
          goto found_action;

        case BMA_WATCH: /* FALLTHROUGH */
        case BMA_SELECT: {
          /* restore the bookmark */

          dr.lev->destroy();
          dr.lev = start->clone();
          ec.replace(dr.lev);

          *sol = books[i]->ns.sol;

          if (a == BMA_WATCH) {
            watching = true;
            nextframe = 0;
            solpos = 0;
          } else {
            solpos = sol->Length();
            watching = false;
            int moves;
            dr.lev->Play(*sol, moves);
          }

          goto found_action;
        }
        }
      }

      /* didn't click on a bookmark. Could be one of the
         other buttons... */

      switch (okay_what) {

      case OKAYWHAT_NEW: {
        /* bookmark new */
        {
          /* need to trim this solution so that we are bookmarking
             the current position without redos */
          Solution book = *sol;
          book.Truncate(solpos);
          NamedSolution ns(std::move(book),
                           (defname.input == "") ? "Bookmark" : defname.input,
                           /* no author */
                           "",
                           time(0),
                           true);
          /* shouldn't make this default */
          plr->AddSolution(md5, std::move(ns), false);
          plr->writefile();
        }
        /* message: "bookmark added" */
        break;
      }

      case OKAYWHAT_DOWNLOAD: {
        bookmark_download(plr, md5, start);
        show_menu_again = true;
        break;
      }

      default: {
         Message::bug(0, "huh?");
        break;
      }
      }

    }
    case MR_QUIT:
      /* XXX actually quit */
    case MR_CANCEL:
      /* cancel */
      break;
    }

  found_action:;

    /* now erase the bookmark menuitems */
    for (int i = 0; i < bmnum; i++) {
      books[i]->destroy();
    }
    free(books);

  } while (show_menu_again);
  redraw();
}

/* XXX this should be documented in protocol.txt */
void Play_::bookmark_download(Player *plr, string lmd5, Level *lev) {
  string s;
  Client::quick_txdraw td;

  std::unique_ptr<HTTP> hh{Client::connect(plr, td.tx.get(), &td)};

  if (hh.get() == nullptr) {
    Message::no(&td, "Couldn't connect!");
    return;
  }

  /* XXX register callback.. */

  httpresult hr = hh->get(ALLSOLS_URL + MD5::Ascii(lmd5), s);

  if (hr == HT_OK) {
    /* parse result. see protocol.txt */
    int nsols = util::stoi(util::getline(s));

    td.say("OK. Solutions on server: " GREEN + itos(nsols) + POP);

    /* get them! */
    for (int i = 0; i < nsols; i++) {
      string line1 = util::getline(s);
      string author = util::getline(s);
      string moves = Base64::Decode(util::getline(s));

      /* this is the solution id, which we don't need */
      (void) util::stoi(util::chop(line1));
      int date = util::stoi(util::chop(line1));
      string name = util::losewhitel(line1);

      Solution s;
      if (!Solution::FromString(moves, &s)) {
        Message::no(&td, "Bad solution on server!");
        return;
      }

      if (!plr->HasSolution(lmd5, s)) {
        NamedSolution ns;
        ns.sol = std::move(s);
        ns.name = std::move(name);
        ns.author = std::move(author);
        ns.date = date;
        /* nb. because we don't allow bookmarks on server yet */
        ns.bookmark = false;

        // Downloaded solutions should not be default.
        plr->AddSolution(lmd5, std::move(ns), false);
      }
    }

    return;

  } else {
    Message::no(&td, "Couldn't get solutions");
    return;
  }
}


void Play_::Checkpoint(Solution *saved_sol) {
  *saved_sol = sol;
  saved_sol->Truncate(solpos);
  /* XXX should indicate that something has happened! */
  /* maybe show the checkpoint up at the top-right as a little
     image? */
  // dr.message= "Saved state.";
  watching = false;
}

void Play_::Restore(Extent<Level> &ec,
                    Level *start,
                    const Solution *saved_sol) {
  if (saved_sol->Length() > 0) {
    dr.lev->destroy();
    dr.lev = start->clone();
    ec.replace(dr.lev);

    sol = *saved_sol;
    solpos = sol.Length();

    /* this should stop playing
       as soon as we die or win...
       (This can happen when we edit;
       the level can change while the
       solution persists)
    */
    int moves;
    dr.lev->Play(sol, moves);
    sol.Truncate(moves);
    watching = false;
    redraw();
  }
}

#define MINRATE 100
/* There are two ways to get an event. If there's a
   real event waiting in the queue, then we always
   return that.

   During watch mode, if there is no real event and
   at least MINRATE frames have passed since the
   last time, we can read a move off the redo future
   and play that. Since a redo won't reset the
   future, we can repeat this process until we reach
   the end of the solution.

   So that we can distinguish real keys from such
   fake keys, the argument 'fake' will be set true
   in the second case.
*/
bool Play_::getevent(SDL_Event *e, bool &fake) {
  if (SDL_PollEvent(e)) {
    fake = false;
    return true;
  }
  Uint32 now = SDL_GetTicks();
  if (watching &&
      solpos < sol.Length() &&
      now > nextframe) {
    nextframe = now + MINRATE;
    fake = true;
    /* XXX is this enough to make it a legal key? */
    e->type = SDL_KEYDOWN;
    e->key.keysym.mod = (SDLMod) 0;
    e->key.keysym.unicode = 0;
    switch (sol.At(solpos)) {
    case DIR_UP:    e->key.keysym.sym = SDLK_UP;    break;
    case DIR_DOWN:  e->key.keysym.sym = SDLK_DOWN;  break;
    case DIR_LEFT:  e->key.keysym.sym = SDLK_LEFT;  break;
    case DIR_RIGHT: e->key.keysym.sym = SDLK_RIGHT; break;
    }
    return true;
  } else return false;
}

PlayResult Play_::DoPlaySave(Player *plr, Level *start,
                             Solution *saved, const string &md5) {
  /* we never modify 'start' */
  dr.lev = start->clone();
  Extent<Level> ec(dr.lev);

  std::unique_ptr<Disamb> ctx{Disamb::Create(start)};

  sol.Clear();
  solpos = 0;
  /* if a solution was passed in, use it. */
  Solution saved_sol;
  if (saved != nullptr) saved_sol = *saved;

  dr.scrollx = 0;
  dr.scrolly = 0;
  dr.posx = 0;
  /* room for menu (tiles), then title, and slack.
     (since the drawing has built in slack (margin,
     we correct for that here too) */
  dr.posy = TILEH + fon->height + 4;
  screenresize();

  redraw();

  SDL_Event event;

  dr.message = "";

  /* XX avoid creating if animation is off? */
  std::unique_ptr<Dirt> dirty{Dirt::create()};

  bool do_animate =
    Prefs::getbool(plr, PREF_ANIMATION_ENABLED);

  for (;;) {
    //  while ( SDL_WaitEvent(&event) >= 0 ) {
    SDL_Delay(1);

    bool fake = false;

    /* XXX shut off keyrepeat? */
    while (getevent(&event, fake)) {

      switch (event.type) {
      case SDL_QUIT: goto play_quit;
      case SDL_MOUSEBUTTONDOWN: {
        SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;

        if (e->button == SDL_BUTTON_LEFT) {

          /* clicky? */
          if (e->y <= TILEH + 4) {
            unsigned int targ = (e->x - 2) / TILEW;

            if (targ < NUM_PLAYMENUITEMS) {
              switch (play_menuitem[targ]) {
              case TU_BOOKMARKS:
                Bookmarks(start, ec, plr, md5, &sol);
                break;
              case TU_RESTORESTATE:
                Restore(ec, start, &saved_sol);
                break;
              case TU_UNDO:
                Undo(start, ec, 1);
                break;
              case TU_REDO:
                Redo();
                redraw();
                break;
              case TU_PLAYPAUSE:
                watching = !watching;
                nextframe = 0;
                redraw();
                break;
              case TU_FUNDO:
                Undo(start, ec, 10);
                break;
              case TU_FREDO:
                { for (int i = 0; i < 10; i++) Redo(); }
                redraw();
                break;

              case TU_RESTART:
                Restart(start, ec);
                break;
              case TU_SAVESTATE:
                Checkpoint(&saved_sol);
                break;
              }
            }
          }

          break;
        }
      }
      case SDL_KEYDOWN: {

        auto Won = [this, &saved, &saved_sol]() {
          sol.Truncate(solpos);
          if (saved != nullptr)
            *saved = std::move(saved_sol);
          return PlayResult::Solved(sol);
        };

        switch (event.key.keysym.unicode) {

        case SDLK_ESCAPE:
          switch (CurState()) {
          case PlayState::WON:
            return Won();
          case PlayState::DEAD: /* fallthrough */
          case PlayState::OKAY:
            goto play_quit;
          }
          break;

        case SDLK_t:
          Restart(start, ec);
          break;

        case SDLK_RETURN:
          watching = false;
          switch (CurState()) {
          case PlayState::DEAD: /* fallthrough */
          case PlayState::OKAY:
            Restart(start, ec);
            break;
          case PlayState::WON:
            return Won();
          }
          break;

          /* debugging "cheats" */
        case SDLK_LEFTBRACKET:
        case SDLK_RIGHTBRACKET:
          dr.zoomfactor +=
            (event.key.keysym.unicode == SDLK_LEFTBRACKET) ? +1 : -1;
          if (dr.zoomfactor < 0) dr.zoomfactor = 0;
          if (dr.zoomfactor >= DRAW_NSIZES) dr.zoomfactor = DRAW_NSIZES - 1;

          /* reset animations, since this is a standard trick to disable
             animation temporarily */
          if (dr.zoomfactor == 0)
            do_animate =
              Prefs::getbool(plr, PREF_ANIMATION_ENABLED);

          /* fix scrolls */
          dr.makescrollreasonable();
          redraw();
          break;

        case SDLK_y:
          layer = !layer;
          redraw();
          break;

        case SDLK_b:
          watching = false;
          Bookmarks(start, ec, plr, md5, &sol);
          break;

        case SDLK_c:
        case SDLK_s:
          Checkpoint(&saved_sol);
          break;

        case SDLK_r:
          Restore(ec, start, &saved_sol);
          break;

        case SDLK_d:
          showdests = !showdests;
          redraw();
          break;

        case SDLK_n:
          showbotnums = !showbotnums;
          redraw();
          break;

        case SDLK_u: {
          Undo(start, ec, 1);
          break;
        }

        case SDLK_p: {
          watching = !watching;
          nextframe = 0;
          // printf("Watching: %d\n", (int)watching);
          redraw();
          break;
        }

        case SDLK_o: {
          Redo();
          redraw();
          break;
        }

        default:
          /* no unicode match; try sym */

        switch (event.key.keysym.sym) {

        case SDLK_a:
          // Temporarily toggle animation preference.
          if (event.key.keysym.mod & KMOD_CTRL) {
            do_animate = !do_animate;
          }
          break;

        case SDLK_d:
          if (event.key.keysym.mod & KMOD_CTRL) {
            showdests = true;
            showdestsshuffle = true;
          }
          redraw();
          break;

        case SDLK_o:
          if (event.key.keysym.mod & KMOD_CTRL) {
            for (int i = 0; i < 10; i++) Redo();
            redraw();
          }
          break;

        case SDLK_u:
          if (event.key.keysym.mod & KMOD_CTRL) {
            Undo(start, ec, 10);
            redraw();
          }
          break;

        case SDLK_i: {
          if (event.key.keysym.mod & KMOD_CTRL) {
            /* import moves for solution to different puzzle */
            /* XXX: It's extremely annoying to type in a whole
               MD5 string here. Better would be to use loader, or
               at least require only a unique prefix of the md5. */
            watching = false;
            string answer = Prompt::ask(this,
                                        PICS QICON POP
                                        " What solution? (give md5 of level)");

            if (answer != "") {
              string md;
              if (MD5::UnAscii(answer, md)) {
                if (const Solution *that = plr->GetSol(md)) {
                  /* We now just append this solution but
                     don't "redo" it. The user can do
                     that himself with the VCR. */
                  sol.Truncate(solpos);
                  sol.Appends(*that);

                  redraw();
                } else Message::no(this, "Sorry, not solved!");
              } else Message::no(this, "Bad MD5");
            }

            redraw();
          }
          break;
        }

        /* ctrl-0 in play mode tries to recreate the solution,
           starting from the current position, and using suffixes
           of the bookmarks. */
        case SDLK_0: {
          if (event.key.keysym.mod & KMOD_CTRL) {
            /* discard redo information */
            watching = false;
            printf("Trying to complete..\n");
            sol.Truncate(solpos);
            Solution ss;
            if (Optimize::TryComplete(start, sol,
                                      plr->SolutionSet(md5), &ss)) {
              Message::quick(this, "Completed from bookmarks: " GREEN
                             + itos(ss.Length()) + POP " moves",
                             "OK", "", PICS THUMBICON POP);
              sol = ss;
              /* put us at end of solution */
              solpos = ss.Length();

              dr.lev->destroy();
              dr.lev = start->clone();
              ec.replace(dr.lev);
              int moves;
              dr.lev->Play(sol, moves);

            } else {
              Message::no(this, "Couldn't complete from bookmarks.");
            }
            redraw();
          }
        }

        case SDLK_END: {
          while (Redo()) {}
          redraw();
          break;
        }

        case SDLK_HOME: {
          Restart(start, ec);
          break;
        }

        case SDLK_DOWN:
        case SDLK_UP:
        case SDLK_RIGHT:
        case SDLK_LEFT: {

          if (event.key.keysym.mod & KMOD_CTRL) {
            switch (event.key.keysym.sym) {
            case SDLK_DOWN: dr.scrolly++; break;
            case SDLK_UP: dr.scrolly--; break;
            case SDLK_RIGHT: dr.scrollx++; break;
            case SDLK_LEFT: dr.scrollx--; break;
            default: ; /* impossible */
            }

            redraw();
            break;
          }

          /* can't move when dead or won... */
          if (CurState() != PlayState::OKAY) break;

          /* if it's real, then cancel watching */
          if (!fake) watching = false;

          /* move */
          dir d = DIR_UP;
          switch (event.key.keysym.sym) {
          case SDLK_DOWN: d = DIR_DOWN; break;
          case SDLK_UP: d = DIR_UP; break;
          case SDLK_RIGHT: d = DIR_RIGHT; break;
          case SDLK_LEFT: d = DIR_LEFT; break;
          default: ; /* impossible - lint */
          }

          bool moved;

          if (do_animate && !dr.zoomfactor) {
            moved = AnimateMove(dr, ctx.get(), dirty.get(), d);
          } else {
            moved = dr.lev->Move(d);
          }

          /* now end turn */
          if (moved) {
            /* add to solution, ... */

            /* if we don't have any redo, or the move is different,
               then we lose all redo and add this move */
            if (solpos == sol.Length() || sol.At(solpos) != d) {
              sol.Truncate(solpos);
              sol.Append(d);
            }
            /* and either way, the solution position increases */
            solpos++;

            dr.message = "";
          }

          /* no matter what, redraw so we change the direction
             we're facing */
          redraw();

          break;
        }
        default:;
        }
        }
        break;
      }  // keydown
      case SDL_VIDEORESIZE: {
        SDL_ResizeEvent *eventp = (SDL_ResizeEvent*)&event;
        videoresize(eventp);
        /* sync size of dirty buffer */
        /* XXX move this into member so that we can do it in
           screenresize and use handle_video_events. */
        dirty->matchscreen();
        break;
      }
      case SDL_VIDEOEXPOSE:
        redraw();
        break;
      default: break;
      }
      SDL_Delay(1);
    }

  }
 play_quit:

  if (saved != nullptr) {
    *saved = std::move(saved_sol);
  }
  return PlayResult::Quit();
}

}  // namespace

void Play::playrecord(string res, Player *plr, bool allowrate) {
  /* only prompt to rate if this is in a
     web collection */
  bool iscollection;
  {
    string idx =
      util::pathof(res) + (string)DIRSEP WEBINDEXNAME;
    DirIndex *di = DirIndex::fromfile(idx);
    iscollection = di ? di->webcollection() : false;
    if (di) di->destroy();
  }

  string ss = readfile(res);

  string md5 = MD5::Hash(ss);

  /* load canceled */
  if (ss == "") return;

  Level *lev = Level::fromstring(ss);

  // TODO2016: This whole part could use some work; I simplified
  // it when making Solutions values, but there are some awkward
  // vestiges still. Should happen after fixing ~Level() though.
  if (lev != nullptr) {
    std::unique_ptr<Play> pla{Play::Create()};

    PlayResult res = pla->DoPlay(plr, lev, md5);

    if (res.type == PlayResultType::ERROR ||
        res.type == PlayResultType::EXIT) {
      lev->destroy();
      /* XXX should return something different */
      return;

    } else if (res.type == PlayResultType::QUIT) {
      /* back to level selection */
      lev->destroy();
      return;

    } else if (res.type == PlayResultType::SOLVED) {
      /* write solution, using the MD5 from the file on
         disk--there's no guarantee that md5(lev.ToString())
         will be the same. */

      Solution sol = res.sol;

      /* remove any non-verfying solution first. we don't want to
         erase non-verfying solutions eagerly, since that could cause
         data lossage in the case that a new bugvision of the game
         causes old good solutions to fail. But if we have something
         to replace it with, and the player actively input that new
         solution, then go for it... */

      // Is this our first solution (not including bookmarks)?
      bool firstsol = true;
      {
        const vector<NamedSolution> &oldsols = plr->SolutionSet(md5);
        vector<NamedSolution> keepsols;
        keepsols.reserve(oldsols.size());

        /* now filter just the ones that verify */
        for (const NamedSolution &ns : oldsols) {
          if (Level *check = Level::fromstring(ss)) {
            /* keep bookmarks too, since they basically never verify */
            if (ns.bookmark || Level::Verify(check, ns.sol)) {
              /* already solved it. */
              if (!ns.bookmark) firstsol = false;
              keepsols.push_back(ns);
            }
            check->destroy();
          }
        }

        plr->SetSolutionSet(md5, keepsols);
      }

      // Initialize opt, the solution we'll actually store.
      Solution opt;
      if (Prefs::getbool(plr, PREF_OPTIMIZE_SOLUTIONS)) {
        if (Level *check = Level::fromstring(ss)) {
          opt = Optimize::Opt(check, sol);
          check->destroy();
        } else {
          opt = sol;
        }
      } else {
        opt = sol;
      }

      {
        NamedSolution ns(opt, "Untitled", plr->name, time(0), false);

        plr->AddSolution(md5, std::move(ns), true);
        plr->writefile();
      }

      if (allowrate &&
          plr->webid &&
          firstsol &&
          iscollection &&
          !plr->getrating(md5) &&
          Prefs::getbool(plr, PREF_ASKRATE)) {

        std::unique_ptr<RateScreen> rs{RateScreen::Create(plr, lev, md5)};
        if (rs.get() != nullptr) {
          rs->setmessage(YELLOW
                         "Please rate this level." POP
                         GREY " (You can turn off this "
                         "automatic prompt from the preferences menu.)" POP);

          rs->rate();
        } else {
          Message::bug(0, "Couldn't create rate object!");
        }

      }
      lev->destroy();

      /* load again ... */
      return;
    } else {
      printf("????\n");
      return ;
    }

  } else return;
}

bool Play::AnimateMove(Drawing &dr, Disamb *ctx, Dirt *dirty, dir d) {
  /* events waiting to be turned into animations */
  elist *events = nullptr;
  /* current phase of animation */
  alist *anims = nullptr;
  /* current sprites (drawn on top of everything) */
  alist *sprites = nullptr;


  /* clear sprites from screen.
     sprites are always drawn on top of
     everything, so eagerly clearing
     them here makes sense.

     after we call move, clearsprites
     is worthless to us, since the
     underlying level will have changed. */

  Animation::clearsprites(dr);

  bool success = dr.lev->MoveAnimate(d, ctx, events);

  /* loop playing animations. */
  while (sprites || anims || events) {
    unsigned int now = SDL_GetTicks();

    /* are we animating? if so, trigger
       the next animation action. */
    if (anims || sprites) {

      /* spin-wait until something is ready to think.
         this is the only reason we draw */
      bool ready = false;
      do {
        // printf("  %d Checked!\n", cycle);
        SDL_PumpEvents();
        if (SDL_PeepEvents(dummy, 255, SDL_PEEKEVENT,
                           SDL_EVENTMASK(SDL_KEYDOWN) |
                           SDL_EVENTMASK(SDL_VIDEORESIZE) |
                           SDL_EVENTMASK(SDL_MOUSEBUTTONDOWN) |
                           SDL_EVENTMASK(SDL_QUIT))) {
          // printf("  %d did short circuit\n", cycle);
          /* key waiting. abort! */
          /* empty the lists and be done */
          while (events) delete elist::pop(events);
          while (anims) delete alist::pop(anims);
          while (sprites) delete alist::pop(sprites);

          return success;
        }

        bool only_finales = true;

        for (alist *atmp = anims; atmp && !ready;
             atmp = atmp->next) {
          if (!atmp->head->finale) only_finales = false;
          if (atmp->head->nexttick < now) {
            ready = true;
          }
        }

        for (alist *atmp = sprites; atmp && !ready;
             atmp = atmp->next) {
          if (!atmp->head->finale) only_finales = false;
          if (atmp->head->nexttick < now) {
            ready = true;
          }
        }

        /* if there are only finales, then we are ready.
           (they will do their death wail) */
        if (only_finales) {
          ready = true;
        }

        /* if nothing is ready, then don't chew CPU */
        /* XXX PERF ever delay when animations are going? */
        if (!ready) SDL_Delay(0);
        now = SDL_GetTicks();
      } while (!ready);

      /*

      the new regime is this:

      in each loop,
      - erase all animations.
      - if any animation is ready to
      think, make it think.
      (the animation may 'become'
      a different animation at
      this point; call its init
      method)
      - draw all active animations.
      - draw all active sprites on
      top of that.

      */

      Animation::erase_anims(anims, dirty);
      Animation::erase_anims(sprites, dirty);

      /* reflect dirty rectangles action */
      dirty->clean();

      bool remirror = false;

      Animation::think_anims(&anims, now, remirror);
      Animation::think_anims(&sprites, now, remirror, !anims);

      /* might need to save new background */
      if (remirror) dirty->mirror();

      /* now draw everything, but only if
         there is something left. */

      /* printf("\n == draw cycle == \n"); */
      if (anims || sprites) {

        /* Sort animations by y-order every time! */
        alist::sort(Animation::yorder_compare, anims);
        alist::sort(Animation::yorder_compare, sprites);

        Animation::draw_anims(anims);
        Animation::draw_anims(sprites);

        SDL_Flip(screen);

        POSTDRAW ;
        /* if (pref_debuganim) SDL_Delay(80); */
      }

    } else {
      /* no? then we should have some events queued
         up to deal with. */

      if (events) {
        unsigned int s = events->head->serial;
        /* push all events with the same serial */
        // printf("** doing serial %d\n", s);
        while (events && events->head->serial == s) {
          std::unique_ptr<aevent> ee{elist::pop(events)};
          Animation::start(dr, anims, sprites, ee.get());
        }

        if (anims || sprites) {
          bool domirror = false;
          domirror = Animation::init_anims(anims, now) || domirror;
          domirror = Animation::init_anims(sprites, now) || domirror;

          /* PERF check return code to be more efficient */
          dirty->mirror();
        }
      }
    }
  }

  return success;
}

Play::~Play() {}

Play *Play::Create() {
  return Play_::Create();
}

