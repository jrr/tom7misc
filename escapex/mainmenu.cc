
#include "mainmenu.h"

#include <string>
#include <memory>

#include "draw.h"
#include "version.h"
#include "prefs.h"
#include "chars.h"
#include "client.h"
#include "loadlevel.h"
#include "message.h"
#include "play.h"
#include "handhold.h"
#include "backgrounds.h"
#include "../cc-lib/util.h"

/* testing */
#include "animation.h"

#define TUTORIAL_DIR "official" DIRSEP "tutorial"
#define TITLE_FILE DATADIR "title.png"
#define TITLE_OFFSET 16

#define SHOWY (400 - 32)

#define TEXTOFFSET (titlegraphic->h + 8 + 32)
#define TEXTX ((screen->w - titlegraphic->w) >> 1)
#define RECOMMENDED_TEXT YELLOW "Recommended " POP PICS ARROWR POP " "

// Note: It doesn't work at all yet; don't bother :)
#define ENABLE_NEW_LEVEL_BROWSER 0

namespace {

enum class MMEType {
  TUTORIAL,
  LOAD,
  EDIT,
  UPGRADE,
  UPDATE,
  PREFS,
  QUIT,
  LOAD_NEW,
#define NUM_ITEMS 8
};

struct MainMenu_;

struct MMEntry {
  MainMenu_ *parent;
  MMEType t;
  static int height() { return TILEH - 8; }
  MMEType convert() { return t; }
  MMEType none() { return MMEType::QUIT; }
  void swap(MMEntry *, MMEntry *) { /* unnecessary, no sorting */ }
  /* no matches, because we catch these keys ourselves */
  bool matches(char) { return false; }
  void draw(int x, int y, bool sel);
};

using MSel = Selector<MMEntry, MMEType>;

struct MainMenu_ : public MainMenu, public Drawable {
  MainMenu::Result Show() override;

  static MainMenu_ *Create(Player *plr);

  /* for Drawable */
  void Draw() override;
  void ScreenResize() override;

  ~MainMenu_() override;

 private:
  friend struct MMEntry;

  void Redraw() {
    sel->Draw();
    SDL_Flip(screen);
  }

  std::unique_ptr<MSel> sel;

  Player *pp;
  SDL_Surface *titlegraphic;
  SDL_Surface *background;

  std::unique_ptr<MainShow> mshow;

  void MakeBackground();
  void ComputeTutorial();
  bool tutorial_left;
  string tutorial_text;
  string tutorial_nextlev;

  void playtutorial() {

    if (tutorial_left &&
        tutorial_nextlev != "") {

      Play::PlayRecord(tutorial_nextlev, pp, false);
      ComputeTutorial();

    } else {
      Message::Quick(this, "Tutorial completed or unavailable!",
                     "Sorry", "", PICS XICON POP);
    }

  }

  int startx() {
    /* XXX ?!? */
    return TEXTX + fon->sizex(RECOMMENDED_TEXT);
  }

};

void MMEntry::draw(int x, int y, bool sel) {

  int sxi = parent->startx();
  int sx = sxi + TILEW + 16;
  /* recommended text */
  int sxr = sxi - (fon->sizex(RECOMMENDED_TEXT) + 16);

  y -= 4;
  int ctry = y + ((height() >> 1) - (fon->height >> 1)) + 4;

  switch (t) {
  case MMEType::TUTORIAL:
    /* figure out what the next unsolved tutorial level is */
    Drawing::DrawTileU(sxi, y, TU_T, 0, screen);
    if (parent->tutorial_left)
      fon->draw(sx, ctry, parent->tutorial_text);
    else
      fon->draw(sx, ctry, GREY + parent->tutorial_text);
    break;
  case MMEType::LOAD:
    Drawing::DrawTileU(sxi, y, TU_1, 0, screen);
    fon->draw(sx, ctry, YELLOW "Load a level.");
    break;

  case MMEType::LOAD_NEW:
    Drawing::DrawTileU(sxi, y, TU_LOAD, 0, screen);
    fon->draw(sx, ctry, YELLOW "New level browser!");
    break;

  case MMEType::EDIT:
    Drawing::DrawTileU(sxi, y, TU_2, 0, screen);
    fon->draw(sx, ctry, "Edit a level.");
    break;

# ifndef MULTIUSER
  case MMEType::UPGRADE:
    Drawing::DrawTileU(sxi, y, TU_3, 0, screen);
    if (HandHold::recommend_upgrade())
      fon->draw(sxr, ctry, RECOMMENDED_TEXT);

    fon->draw(sx, ctry, "Upgrade Escape from internet.");
    break;
# endif

  case MMEType::UPDATE:
    Drawing::DrawTileU(sxi, y, TU_4, 0, screen);
    /* don't show more than one recommendation */
    if (HandHold::recommend_update() && !HandHold::recommend_upgrade())
      fon->draw(sxr, ctry, RECOMMENDED_TEXT);

    fon->draw(sx, ctry, "Get new levels from internet.");
    break;

  case MMEType::PREFS:
    Drawing::DrawTileU(sxi, y, TU_P, 0, screen);
    fon->draw(sx, ctry, "Change preferences.");
    break;
  case MMEType::QUIT:
    Drawing::DrawTileU(sxi, y, TU_X, 0, screen);
    fon->draw(sx, ctry, "Quit.");
    break;


  default:
    fon->draw(sx, y, RED "???");
  }
}


void MainMenu_::ComputeTutorial() {
  // XXX use leveldb for this.

  std::unique_ptr<LoadLevel> ll{
    LoadLevel::Create(pp, TUTORIAL_DIR, false, false)};
  if (ll.get() == nullptr) {
    tutorial_left = false;
    tutorial_nextlev = "error.esx";
    tutorial_text = "(" RED "Tutorial missing!" POP ")";
    return;
  }

  /* search through loader for first unsolved level */
  tutorial_left = ll->FirstUnsolved(tutorial_nextlev, tutorial_text);
  if (!tutorial_left) tutorial_text = "Tutorial complete.";
}

void MainMenu_::Draw() {
  SDL_BlitSurface(background, 0, screen, 0);

  /* draw status info at the bottom */
  int y = screen->h - (int)(fon->height * 3.2f);

  if (pp->webid) {
    fon->draw(6, y += fon->height,
              (string)BLUE + pp->name + (string)GREY " is Player #" POP +
              Util::itos(pp->webid) + POP);
  } else {
    if (network) {
      fon->draw(6, y += fon->height, WHITE
                "Press [" YELLOW "R" POP "] to register your player online!"
                POP);
    } else {
      fon->draw(6, y += fon->height, GREY
                "(connect to the internet to register your player online)"
                POP);
    }
  }

  y += fon->height;
  fon->draw(6, y,
            PICS BARLEFT BAR BAR BARRIGHT POP
            BLUE "ESCAPE " POP GREY "Version " VERSION POP
            PICS BARLEFT BAR BAR BARRIGHT POP);

  {
    string web = GREY "http://" BLUE DEFAULT_SERVER POP "/";
    fon->draw(screen->w - fon->sizex(web) - 6, y, web);
  }

  mshow->draw((screen->w - mshow->width()) >> 1, SHOWY, screen);
}


#define FRAME_TICKS 500
MainMenu::Result MainMenu_::Show() {
  ComputeTutorial();

  MakeBackground();
  Redraw();

  SDL_Event e;

  Uint32 nextframe = SDL_GetTicks() + FRAME_TICKS;
  for (;;) {
    SDL_Delay(1);

    /* turn on animation? */
    Uint32 now = SDL_GetTicks();

    if (now > nextframe) {
      mshow->step();
      nextframe = now + FRAME_TICKS;
      Redraw();
    }

    while (SDL_PollEvent(&e)) {
      int key;

      if (HandleVideoEvent(this, e)) continue;

      switch (e.type) {
      case SDL_QUIT:
        return QUIT;

      case SDL_KEYDOWN:
        key = e.key.keysym.sym;
        switch (key) {

        case SDLK_ESCAPE:
        case SDLK_x:
          return QUIT;

        case SDLK_l:
        case SDLK_1:
          return LOAD;

        case SDLK_b:
          return LOAD_NEW;

        case SDLK_e:
        case SDLK_2:
          return EDIT;

#       ifndef MULTIUSER
        case SDLK_u:
        case SDLK_3:
          if (network) return UPGRADE;
          else continue;
#       endif

        case SDLK_g:
        case SDLK_4:
          if (network) return UPDATE;
          else continue;

        case SDLK_t: {
          /* XXX shouldn't this be a return TUTORIAL or whatever? */
          playtutorial();
          Redraw();
          continue;
        }

        case SDLK_p: {
          Prefs::Show(pp);
          Redraw();
          continue;
        }

        case SDLK_r: {
          if (network && !pp->webid) return REGISTER;
          else continue;
        }

        default: break;

        }
      default: break;
      }

      /* if we got here, then we don't know how to
         preempt the event, so use the selector. */
      MSel::PERes pr = sel->DoEvent(e);
      switch (pr.type) {
      case MSel::PEType::SELECTED:
        switch (sel->items[sel->selected].t) {
        case MMEType::TUTORIAL:
          playtutorial();
          Redraw();
          continue;
        case MMEType::LOAD: return LOAD;
        case MMEType::LOAD_NEW: return LOAD_NEW;
        case MMEType::EDIT: return EDIT;
        case MMEType::QUIT: return QUIT;
        case MMEType::UPGRADE:
	  if (network) return UPGRADE;
	  else continue;
        case MMEType::UPDATE:
	  if (network) return UPDATE;
	  else continue;
        case MMEType::PREFS:
	  Prefs::Show(pp);
	  Redraw();
	  continue;
        default: break;
        }
        /* ??? */
        break;
        /* FALLTHROUGH */
      case MSel::PEType::EXIT:
      case MSel::PEType::CANCEL:
        return QUIT;
      default:
      case MSel::PEType::NONE:
	break;
      }
    }
  }

  return QUIT;
}

void MainMenu_::ScreenResize() {
  MakeBackground();
}

/* the background is pretty complex, so we precompute it */
void MainMenu_::MakeBackground() {
  int w = screen->w;
  int h = screen->h;

  Backgrounds::GradientBlocks(background,
                              T_GREY,
                              T_BLUE,
                              Backgrounds::blueish);
  if (!background) return;

  /* draw alpharect for bottom */
  int botoff = fon->height * 2 + 4;
  SDL_Surface *bot = sdlutil::makealpharect(w, botoff,
                                             0, 0, 0, 120);
  if (bot) {
    SDL_Rect dest;
    dest.x = 0;
    dest.y = h - (botoff + 1);
    SDL_BlitSurface(bot, 0, background, &dest);
    SDL_FreeSurface(bot);
  }

  /* bg for titlegraphic */
  bot = sdlutil::makealpharect(w, titlegraphic->h + 16, 0, 0, 0, 120);
  if (bot) {
    SDL_Rect dest;
    dest.x = 0;
    dest.y = TITLE_OFFSET - 8;
    SDL_BlitSurface(bot, 0, background, &dest);
    SDL_FreeSurface(bot);
  }

  /* title graphic */
  {
    int sx = TEXTX;
    SDL_Rect dest;
    dest.x = sx;
    dest.y = 16;

    SDL_BlitSurface(titlegraphic, 0, background, &dest);
  }

  /* and the background for the text */
  bot = sdlutil::makealpharect(w,
                               16 +
                               sel->number *
                               MMEntry::height(),
                               0, 0, 0, 120);

  if (bot) {
    SDL_Rect dest;
    dest.x = 0;
    dest.y = TEXTOFFSET - 8;
    SDL_BlitSurface(bot, 0, background, &dest);
    SDL_FreeSurface(bot);
  }
}


MainMenu_ *MainMenu_::Create(Player *plr) {
  std::unique_ptr<MainMenu_> mm{new MainMenu_};
  if (mm.get() == nullptr) return nullptr;
  mm->titlegraphic = 0;
  mm->background = 0;

  mm->titlegraphic = sdlutil::LoadImageFile(TITLE_FILE);

  if (!mm->titlegraphic) return 0;

  mm->pp = plr;

  mm->tutorial_left = false;

  mm->mshow = MainShow::Create(18, 10, 1);


  /* set up selector... */
  mm->sel = MSel::Create(NUM_ITEMS);
  mm->sel->below = mm.get();
  /* XXX should be a better way to do this.
     (should really get it from titlegraphic, for one) */
  mm->sel->title = "\n\n\n\n\n\n\n\n\n\n";
  for (int j = 0; j < NUM_ITEMS; j++) {
    mm->sel->items[j].parent = mm.get();
  }

  int i = 0;

  mm->sel->selected = 1;
  if (Prefs::GetBool(plr, PREF_SHOWTUT)) {
    mm->sel->items[i++].t = MMEType::TUTORIAL;

    /* select tutorial if there's something left. */
    mm->ComputeTutorial();
    if (mm->tutorial_left)
      mm->sel->selected = 0;
  }

  mm->sel->items[i++].t = MMEType::LOAD;
  if (ENABLE_NEW_LEVEL_BROWSER) {
    mm->sel->items[i++].t = MMEType::LOAD_NEW;
  }
  mm->sel->items[i++].t = MMEType::EDIT;
  if (network) {
#   ifndef MULTIUSER
    mm->sel->items[i++].t = MMEType::UPGRADE;
#   endif
    mm->sel->items[i++].t = MMEType::UPDATE;
  }

  mm->sel->items[i++].t = MMEType::PREFS;
  mm->sel->items[i++].t = MMEType::QUIT;


  /* FIXME the rest ... */

  /* maybe fewer, if some were removed */
  mm->sel->number = i;
  return mm.release();
}

MainMenu_::~MainMenu_() {
  if (titlegraphic) SDL_FreeSurface(titlegraphic);
  if (background) SDL_FreeSurface(background);
}

}  // namespace

MainMenu::~MainMenu() {}

MainMenu *MainMenu::Create(Player *plr) {
  return MainMenu_::Create(plr);
}
