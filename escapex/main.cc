#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"

#include "util.h"
#include "escapex.h"
#include "font.h"
#include "loadlevel.h"
#include "player.h"
#include "../cc-lib/md5.h"
#include "prompt.h"
#include "draw.h"
#include "playerdb.h"
#include "message.h"

#include "edit.h"
#include "play.h"
#include "upgrade.h"
#include "update.h"
#include "mainmenu.h"
#include "registration.h"
#include "cleanup.h"
#include "prefs.h"
#include "dircache.h"

#include "handhold.h"
#include "animation.h"
#include "sound.h"
#include "startup.h"
#include "leveldb.h"
#include "progress.h"
#include "browse.h"

#define DEFAULT_DIR "."
#define SPLASH_FILE DATADIR "splash.png"
#define ICON_FILE DATADIR "icon.png"

/* for debugging, turn on noparachute */
// #define DEBUG_PARACHUTE 0
#define DEBUG_PARACHUTE SDL_INIT_NOPARACHUTE

int main(int argc, char **argv) {

  /* change to correct location, initializing it
     if we are in MULTIUSER mode and this is the
     first launch */
  if (!StartUp::setdir(argc, argv)) {
    exit(-1);
  }

  // util::setclipboard("hello escapists");

  /* set up md5 early, before any threads */
  MD5::Init();

  Drawable::init();

  /* clean up any stray files */
  Cleanup::clean();

  audio = 0;
  network = 0;
  if (SDL_Init(SDL_INIT_VIDEO |
               SDL_INIT_TIMER |
               SDL_INIT_AUDIO | DEBUG_PARACHUTE) < 0) {

    /* try without audio */
    if (SDL_Init(SDL_INIT_VIDEO |
                 SDL_INIT_TIMER | DEBUG_PARACHUTE) < 0) {

      printf("Unable to initialize SDL. (%s)\n", SDL_GetError());

      return 1;

    } else {
      audio = 0;
    }
  } else {
    audio = 1;
  }

  if (SDLNet_Init() == -1) {
    network = 0;
    printf("(debug) SDLNet_Init: %s\n", SDLNet_GetError());
  } else {
    network = 1;
  }

  Sound::init();

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  /* set caption and icon */
  {
    printf("Welcome to escape " VERSION ".\n");
    SDL_WM_SetCaption("escape " VERSION, "");
    SDL_Surface *icon = sdlutil::LoadImage(ICON_FILE);
    if (icon) SDL_WM_SetIcon(icon, 0);
    /* XXX free icon? It's not clear where we
       can safely do this. */
  }

  screen = sdlutil::makescreen(STARTW, STARTH);

  if (!screen) {
    printf("Failed to create screen.\n");
    goto no_drawings;
  }

  /* draw splash while loading images. animation init
     takes some time! */

  {
    SDL_Surface *splash = sdlutil::LoadImage(SPLASH_FILE);
    if (splash) {
      SDL_Rect dst;
      dst.x = 2;
      dst.y = screen->h - (splash->h + 2);
      SDL_BlitSurface(splash, 0, screen, &dst);
      SDL_Flip(screen);

      SDL_FreeSurface(splash);

    }

  }

  /* XXX callback progress for ainit */
  if (!Drawing::loadimages() || !Animation::ainit()) {
    if (fon) Message::bug(0, "Error loading graphics!");
    printf("Failed to load graphics.\n");
    goto no_drawings;
  }


# ifdef WIN32
  /* XXX this could display for unix and osx too, direct from the exec */
  /* Windows has some weird command line args stuff for an executable
     upgrading itself. See upgrade.cc and replace.cc for
     description. */

  if (argc == 2 &&
      !strcmp(argv[1], "-upgraded")) {

    Message::quick(0, "Upgrade to version " VERSION " successful!",
                   "OK", "", PICS THUMBICON POP);

  }

# endif

  /* before we go to the player database, get rid of bad
     mousemotion events that the queue starts with. */
  sdlutil::eatevents(30, SDL_EVENTMASK(SDL_MOUSEMOTION));
  /* this may or may not be a good idea, now */
  // SDL_WarpMouse((Uint16)(screen->w * 0.75f), 8);


  /* The "real" main loop. */
  /* XXX should put the following in some other function. */
  {
    std::unique_ptr<Player> plr;
    {
      std::unique_ptr<PlayerDB> pdb{PlayerDB::create()};
      if (pdb.get() == nullptr) {
        Message::bug(0, "Error in playerdb?");
        goto oops;
      }

      /* If there are no players, assume this is the
         first launch. */
      if (pdb->firsttime()) {
        HandHold::firsttime();
      }

      plr.reset(pdb->chooseplayer());
    }

    if (!plr.get()) goto oops;

    /* selected player. now begin the game. */

    std::unique_ptr<MainMenu> mm{MainMenu::Create(plr.get())};
    if (mm.get() == nullptr) {
      Message::bug(0, "Error creating main menu");
      goto oops;
    }

    // XXX here?
    LevelDB::setplayer(plr.get());
    LevelDB::addsourcedir("triage");
    LevelDB::addsourcedir("mylevels");
    LevelDB::addsourcedir("official");

    for (;;) {
      MainMenu::result r = mm->show();

      if (r == MainMenu::LOAD) {
        /* load and play levels */

        /* we stay in 'load' mode until
           the user hits escape on the load screen. */
        for (;;) {
          std::unique_ptr<LoadLevel> ll{
            LoadLevel::Create(plr.get(), DEFAULT_DIR, true, false)};
          if (ll.get() != nullptr) {
            string res = ll->selectlevel();

            Play::playrecord(res, plr.get());

            if (res == "") break;
          } else {
            Message::bug(0, "Error creating load screen");
            break;
          }
        }

      } else if (r == MainMenu::LOAD_NEW) {

        // FIXME not finished, doesn't work
        for (;;) {// XXX loop in browser instead.
          std::unique_ptr<Browse> bb{Browse::Create()};
            if (bb.get() != nullptr) {
            // XXX: Instead, have a version of the browser
            // that invokes playrecord on the stack, and a
            // separate bb->selectfile() for editing.
            string res = bb->selectlevel();
            if (res.empty()) break;

            Play::playrecord(res, plr.get());

          } else {
            Message::bug(0, "Error creating browser");
            break;
          }
        }

      } else if (r == MainMenu::EDIT) {
        /* edit a level */

        std::unique_ptr<Editor> ee{Editor::Create(plr.get())};
        if (ee.get() == nullptr) {
          Message::bug(0, "Error creating Editor");
          goto oops;
        }

        ee->edit();

#     ifndef MULTIUSER
      } else if (r == MainMenu::UPGRADE) {
        /* upgrade escape binaries and graphics */

        std::unique_ptr<Upgrader> uu{Upgrader::Create(plr.get())};
        if (!uu) {
          Message::bug(0, "Error creating upgrader");
          goto oops;
        }

        string msg;

        switch (uu->upgrade(msg)) {
        case UP_EXIT:
          /* force quit */
          goto done;
        default:
          break;
        }
#     endif

      } else if (r == MainMenu::UPDATE) {
        /* update levels */

        std::unique_ptr<Updater> uu{Updater::Create(plr.get())};
        if (uu.get() == nullptr) {
          Message::bug(0, "Error creating updater");
          goto oops;
        }

        string msg;

        uu->update(msg);

      } else if (r == MainMenu::REGISTER) {
        /* register player online */

        std::unique_ptr<Registration> rr{Registration::Create(plr.get())};
        if (!rr.get()) {
          Message::bug(0, "Couldn't create registration object");
          goto oops;
        }

        rr->registrate();

      } else if (r == MainMenu::QUIT) {
        break;
      }

    }
  }
#ifndef MULTIUSER
 done: ;
#endif

 oops: ;
  Drawing::destroyimages();


 no_drawings: ;
  Sound::shutdown();
  SDL_Quit();

  return 0;
}
