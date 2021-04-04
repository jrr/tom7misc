#include "handhold.h"

#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"

#include "escape-util.h"
#include "escapex.h"

#include "time.h"
#include "message.h"

#include "bytes.h"

/* try to avoid being annoying if something is wrong */
static bool hh_ok = false;

static int hh_lastupdate = 0;
static int hh_lastupgrade = 0;

/* XXX ??? */
/* static int hh_lasttutorial = 0; */

#define HANDHOLD_MAGIC "ESXH"
#define HANDHOLD_FILE "history.esd"

#define UPDATE_INTERVAL ((24 * (60 * (60 /* minutes */) /* hours */) /* days */) * 14)
#define UPGRADE_INTERVAL (UPDATE_INTERVAL * 2)

static bool hh_write() {
  return writefile(HANDHOLD_FILE,
                   (string)HANDHOLD_MAGIC +
                   BigEndian32(hh_lastupdate) +
                   BigEndian32(hh_lastupgrade));
}

void HandHold::init() {
  string hh = EscapeUtil::readfilemagic(HANDHOLD_FILE, HANDHOLD_MAGIC);

  unsigned int idx = strlen(HANDHOLD_MAGIC);
  if (hh.length() == (idx + (2 * 4))) {
    hh_lastupdate = ReadBigEndian32(hh, idx);
    hh_lastupgrade = ReadBigEndian32(hh, idx);
    hh_ok = true;
  } else {
    /* well, try making it! */
    hh_lastupdate = time(0);
    hh_lastupgrade = time(0);

    hh_ok = hh_write();
  }
}

void HandHold::firsttime() {
  Message::Quick(0,
                 GREEN "Welcome to Escape!\n"
                 "\n"
                 "You should start by creating a new player.\n"
                 "    " GREY "(on the next screen)" POP "\n"
                 "\n"
                 "Escape has a number of internet features. If\n"
                 "you're connected, it is recommended that you do\n"
                 "this stuff before playing:\n"
                 "\n"
                 "  " PICS ARROWR POP
                 " Register your player with the server.\n"
# ifndef MULTIUSER
                 "  " PICS ARROWR POP " Upgrade Escape (if available).\n"
# endif
                 "  " PICS ARROWR POP " Get any new levels (if available).\n"
                 "\n"
                 "You can do each of these from the main menu."
                 "\n ",
                 "Play the game!",
                 "", PICS EXCICON POP);

  /* XXX could use build date here */
  hh_lastupdate = 0;
  hh_lastupgrade = 0;
  if (hh_write()) hh_ok = true;
}


void HandHold::did_update() {
  hh_lastupdate = time(0);
  hh_write();
}

void HandHold::did_upgrade() {
  hh_lastupgrade = time(0);
  hh_write();
}


bool HandHold::recommend_update() {
  return hh_ok && (hh_lastupdate < (time(0) - UPDATE_INTERVAL));
}

bool HandHold::recommend_upgrade() {
# ifdef MULTIUSER
  return false;
# else
  return hh_ok && (hh_lastupgrade < (time(0) - UPGRADE_INTERVAL));
# endif
}
