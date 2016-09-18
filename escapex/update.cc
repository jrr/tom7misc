
#include "update.h"
#include "util.h"
#include "textscroll.h"
#include "prompt.h"
#include "message.h"
#include "chars.h"
#include "upper.h"
#include "handhold.h"
#include "menu.h"
#include "ptrlist.h"

#include <memory>
#include <string>

#include "client.h"

#define UNSUBMARKER "unsubscribed"
#define SHOWRATE 500

namespace {

/* result of checkcollections */
enum class CCResult {
  FAIL, OK,
};

enum class SelResult {
  FAIL, OK,
};

struct Updater_ : public Updater {
  static Updater_ *Create(Player *p);

  UpdateResult update(string &msg) override;

  void draw() override;
  void screenresize() override;

  void redraw();

  private:

  void say(const string &s) {
    if (tx.get() != nullptr) tx->say(s);
  }

  void sayover(const string &s) {
    if (tx.get() != nullptr) {
      tx->unsay();
      tx->say(s);
    }
  }

  Player *plr;

  std::unique_ptr<TextScroll> tx;

  CCResult checkcolls(HTTP *hh,
                      stringlist *&fnames,
                      stringlist *&shownames);

  SelResult selectcolls(stringlist *fnames,
                        stringlist *shownames,
                        stringlist *&, stringlist *&);

  void updatecoll(HTTP *hh, string fname, string showname);

};

/* version of toggle where toggling causes the
   subscription file to be written/removed */
struct subtoggle : public Toggle {
  string fname;

  virtual InputResult key(SDL_Event e);
  virtual InputResult click(int x, int y);

  void docheck();

};

void subtoggle::docheck() {
    if (!checked) {

      /* try to subscribe */
      if (util::existsdir(fname)) {
        /* delete unsub file from dir */
        if (util::remove(fname + (string)DIRSEP + UNSUBMARKER)) {

          checked = true;

        } else {

          Message::Quick(0, (string)"Can't subscribe to "
                         BLUE + fname + (string)POP " (remove unsub file)",
                         "Cancel", "", PICS XICON POP);

        }
      } else {
        /* create subscription (directory) */
        if (util::makedir(fname)) {
          checked = true;

        } else {
          Message::Quick(0, (string)"Can't subscribe to "
                         BLUE + fname + (string)POP " (can't make dir!!)",
                         "Cancel", "", PICS XICON POP);
        }
      }
    } else {
      /* try to unsubscribe */

      if (writefile(fname + (string)DIRSEP + UNSUBMARKER,
                    (string)"delete this file to resubscribe to " + fname +
                    (string)"\n")) {

        checked = false;

      } else {
        Message::Quick(0, (string)"Can't unsubscribe to "
                       BLUE + fname +
                       (string)POP " (can't make unsub file!)",
                       "Cancel", "", PICS XICON POP);
      }
    }
}

InputResult subtoggle::click(int, int) {
  docheck();
  return InputResult(InputResultKind::UPDATED);
}

InputResult subtoggle::key(SDL_Event e) {
  int kk = e.key.keysym.sym;

  switch (kk) {
  case SDLK_RETURN:
  case SDLK_SPACE:
    docheck();
    return InputResult(InputResultKind::UPDATED);
  default: return MenuItem::key(e);
  }

}

Updater_ *Updater_::Create(Player *p) {
  Updater_ *uu = new Updater_();
  uu->tx.reset(TextScroll::Create(fonsmall));
  uu->tx->posx = 5;
  uu->tx->posy = 5;
  uu->tx->width = screen->w - 10;
  uu->tx->height = screen->h - 10;
  uu->tx->vskip = 2;
  uu->plr = p;
  return uu;
}

void Updater_::redraw() {
  draw();
  SDL_Flip(screen);
}

/* return the available collections.
   (by adding items to fnames, shownames)
*/
CCResult Updater_::checkcolls(HTTP *hh,
                              stringlist *&fnames,
                              stringlist *&shownames) {
  /* first, grab COLLECTIONS. */

  string s;
  httpresult hr = hh->get(COLLECTIONSURL, s);

  if (hr == HT_OK) {
    /* parse result. see protocol.txt */
    int ncolls = util::stoi(util::getline(s));

    say((string)BLUE + itos(ncolls) + (string)" collection" +
        (string)((ncolls==1)?"":"s") + (string)":" POP);

    /* then, ncolls collections */
    while (ncolls--) {
      string line = util::getline(s);

      string fname = util::chop(line);
      int minv  = util::stoi(util::chop(line));
      string showname = line;

      int usable = util::stoi(VERSION) >= minv;

      if (fname == "") {
        say(RED "  collections file corrupt!!" POP);
        return CCResult::FAIL;
      }

      say((string)"  " YELLOW + fname + POP +
          (string)(usable? " " GREEN : " " RED) +
          itos(minv) + (string)POP " " BLUE + showname + POP);

      if (usable) {
        stringlist::push(fnames, fname);
        stringlist::push(shownames, showname);
      }
    }

    return CCResult::OK;

  } else {
    say("Message from server: " RED + s);
    say(RED "Unable to fetch collection list." POP);
    return CCResult::FAIL;
  }

}


/* invt: length(fnames) > 0 */

SelResult Updater_::selectcolls(stringlist *fnames,
                                stringlist *shownames,
                                stringlist *&subsf,
                                stringlist *&subss) {

  PtrList<MenuItem> *boxes = nullptr;

  Okay ok;
  ok.text = "Update Now";

  Cancel can;

  VSpace v(16);

  stringlist *fnt = fnames;
  stringlist *snt = shownames;

  int n_entries = 0;

  while (fnt && snt) {
    subtoggle *b = new subtoggle();
    b->fname = fnt->head;
    b->question = snt->head;

    /* check subscribed:
       for collection 'triage' we are
       subscribed if 'triage' subdir exists
       AND 'triage/unsubscribed' does not exist */
    if (util::existsdir(fnt->head)) {
      if (util::existsfile(fnt->head + (string)DIRSEP + UNSUBMARKER)) {
        b->checked = false;
      } else {
        b->checked = true;
      }
    } else {
      b->checked = false;
    }

    PtrList<MenuItem>::push(boxes, b);
    n_entries++;

    fnt = fnt->next;
    snt = snt->next;
  }

  PtrList<MenuItem>::push(boxes, &v);
  PtrList<MenuItem>::push(boxes, &can);
  PtrList<MenuItem>::push(boxes, &ok);


  std::unique_ptr<Menu> mm =
    Menu::Create(this, "Select your collection subscriptions.", boxes, false);
  if (mm.get() == nullptr) return SelResult::FAIL;

  InputResultKind res = mm->menuize();
  mm.reset();

  if (res == InputResultKind::OK) {
    /* process selections from 'boxes' list */

    subsf = 0;
    subss = 0;

    /* skip first three, the buttons */
    for (int z = 0; z < 3; z++) {
      PtrList<MenuItem>::pop(boxes);
    }

    for (int i = 0; i < n_entries; i++) {
      subtoggle *st = (subtoggle*)PtrList<MenuItem>::pop(boxes);
      if (st->checked) {
        stringlist::push(subsf, st->fname);
        stringlist::push(subss, st->question);
      }

      delete st;
    }

    return SelResult::OK;
  } else {
    return SelResult::FAIL;
  }

}

/* update a single collection "fname" using http connection hh. */
void Updater_::updatecoll(HTTP *hh, string fname, string showname) {

  say("");
  say((string)"Updating " BLUE + showname + (string)POP " (" YELLOW +
      fname + (string)POP") ...");
  say("");

  string s;
  httpresult hr = hh->get((string)"/" + fname + (string) ".txt", s);

  if (hr == HT_OK) {
    /* parse result. see protocol.txt */

    string showname = util::getline(s);
    int minv = util::stoi(util::getline(s));
    int ndirs = util::stoi(util::getline(s));
    int nfiles = util::stoi(util::getline(s));
    string version = util::getline(s);

    if (util::stoi(VERSION) < minv) {
      say(RED "?? Server inconsistent: This escape version is too old!" POP);
      /* fail */
      return;
    }

    /* create upper for this collection dir. */
    std::unique_ptr<Upper> up{Upper::Create(hh, tx.get(), this, fname)};

    if (!up.get()) {
      Message::Bug(this, "couldn't create upper object?!");
      return;
    }

    /* always save the root dir */
    up->savedir("", showname);

    say("ndirs: " + itos(ndirs));

    while (ndirs--) {
      string line = util::getline(s);

      string d = util::chop(line);
      string name = util::losewhitel(line);

      /* sanity check d. */
      if (d.length() < 1 || d[0] == '/' ||
          d.find("..") != string::npos ||
          d.find("//") != string::npos ||
          /* yes, I mean one backslash */
          d.find("\\") != string::npos) {
        Message::No(this, "Bad directory name in collection: " RED + d);
        return;
      }

      /* on win32, rewrite / to \ */
      util::replace(d, "/", DIRSEP);

      up->savedir(d, name);
    }

    say("nfiles: " + itos(nfiles));

    int epoch = SDL_GetTicks();

    /* then, ncolls collections */
    while (nfiles--) {
      string line = util::getline(s);

      /* printf("line: '%s'\n", line.c_str()); */

      string f = util::chop(line);
      string md = util::chop(line);

      /* sanity check f. This may be relative
         to the current directory, so it is the
         same condition as above */
      if (f.length() < 1 || f[0] == '/' ||
          f.find("..") != string::npos ||
          f.find("//") != string::npos ||
          f.find("\\") != string::npos) {
        Message::No(this, "Bad file name in collection: " RED + f);
        return;
      }


      RateStatus votes;

      votes.nvotes = util::stoi(util::chop(line));
      votes.difficulty = util::stoi(util::chop(line));
      votes.style = util::stoi(util::chop(line));
      votes.rigidity = util::stoi(util::chop(line));
      votes.cooked = util::stoi(util::chop(line));
      votes.solved = util::stoi(util::chop(line));

      int date = util::stoi(util::chop(line));
      int speedrecord = util::stoi(util::chop(line));
      int owner = util::stoi(util::chop(line));

      /* require file and md5, but not any of the voting stuff
         (they will default to 0) */
      if (f != "" && md != "" && up->setfile(f, md, votes,
                                             date, speedrecord, owner)) {
        /* XXX change to sayover? */
        /* say((string)GREEN + f + (string)" " GREY + md + POP POP); */

      } else {
        say((string)RED + f + (string)" " GREY + md +
            (string)POP " (error!)" POP);

        Message::Quick(this, (string)
                       RED "Unable to complete update of " BLUE + fname +
                       (string)POP "." POP, "Next", "", PICS XICON POP);

        return;

      }

      if (SDL_GetTicks() - epoch > SHOWRATE) {
        epoch = SDL_GetTicks();
        redraw();
      }

    }

    /* XXX only say this if there were stray files */
    if (up->commit()) {
      say(GREEN "Success. Stray files moved to " BLUE "attic" POP"." POP);
    } else {
      say(RED "Committing failed. Stray files not deleted." POP);
    }

    /* succeed */
    return;

  } else {
    say(RED "Unable to fetch collection index!" POP);
    return;
  }

}

/* very similar to upgrade... maybe abstract it? */
UpdateResult Updater_::update(string &msg) {
  /* always cancel the hint */
  HandHold::did_update();

  std::unique_ptr<HTTP> hh{Client::connect(plr, tx.get(), this)};

  if (hh.get() == nullptr) {
    msg = YELLOW "Couldn't connect." POP;
    return UD_FAIL;
  }

  stringlist *fnames = nullptr;
  stringlist *shownames = nullptr;

  switch (checkcolls(hh.get(), fnames, shownames)) {
  case CCResult::OK:
    say("Got collections list.");
    break;
  default:
  case CCResult::FAIL:
    /* parse error? */
    Message::Quick(this, "Failed to get collections list.",
                   "Cancel", "", PICS XICON POP);
    stringlist::diminish(fnames);
    stringlist::diminish(shownames);
    return UD_FAIL;
  }

  if (fnames == 0) {
    Message::Quick(this,
                   "This version cannot accept any collections.\n"
                   "   You should try upgrading, though a new version\n"
                   "   might not be available yet for your platform.",
                   "Cancel", "", PICS XICON POP);
    /* fnames, shownames already empty */
    return UD_FAIL;
  }

  stringlist *subss = nullptr, *subsf = nullptr;

  SelResult sr = selectcolls(fnames, shownames, subsf, subss);
  stringlist::diminish(fnames);
  stringlist::diminish(shownames);

  if (sr != SelResult::OK) return UD_FAIL;

  say(GREEN "Selected subscriptions.");

  /* now, for each subscribed collection, update it... */

  /* invt: subss and subsf are the same length */
  while (subss && subsf) {
    string ff = stringpop(subsf);
    string ss = stringpop(subss);

    updatecoll(hh.get(), ff, ss);
  }

  /* XXX might want to give a fail message if any failed. */
  msg = GREEN "Level update complete.";
  say(msg);
  Message::Quick(this, "Level update complete.", "OK", "",
                 PICS THUMBICON POP);

  return UD_SUCCESS;
}

void Updater_::screenresize() {
  /* XXX resize tx */
}

void Updater_::draw() {
  sdlutil::clearsurface(screen, BGCOLOR);
  tx->draw();
}

}  // namespace

Updater::~Updater() {}

Updater *Updater::Create(Player *p) {
  return Updater_::Create(p);
}
