
#include "update.h"

#include <memory>
#include <string>

#include "../cc-lib/util.h"
#include "escape-util.h"
#include "textscroll.h"
#include "prompt.h"
#include "message.h"
#include "chars.h"
#include "upper.h"
#include "handhold.h"
#include "menu.h"
#include "ptrlist.h"

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

  UpdateResult Update(string &msg) override;

  void Draw() override {
    sdlutil::clearsurface(screen, BGCOLOR);
    tx->Draw();
  }

  void ScreenResize() override {
    // XXX resize tx
  }

  private:

  void Redraw() {
    Draw();
    SDL_Flip(screen);
  }

  void say(const string &s) {
    if (tx.get() != nullptr) tx->Say(s);
  }

  void sayover(const string &s) {
    if (tx.get() != nullptr) {
      tx->Unsay();
      tx->Say(s);
    }
  }

  Player *plr = nullptr;

  std::unique_ptr<TextScroll> tx;

  CCResult CheckCollections(
      HTTP *hh,
      std::vector<std::pair<string, string>> *collections);

  SelResult SelectCollections(
      const std::vector<std::pair<string, string>> &collections,
      std::vector<std::pair<string, string>> *subs);

  void UpdateCollection(HTTP *hh,
                        const string &fname,
                        const string &showname);
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
      if (EscapeUtil::existsdir(fname)) {
        /* delete unsub file from dir */
        if (EscapeUtil::remove(fname + (string)DIRSEP + UNSUBMARKER)) {

          checked = true;

        } else {

          Message::Quick(0, (string)"Can't subscribe to "
                         BLUE + fname + (string)POP " (remove unsub file)",
                         "Cancel", "", PICS XICON POP);

        }
      } else {
        /* create subscription (directory) */
        if (EscapeUtil::makedir(fname)) {
          checked = true;

        } else {
          Message::Quick(0, (string)"Can't subscribe to "
                         BLUE + fname + (string)POP " (can't make dir!!)",
                         "Cancel", "", PICS XICON POP);
        }
      }
    } else {
      /* try to unsubscribe */

      if (Util::WriteFile(
              fname + (string)DIRSEP + UNSUBMARKER,
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

/* return the available collections.
   (by adding items to fnames, shownames)
*/
CCResult Updater_::CheckCollections(
    HTTP *hh,
    std::vector<std::pair<string, string>> *collections) {
  /* first, grab COLLECTIONS. */

  string s;
  HTTPResult hr = hh->get(COLLECTIONSURL, s);

  if (hr == HTTPResult::OK) {
    /* parse result. see protocol.txt */
    int ncolls = EscapeUtil::stoi(EscapeUtil::getline(s));

    say((string)BLUE + itos(ncolls) + (string)" collection" +
        (string)((ncolls==1)?"":"s") + (string)":" POP);

    /* then, ncolls collections */
    while (ncolls--) {
      string line = EscapeUtil::getline(s);

      string fname = EscapeUtil::chop(line);
      int minv  = EscapeUtil::stoi(EscapeUtil::chop(line));
      string showname = line;

      int usable = EscapeUtil::stoi(VERSION) >= minv;

      if (fname == "") {
        say(RED "  collections file corrupt!!" POP);
        return CCResult::FAIL;
      }

      say((string)"  " YELLOW + fname + POP +
          (string)(usable? " " GREEN : " " RED) +
          itos(minv) + (string)POP " " BLUE + showname + POP);

      if (usable) {
        collections->emplace_back(fname, showname);
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

SelResult Updater_::SelectCollections(
    const std::vector<std::pair<string, string>> &collections,
    std::vector<std::pair<string, string>> *subs) {
  PtrList<MenuItem> *boxes = nullptr;

  Okay ok;
  ok.text = "Update Now";

  Cancel can;

  VSpace v(16);

  const int n_entries = (int)collections.size();

  for (const auto &[filename, showname] : collections) {
    subtoggle *b = new subtoggle();
    b->fname = filename;
    b->question = showname;

    /* check subscribed:
       for collection 'triage' we are
       subscribed if 'triage' subdir exists
       AND 'triage/unsubscribed' does not exist */
    if (EscapeUtil::existsdir(filename)) {
      if (EscapeUtil::existsfile(filename + (string)DIRSEP + UNSUBMARKER)) {
        b->checked = false;
      } else {
        b->checked = true;
      }
    } else {
      b->checked = false;
    }

    PtrList<MenuItem>::push(boxes, b);
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

    /* skip first three, the buttons */
    for (int z = 0; z < 3; z++) {
      PtrList<MenuItem>::pop(boxes);
    }

    for (int i = 0; i < n_entries; i++) {
      subtoggle *st = (subtoggle*)PtrList<MenuItem>::pop(boxes);
      if (st->checked) {
        subs->emplace_back(st->fname, st->question);
      }

      delete st;
    }

    return SelResult::OK;
  } else {
    return SelResult::FAIL;
  }

}

/* update a single collection "fname" using http connection hh. */
void Updater_::UpdateCollection(
    HTTP *hh, const string &fname, const string &showname) {

  say("");
  say((string)"Updating " BLUE + showname + (string)POP " (" YELLOW +
      fname + (string)POP") ...");
  say("");

  string s;
  HTTPResult hr = hh->get((string)"/" + fname + (string) ".txt", s);

  if (hr == HTTPResult::OK) {
    /* parse result. see protocol.txt */

    string showname = EscapeUtil::getline(s);
    int minv = EscapeUtil::stoi(EscapeUtil::getline(s));
    int ndirs = EscapeUtil::stoi(EscapeUtil::getline(s));
    int nfiles = EscapeUtil::stoi(EscapeUtil::getline(s));
    string version = EscapeUtil::getline(s);

    if (EscapeUtil::stoi(VERSION) < minv) {
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
    up->SaveDir("", showname);

    say("ndirs: " + itos(ndirs));

    while (ndirs--) {
      string line = EscapeUtil::getline(s);

      string d = EscapeUtil::chop(line);
      string name = EscapeUtil::losewhitel(line);

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
      EscapeUtil::replace(d, "/", DIRSEP);

      up->SaveDir(d, name);
    }

    say("nfiles: " + itos(nfiles));

    int epoch = SDL_GetTicks();

    /* then, ncolls collections */
    while (nfiles--) {
      string line = EscapeUtil::getline(s);

      /* printf("line: '%s'\n", line.c_str()); */

      string f = EscapeUtil::chop(line);
      string md = EscapeUtil::chop(line);

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

      votes.nvotes = EscapeUtil::stoi(EscapeUtil::chop(line));
      votes.difficulty = EscapeUtil::stoi(EscapeUtil::chop(line));
      votes.style = EscapeUtil::stoi(EscapeUtil::chop(line));
      votes.rigidity = EscapeUtil::stoi(EscapeUtil::chop(line));
      votes.cooked = EscapeUtil::stoi(EscapeUtil::chop(line));
      votes.solved = EscapeUtil::stoi(EscapeUtil::chop(line));

      int date = EscapeUtil::stoi(EscapeUtil::chop(line));
      int speedrecord = EscapeUtil::stoi(EscapeUtil::chop(line));
      int owner = EscapeUtil::stoi(EscapeUtil::chop(line));

      /* require file and md5, but not any of the voting stuff
         (they will default to 0) */
      if (f != "" && md != "" && up->SetFile(f, md, votes,
                                             date, speedrecord, owner)) {
        /* XXX change to sayover? */
        /* say((string)GREEN + f + (string)" " GREY + md + POP POP); */

        // (XXX do something on success... it can look like the game
        // crashed when downloading lots of levels!)

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
        Redraw();
      }

    }

    /* XXX only say this if there were stray files */
    if (up->Commit()) {
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
UpdateResult Updater_::Update(string &msg) {
  /* always cancel the hint */
  HandHold::did_update();

  std::unique_ptr<HTTP> hh{Client::Connect(plr, tx.get(), this)};

  if (hh.get() == nullptr) {
    msg = YELLOW "Couldn't connect." POP;
    return UpdateResult::FAIL;
  }

  // filename, showname
  std::vector<std::pair<string, string>> collections;

  switch (CheckCollections(hh.get(), &collections)) {
  case CCResult::OK:
    say("Got collections list.");
    break;
  default:
  case CCResult::FAIL:
    /* parse error? */
    Message::Quick(this, "Failed to get collections list.",
                   "Cancel", "", PICS XICON POP);
    return UpdateResult::FAIL;
  }

  if (collections.empty()) {
    Message::Quick(this,
                   "This version cannot accept any collections.\n"
                   "   You should try upgrading, though a new version\n"
                   "   might not be available yet for your platform.",
                   "Cancel", "", PICS XICON POP);
    return UpdateResult::FAIL;
  }

  std::vector<std::pair<string, string>> subs;

  SelResult sr = SelectCollections(collections, &subs);

  if (sr != SelResult::OK) return UpdateResult::FAIL;

  say(GREEN "Selected subscriptions.");

  /* now, for each subscribed collection, update it... */

  /* invt: subss and subsf are the same length */
  for (const auto &[ff, ss] : subs) {
    UpdateCollection(hh.get(), ff, ss);
  }

  /* XXX might want to give a fail message if any failed. */
  msg = GREEN "Level update complete.";
  say(msg);
  Message::Quick(this, "Level update complete.", "OK", "",
                 PICS THUMBICON POP);

  return UpdateResult::SUCCESS;
}

}  // namespace

Updater::~Updater() {}

Updater *Updater::Create(Player *p) {
  return Updater_::Create(p);
}
