
#include "playerdb.h"
#include "util.h"
#include "prompt.h"
#include "chars.h"
#include "message.h"
#include "prefs.h"
#include "directories.h"

#include "draw.h"

#define PLAYERDB_MAGIC "ESXD"

#define PDBTITLE YELLOW "Welcome to Escape!" POP \
                 " Select a player with the " \
                 BLUE "arrow keys" POP " and " BLUE "enter" POP ".\n" \
                 "To delete a player, press " BLUE "ctrl-d" POP ".\n \n"

#define MENUITEMS 3
enum pdbkind { K_PLAYER, K_NEW, K_IMPORT, K_QUIT, };

namespace {

/* entries in the selector */
struct PDBEntry {
  pdbkind kind;

  string name;
  string fname;

  int solved;

  static int height() { return TILEH + 8; }
  void draw(int x, int y, bool sel);
  string display(bool sel);
  Player *convert();

  bool matches(char k);
  static Player *none() { return 0; }

  static void swap(PDBEntry *l, PDBEntry *r) {
#   define SWAP(f) { const auto f ## _tmp = l->f; l->f = r->f; r->f = f ## _tmp; }
    SWAP(kind);
    SWAP(name);
    SWAP(fname);
    SWAP(solved);
#   undef SWAP
  }

  static int cmp_bysolved(const PDBEntry &l,
                          const PDBEntry &r) {
    if (l.kind < r.kind) return -1;
    if (l.kind > r.kind) return 1;
    if (l.solved > r.solved) return -1;
    else if (l.solved == r.solved) {
      if (l.name > r.name) return -1;
      else if (l.name == r.name) return 0;
      else return 1;
    }
    else return 1;
  }

};

using Selor = Selector<PDBEntry, Player *>;

struct PlayerDB_ : public PlayerDB {
  /* make db with one player, 'Default' */
  static PlayerDB_ *create();

  bool first;
  bool firsttime() override { return first; }

  Player *chooseplayer() override;

 private:
  std::unique_ptr<Selor> sel;
  void AddPlayer(string);
  void DeletePlayer(int);
  void InsertMenu(int);
  void PromptNew();
  void PromptImport();

  static string MakeFilename(string name);
  static string Safeify(string name);
};

/* assumes there's enough room! */
void PlayerDB_::InsertMenu(int start) {
  sel->items[start++].kind = K_QUIT;
  sel->items[start++].kind = K_NEW;
  sel->items[start++].kind = K_IMPORT;
}

void PDBEntry::draw(int x, int y, bool selected) {

  int ix = x + 4;
  int iy = y + 4;

  int tx = x + TILEW + 8;
  int ty = y + ((TILEW + 8) >> 1) - (fon->height >> 1);

  switch (kind) {

  default:
    fon->draw(tx, ty, "???");
    break;

  case K_IMPORT:
    Drawing::drawtileu(ix, iy, TU_I, 0, screen);
    fon->draw(tx, ty, BLUE "Import / Recover Player");
    break;

  case K_NEW:
    Drawing::drawtileu(ix, iy, TU_N, 0, screen);
    fon->draw(tx, ty, BLUE "New Player");
    break;

  case K_QUIT:
    Drawing::drawtileu(ix, iy, TU_X, 0, screen);
    fon->draw(tx, ty, BLUE "Quit");
    break;

  case K_PLAYER:
    /* Draw the player graphic. Should actually animate the
       player that's selected. */
    Drawing::drawguy(DIR_DOWN, ix, iy, 0, screen);

    fon->draw(tx, ty, display(selected));
    break;
  }
}

string PDBEntry::display(bool sel) {
  string color = "";
  if (sel) color = BLUE;
  return color + name + (string)" " POP
         "(" YELLOW + itos(solved) + (string)POP ")";
}

Player *PDBEntry::convert() {
  if (Player *ret = Player::FromFile(fname)) {
    /* ensure that this player (which may be from an older version)
       has at least defaults for any new prefs */
    Prefs::defaults(ret);
    return ret;
  } else {
    Message::No(0, "Couldn't read player " + fname);
    return nullptr;
  }
}

bool PDBEntry::matches(char k) {
  if (kind == K_PLAYER)
    return (name.length() > 0) && ((name[0]|32) == k);
  else if (kind == K_IMPORT) return ((k | 32) == 'i');
  else if (kind == K_NEW) return ((k | 32) == 'n');
  else if (kind == K_QUIT) return ((k | 32) == 'x' ||
                                   (k | 32) == 'q');
  /* ?? */
  else return false;
}

/* Read current directory, inserting any player file that's found.
   If there are none found, then create a "default" player and
   start over. */
PlayerDB_ *PlayerDB_::create() {

  std::unique_ptr<PlayerDB_> pdb{new PlayerDB_{}};

  /* need at least enough for the menuitems, and for the
     default player. */
  pdb->sel = Selector<PDBEntry, Player *>::Create(MENUITEMS);
  if (!pdb->sel) return 0;

  pdb->sel->title = PDBTITLE;


  DIR *d = opendir(".");
  if (!d) return 0;
  dirent *de;

  int n = 0;
  while ( (de = readdir(d)) ) {
    string f = de->d_name;

#   ifdef WIN32
    f = util::lcase(f);
#   endif

    /* only bother with .esp files, particularly
       to avoid importing backups */
    /* printf("%s...\n", f.c_str()); */
    if (util::endswith(f, ".esp")) {
      pdb->sel->Resize(MENUITEMS + n + 1);
      pdb->sel->items[n].kind = K_PLAYER;
      pdb->sel->items[n].fname = f;

      if (Player *p = Player::FromFile(f)) {
        pdb->sel->items[n].name = p->name;
        pdb->sel->items[n].solved = p->num_solutions();

        delete p;
      } else {
        pdb->sel->items[n].name = f + "  **" RED "ERROR" POP "**";
        pdb->sel->items[n].solved = -1;
      }

      n++;
    }
  }

  /* no players found? */
  if (n == 0) {
    pdb->sel->Resize(MENUITEMS);
    pdb->InsertMenu(0);
    pdb->AddPlayer("Default");
    pdb->first = true;
  } else {
    pdb->InsertMenu(n);
    pdb->first = false;
  }

  return pdb.release();
}

string PlayerDB_::Safeify(string name) {
  /* names can only contain certain characters. */

  string ou;

  for (unsigned int i = 0;
       i < name.length() && ou.length() < 32;
       i++) {

    if ((name[i] >= 'A' &&
         name[i] <= 'Z') ||
        (name[i] >= 'a' &&
         name[i] <= 'z') ||
        (name[i] >= '0' &&
         name[i] <= '9') ||
        name[i] == '_' ||
        name[i] == '-' ||
        name[i] == ' ' ||
        name[i] == '.' ||
        name[i] == '(' ||
        name[i] == ')' ||
        name[i] == '!' ||
        name[i] == '@') ou += (char)name[i];
  }

  return ou;
}

string PlayerDB_::MakeFilename(string name) {
  /* shorten to 8 chars, strip special characters,
     add .esp */

  string ou;

  for (unsigned int i = 0;
      i < name.length() && ou.length() <= 8;
      i++) {

    if ((name[i] >= 'A' &&
         name[i] <= 'Z') ||
        (name[i] >= 'a' &&
         name[i] <= 'z') ||
        (name[i] >= '0' &&
         name[i] <= '9') ||
        name[i] == '_') ou += (char)name[i];
    else if (name[i] == ' ') ou += '_';
  }

  if (ou == "") ou = "player";

  ou += ".esp";

  /* XXX test if the file exists,
     and if it does, change the
     player filename. */

  if (util::existsfile(ou)) return "";

  return ou;
}

void PlayerDB_::AddPlayer(string name) {
  if (Player *plr = Player::Create(name)) {

    /* can fail, for example, if the file exists */
    string fname = MakeFilename(name);
    if (fname != "") {

      sel->Resize(sel->number + 1);

      plr->fname = fname;
      plr->WriteFile();

      /* one slack spot; initialize it */
      sel->items[sel->number - 1].kind = K_PLAYER;
      sel->items[sel->number - 1].solved = 0;
      sel->items[sel->number - 1].name = name;
      sel->items[sel->number - 1].fname = fname;

      // XXX this leaks the player? -tom7   20 Aug 2016

      return;
    } else {
      delete plr;
    }
  }

  /* failed if we got here */
  Message::Quick(0, "Couldn't create player. Does file already exist?",
                 "OK", "", PICS XICON POP);
}

void PlayerDB_::DeletePlayer(int i) {

  /* XXX delete player file from disk? */
  if (sel->items[i].kind == K_PLAYER) {

    util::toattic(sel->items[i].fname);

    int n = 0;
    for (int m = 0; m < sel->number; m++) {
      if (m != i) {
        sel->items[n++] = sel->items[m];
      }
    }

    sel->number = n;
    sel->selected = 0;
  }
}

Player *PlayerDB_::chooseplayer() {
  sel->Sort(PDBEntry::cmp_bysolved);

  sel->Redraw();

  SDL_Event event;

  while (SDL_WaitEvent(&event) >= 0) {

    /* control-something is handled
       separately. */

    if (event.type == SDL_KEYDOWN &&
        (event.key.keysym.mod & KMOD_CTRL))
      switch (event.key.keysym.sym) {

      default:
        break;

        /* delete selected */
      case SDLK_d: {
        /* XXX use a more robust method here to detect the default player. */
        if (sel->items[sel->selected].name != "Default"
            && sel->items[sel->selected].kind == K_PLAYER) {

          string answer =
            Prompt::ask(0,
                        ((string)PICS QICON POP " Really delete " BLUE +
                         sel->items[sel->selected].name +
                         (string)" " POP "(" YELLOW +
                         itos(sel->items[sel->selected].solved) +
                         (string)" " POP "solved)? (y/N) "));

          if (answer.length() > 0 && (answer[0]|32) == 'y') {
            DeletePlayer(sel->selected);
          }

          sel->Sort(PDBEntry::cmp_bysolved);
          sel->Redraw();
        } else {

          /* XXX should we even do anything here? */
          Message::No(0, "Can't delete default player or menu items!");
          sel->Redraw();
        }
        continue;
      }
        /* create new */
      case SDLK_n: {
        PromptNew();
        continue;
      }
      }


    /* otherwise, handle via default selector */
    Selor::PERes pr = sel->DoEvent(event);
    switch (pr.type) {
    case Selor::PEType::SELECTED:
      switch (sel->items[sel->selected].kind) {
      case K_PLAYER:
        return sel->items[sel->selected].convert();
      case K_QUIT:
        return 0; /* XXX? */
      case K_IMPORT:
        PromptImport();
        continue;

      case K_NEW:
        PromptNew();
        continue;
      }
      /* ??? */
      break;
      /* FALLTHROUGH */
    case Selor::PEType::EXIT: /* XXX */
    case Selor::PEType::CANCEL:
      return 0;
    default:
    case Selor::PEType::NONE:
      break;
    }
  }

  return 0;
}

void PlayerDB_::PromptNew() {
  /* XXX could default to getenv(LOGNAME) on linux */
  string ssss = Safeify(
      Prompt::ask(0, "Enter name for new player: "));

  if (ssss != "") {
    AddPlayer(ssss);
  }

  sel->Sort(PDBEntry::cmp_bysolved);
  sel->Redraw();
}

/* FIXME there should be no need for this any more? */
/* maybe it could re-scan files that aren't .esp, to show
   backups? */
/* XXX this would be much nicer if it actually let
   you browse the directory for a player file */
void PlayerDB_::PromptImport() {
  Message::No(0,
              "To import a player, just " BLUE "copy the .esp file" POP "\n"
              "   into the escape directory and restart the game.");

  sel->Redraw();
}

}  // namespace

PlayerDB *PlayerDB::create() {
  return PlayerDB_::create();
}
