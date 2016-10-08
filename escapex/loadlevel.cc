
#include "escapex.h"
#include "loadlevel.h"
#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/md5.h"

#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <memory>

#include "directories.h"

#include "dircache.h"
#include "util.h"
#include "chars.h"

#include "message.h"
#include "upload.h"
#include "prompt.h"

#include "commenting.h"

#include "dirindex.h"
#include "optimize.h"
#include "client.h"

#include "menu.h"
#include "textbox.h"

#include "progress.h"

#ifdef LINUX
/* just used for debugging */
#  include <sys/time.h>
#endif

/* how frequently to make a move in solution playback */
#define LOADFRAME_TICKS 100
#define STEPS_BEFORE_SOL 5

namespace {

/* XXX rationalize ".." stuff.
   Currently we disallow it completely, which
   seems like a good solution.

   But there is still something strange about entering
   .. from the "root" directory (see TODO)
*/

/* XXX this has gotten pretty big.
   move it to another file */
/* XXX normalize these by pulling
   sizex, sizey, author (at least)
   out of lev */
struct LLEntry {
  string fname;
  string name;
  string md5;
  int isdir;

  string author;
  int sizex;
  int sizey;

  // If >0, length of player's solution.
  int solved;
  int total;

  bool corrupted;

  /* always owned by player; don't free */
  Rating *myrating;
  RateStatus votes;
  int date;
  int speedrecord;
  bool owned;

  /* true if this is a 'managed' file
     (part of a web collection) */
  bool managed;

  std::unique_ptr<Level> lev;

  static int height() { return fon->height; }
  string display(bool selected);
  void draw(int x, int y, bool sel);
  string convert() { return fname; }
  bool matches(char k);

  LLEntry() {}

  static void swap(LLEntry *l, LLEntry *r) {
#   define SWAP(f) {auto f ## _tmp = std::move(l->f); l->f = std::move(r->f); r->f = std::move(f ## _tmp); }
    SWAP(md5);
    SWAP(fname);
    SWAP(name);
    SWAP(date);
    SWAP(speedrecord);
    SWAP(owned);
    SWAP(managed);
    SWAP(isdir);
    SWAP(author);
    SWAP(corrupted);
    SWAP(sizex);
    SWAP(sizey);
    SWAP(solved);
    SWAP(total);
    SWAP(lev);
    SWAP(myrating);
    SWAP(votes);
#   undef SWAP
    
  }

  string actualfile(stringlist *p) {
    string file = fname;
    stringlist *pathtmp = stringlist::copy(p);
    while (pathtmp) {
      file = stringpop(pathtmp) +
             string(DIRSEP) + file;
    }
    return file;
  }

  /* default: directories are first */
  static bool default_dirs(int &ret,
			   const LLEntry &l,
			   const LLEntry &r) {

    /* make this appear first */
    if (l.fname == ".." && r.fname != "..") {
      ret = -1; return true;
    }
    if (l.fname != ".." && r.fname == "..") {
      ret = 1; return true;
    }

    /* then directories */
    if (l.isdir && !r.isdir) { ret = -1; return true; }
    if (!l.isdir && r.isdir) { ret = 1; return true; }

    /* if one is a dir, both are. sort by
       number of levels first. */
    if (l.isdir) {
      ret = 1;
      if (l.total > r.total) ret = -1;
      if (l.total == r.total) ret = 0;
      return true;
    }

    return false;

  }

  /* newest first -- only if webindex */
  static int cmp_bydate(const LLEntry &l,
                        const LLEntry &r) {

    int order;
    if (default_dirs(order, l, r)) return order;

    if (l.date > r.date) return -1;
    else if (l.date < r.date) return 1;
    /* or by solved? */
    else return cmp_byname(l, r);
  }

  static int cmp_bywebsolved(const LLEntry &l,
                             const LLEntry &r) {

    int order;
    if (default_dirs(order, l, r)) return order;

    if (l.votes.solved < r.votes.solved) return -1;
    else if (l.votes.solved == r.votes.solved) return cmp_byname(l, r);
    else return 1;
  }

  static int cmp_bysolved(const LLEntry &l,
                          const LLEntry &r) {
    int order;
    if (default_dirs(order, l, r)) return order;

    if (l.solved < r.solved) return -1;
    else if (l.solved == r.solved) return cmp_byname(l, r);
    else return 1;
  }

  /* descending sort by personal rating field */
# define MINE(letter, field) \
  static int cmp_bymy ## letter(const LLEntry &l, \
                                const LLEntry &r) { \
    int order; \
    if (default_dirs(order, l, r)) return order; \
                                                  \
    int nl = (l.myrating)?(l.myrating-> field):-1; \
    int nr = (r.myrating)?(r.myrating-> field):-1; \
                                                   \
    if (nl < nr) return 1;                         \
    else if (nl == nr) return cmp_byname(l, r);    \
    else return -1;                                \
  }

  MINE(d, difficulty)
  MINE(s, style)
  MINE(r, rigidity)
# undef MINE

  /* descending sort by global rating field */
# define GLOB(letter, field) \
  static int cmp_byglobal ## letter(const LLEntry &l, \
                                    const LLEntry &r) { \
    int order; \
    if (default_dirs(order, l, r)) return order; \
                                                 \
    float nl = l.votes.nvotes?(l.votes. field / \
                               (float)l.votes.nvotes):-1.0f; \
    float nr = r.votes.nvotes?(r.votes. field / \
                               (float)r.votes.nvotes):-1.0f; \
    if (nl < nr) return 1; \
    else if (nl > nr) return -1; \
    else return cmp_byname(l, r); \
  }

  GLOB(d, difficulty)
  GLOB(s, style)
  GLOB(r, rigidity)
# undef GLOB

  static int cmp_byauthor(const LLEntry &l,
                          const LLEntry &r) {

    int order;
    if (default_dirs(order, l, r)) return order;

    int c = util::natural_compare(l.author, r.author);

    if (!c) return cmp_byname(l, r);
    else return c;
  }

  static int cmp_byname(const LLEntry &l,
                        const LLEntry &r) {

    int order;
    if (default_dirs(order, l, r)) return order;

    /* XXX filter color codes here */
    /* XXX ignore case -- strcasecmp is not available on win32? */

    /* then, sort by names. */
    if (l.name == r.name) {
      /* they might both be empty -- then
         use filenames. */
      if (l.name == "") {
        if (l.fname == r.fname) return 0;

        /* we assume filenames are unique */
        return util::natural_compare(l.fname, r.fname);
      } else return 0;
    }

    /* this also includes the case where one has a
       name and the other doesn't */
    return util::library_compare(l.name, r.name);
  }

  static int cmp_none(const LLEntry &l,
                      const LLEntry &r) {
    return 0;
  }

  static string none() { return ""; }
};

using Selor = Selector<LLEntry, string>;

struct LoadLevel_ : public LoadLevel {
  void draw() override;
  void screenresize() override {}

  ~LoadLevel_() override {
    showlev.reset();
    while (path) stringpop(path);
  }

  bool FirstUnsolved(string &file, string &title) override;

  string SelectLevel() override;

  LoadLevel_() {}

  static LoadLevel_ *Create(Player *p, string dir,
                            bool inexact, bool ac);


 private:
  /* rep inv:
     always of the form (../)*(realdir)* */
  /* XXX I think I now require/maintain that
     there be no .. at all in the path */
  stringlist *path;

  int helppos = 0;
  static const int numhelp;
  static string helptexts(int);

  enum sortstyle {
    SORT_DATE,
    SORT_ALPHA, SORT_SOLVED,
    SORT_PD, SORT_PS, SORT_PR,
    SORT_GD, SORT_GS, SORT_GR,
    SORT_AUTHOR, SORT_WEBSOLVED,
  };

  /* getsort returns a comparison function. C++
     syntax for this is ridiculous */
  static int (*getsort(sortstyle s)) (const LLEntry &l,
                                      const LLEntry &r) {
    switch (s) {
    default:
    case SORT_DATE: return LLEntry::cmp_bydate;
    case SORT_ALPHA: return LLEntry::cmp_byname;
    case SORT_SOLVED: return LLEntry::cmp_bysolved;
    case SORT_WEBSOLVED: return LLEntry::cmp_bywebsolved;
    case SORT_PD: return LLEntry::cmp_bymyd;
    case SORT_PS: return LLEntry::cmp_bymys;
    case SORT_PR: return LLEntry::cmp_bymyr;
    case SORT_GD: return LLEntry::cmp_byglobald;
    case SORT_GS: return LLEntry::cmp_byglobals;
    case SORT_GR: return LLEntry::cmp_byglobalr;
    case SORT_AUTHOR: return LLEntry::cmp_byauthor;
    }
  }

  // aka "ctrl-0"
  void solvefrombookmarks(const string &filename, bool wholedir);


  static sortstyle sortby;

  std::unique_ptr<DirCache> cache;

  Player *plr;
  bool allow_corrupted;

  /* save last dir we entered */
  static string lastdir;
  /* and last filename we selected */
  static string lastfile;
  /* and last sort order */



  string locate(string);
  int ChangeDir(string, bool remember = true);

  /* this for solution preview */
  Uint32 showstart;
  std::unique_ptr<Level> showlev;
  int solstep;
  Solution showsol;
  /* if this isn't the same as sel->selected,
     then we are out of sync. */
  int showidx;

  std::unique_ptr<Selor> sel;
  string Loop();

  /* if possible, select the last file seen here */
  void SelectLastFile();

  /* called a few times a second, advancing through
     a solution if one exists. */
  void Step();
  void fix_show(bool force = false);
  void drawsmall();

};

LoadLevel_::sortstyle LoadLevel_::sortby = LoadLevel_::SORT_DATE;
string LoadLevel_::lastdir;
string LoadLevel_::lastfile;

void LoadLevel_::SelectLastFile() {
  for (int i = 0; i < sel->number; i++) {
    if (sel->items[i].fname == lastfile) sel->selected = i;
  }
}

void LoadLevel_::fix_show(bool force) {
  /* if we notice that we are out of sync with the selected
     level, switch to it. */

  if (force || (sel->selected != showidx)) {

    showidx = sel->selected;
    showlev.reset();
    showsol.Clear();
    solstep = 0;

    /* directory might be totally empty?? */
    if (sel->number) {
      if (!sel->items[showidx].isdir) {
        // printf("well the level is %p\n", sel->items[showidx].lev);
        showlev = sel->items[showidx].lev->Clone();
        const Solution *lsol = plr->GetSol(sel->items[showidx].md5);
        if (lsol != nullptr) {
          showsol = *lsol;
          solstep = 0;
          showstart = STEPS_BEFORE_SOL;
        }
      }
    } else {
      // printf("empty dir!\n");
    }

  }
}

void LoadLevel_::Step() {
  fix_show();

  /* now, if we have a lev, maybe make a move */
  if (showlev.get() != nullptr && !showsol.IsEmpty()) {
    if (!showstart) {
      /* step */
      if (solstep < showsol.Length()) {
        dir d = showsol.At(solstep);
        showlev->Move(d);
        solstep++;
      }

    } else showstart--;
  }
}


/* PERF could precompute most of this */
void LLEntry::draw(int x, int y, bool selected) {
  fon->draw(x, y, display(selected));
}

string LLEntry::display(bool selected) {
  string color = WHITE;
  if (selected) color = YELLOW;

  unsigned int ns = 32;
  unsigned int ss = 6;
  unsigned int as = 24;

  if (isdir) {

    if (fname == "..") {
      return (string)"   " + color + (string)"[..]" POP;
    } else {
      string pre = "   ";
      if (total > 0 && total == solved) pre = YELLOW LCHECKMARK " " POP;

      string so = "";
      if (total > 0) {
        so = itos(solved) + (string)"/" + itos(total);
      } else {
        if (!selected) color = GREY;
        so = "(no levels)";
      }

      string showname = "";
      if (name != "") showname = name;
      else showname = fname;

      return pre +
        color + (string)"[" + showname + (string)" " + so + "]" POP;
    }
  } else {
    string pre = "   ";
    if (owned) pre = PICS KEYPIC " " POP;
    else if (solved) pre = YELLOW LCHECKMARK " " POP;

    string myr = "  " "  " "  ";

    if (myrating) {
      myr =
        (string)RED   BARSTART + (char)(BAR_0[0] + (int)(myrating->difficulty)) +
        (string)GREEN BARSTART + (char)(BAR_0[0] + (int)(myrating->style)) +
        (string)BLUE  BARSTART + (char)(BAR_0[0] + (int)(myrating->rigidity));
    }

    /* shows alpha-dimmed if there are fewer than 3(?) votes */
    string ratings = "  " "  " "  ";
    if (votes.nvotes > 0) {
      ratings =
        (string)((votes.nvotes <= 2)? ((votes.nvotes <= 1) ? ALPHA25 : ALPHA50) : "") +
        (string)RED   BARSTART + (char)(BAR_0[0] + (int)(votes.difficulty / votes.nvotes)) +
        (string)GREEN BARSTART + (char)(BAR_0[0] + (int)(votes.style / votes.nvotes)) +
        (string)BLUE  BARSTART + (char)(BAR_0[0] + (int)(votes.rigidity / votes.nvotes));    }

    string line =
      pre + color + Font::pad(name, ns) + (string)" " POP +
      (string)(corrupted?RED:GREEN) +
      Font::pad(itos(sizex) + (string)GREY "x" POP +
                itos(sizey), ss) + POP +
      (string)" " BLUE + Font::pad(author, as) + POP + myr +
      (string)" " + ratings;

    return line;
  }
}

bool LLEntry::matches(char k) {
  if (name.length() > 0) return util::library_matches(k, name);
  else return (fname.length() > 0 && (fname[0] | 32) == k);
}

bool LoadLevel_::FirstUnsolved(string &file, string &title) {
  /* should use this natural sort, since this is used for the
     tutorials */
  sel->Sort(getsort(SORT_ALPHA));

  for (int i = 0; i < sel->number; i++) {
    if ((!sel->items[i].isdir) &&
        (!sel->items[i].solved)) {
      file = sel->items[i].actualfile(path);
      title = sel->items[i].name;
      return true;
    }
  }
  /* all solved */
  return false;
}

/* measuring load times -- debugging only. */
#if 0  // ifdef LINUX
#  define DBTIME_INIT long seclast, useclast; { struct timeval tv; gettimeofday(&tv, 0); seclast = tv.tv_sec; useclast = tv.tv_usec; }
#  define DBTIME(s) do { struct timeval tv; gettimeofday(&tv, 0); int nsec = tv.tv_sec - seclast; int usec = nsec * 1000000 + tv.tv_usec - useclast; printf("%d usec " s "\n", usec); useclast = tv.tv_usec; seclast = tv.tv_sec; } while (0)
#else
#  define DBTIME_INIT ;
#  define DBTIME(s) ;
#endif

LoadLevel_ *LoadLevel_::Create(Player *p, string default_dir,
                                     bool inexact,
                                     bool allow_corrupted_) {
  DBTIME_INIT;

  LoadLevel_ *ll = new LoadLevel_();
  ll->cache.reset(DirCache::Create(p));
  if (!ll->cache.get()) return nullptr;

  DBTIME("created dircache");

  /* might want this to persist across loads, who knows... */
  /* would be good, except that the directory structure can
     change because of 'edit' or because of 'update.' (or
     because of another process!) */
  /* also, there are now multiple views because of corrupted flag */
  /* Nonetheless, a global cache with a ::refresh method
     and a key to call refresh would probably be a better
     user experience. */

  ll->allow_corrupted = allow_corrupted_;
  ll->plr = p;
  ll->sel = 0;
  ll->path = 0;

  ll->showstart = 0;
  ll->solstep = 0;
  ll->showsol.Clear();
  ll->showidx = -1; /* start outside array */

  /* recover 'last dir' */
  string dir = default_dir;

  if (inexact) {
    if (lastdir != "") dir = lastdir;

    /* XXX should fall back (to argument?) if this fails;
       maybe the last directory was deleted? */
    do {
      if (ll->ChangeDir(dir)) goto chdir_ok;
      printf("Dir missing? Up: %s\n", dir.c_str());
      dir = util::cdup(dir);
    } while (dir != ".");

    return 0;

  chdir_ok:
    /* try to find lastfile in this dir, if possible */
    ll->SelectLastFile();
  } else {
    if (!ll->ChangeDir(dir, false)) return 0;
  }

  DBTIME("done");

  return ll;
}


string LoadLevel_::SelectLevel() {
  return Loop();
}


/* prepend current path onto filename */
string LoadLevel_::locate(string filename) {

  stringlist *pp = path;

  string out = filename;
  while (pp) {
    if (out == "") out = pp->head;
    else out = pp->head + string(DIRSEP) + out;
    pp = pp->next;
  }

  if (out == "") return ".";
  else return out;
}

int LoadLevel_::ChangeDir(string what, bool remember) {
  DBTIME_INIT;

  // printf("ChangeDir '%s' with path %p\n", what.c_str(), path);
  {
    stringlist *pp = path;
    while (pp) {
      /* printf("  '%s'\n", pp->head.c_str()); */
      pp = pp->next;
    }
  }

  string where;

  /* Special case directory traversals. */
  if (what == "..") {
    if (path) {
      /* does the wrong thing after descending past cwd. */
      stringpop(path);
      where = locate("");
    } else return 0;
  } else if (what == ".") {
    /* no change, but do reload */
    // printf("special .\n");
    where = locate("");
  } else {
    /* n.b.
       if cwd is c:\escapex\goodesc,
       and path is ..\..\
       and we enter escapex,
       we should change to ..\,
       not ..\..\escapex.

       .. since we don't allow the path
       to descend beneath the 'root'
       (which is the cwd of escape), this
       scenario never arises.
    */

    where = locate(what);
    path = new stringlist(what, path);
  }

  DBTIME("cd got where");

  int n = dirsize(where.c_str());
  // printf("AFTER DIRSIZE\n");

  DBTIME("cd counted");

  /* printf("Dir \"%s\" has %d entries\n", where.c_str(), n); */

  if (!n) return 0;

  /* save this dir */
  if (remember) lastdir = where;

  std::unique_ptr<Selor> nsel = Selor::Create(n);

  nsel->botmargin = Drawing::smallheight() + 16 ;

  nsel->below = this;

  nsel->title = helptexts(helppos);


  DBTIME("cd get index");

  /* get (just) the index for this dir, which allows us to
     look up ratings. note that there may be no index. */
  std::unique_ptr<DirIndex> thisindex = cache->GetIdx(where);

  DBTIME("cd got index");

  /* now read all of the files */

  /* init array */
  DIR *d = opendir(where.c_str());
  if (!d) return 0;
  dirent *de;

  int i;
  for (i = 0; i < n;) {
    de = readdir(d);
    if (!de) break;

    string ldn = locate(de->d_name);

    if (util::isdir(ldn)) {

      string dn = de->d_name;

      /* senseless to include current dir,
         CVS and SVN dirs... */
      if (!(dn == "." ||
            dn == "CVS" ||
            dn == ".svn" ||
            /* for now, don't even allow the user
               to go above the home directory,
               since we don't handle that
               properly. */
            (dn == ".." && !path))) {

        if (dn == "..") {
          /* don't report completions for parent */
          nsel->items[i].fname = dn;
          nsel->items[i].isdir = 1;
          nsel->items[i].solved = 0;
          i++;
        } else {
          int ttt, sss;

	  // Note 2016: I may have changed the behavior here
	  // on directories with .escignore.
          int dcp = SDL_GetTicks() + (PROGRESS_TICKS * 2);
          if (DirIndex *iii =
	      cache->Get(ldn, ttt, sss, Progress::drawbar,
                         (void *)&dcp)) {

            /* only show if it has levels,
               or at least has an index
               (in corrupted mode we show everything) */
            if (iii || ttt || allow_corrupted) {

              nsel->items[i].fname = dn;
              nsel->items[i].isdir = 1;
              nsel->items[i].solved = sss;
              nsel->items[i].total = ttt;

              /* no need to save the index, just the title */
              nsel->items[i].name = iii ? iii->title : dn;

              i++;
            }
          }
        }
      }

    } else {

      string contents = util::readfilemagic(ldn, LEVELMAGIC);

      /* try to read it, passing along corruption flag */
      std::unique_ptr<Level> l = Level::FromString(contents, allow_corrupted);

      if (l) {
        string md5c = MD5::Hash(contents);

        nsel->items[i].solved = 0;

        {
          const vector<NamedSolution> &sols = plr->SolutionSet(md5c);
          for (int sidx = 0; sidx < sols.size(); sidx++) {
            const NamedSolution &ns = sols[sidx];
            if (!ns.bookmark &&
                (ns.sol.verified ||
                 Level::Verify(l.get(), ns.sol))) {
              plr->SetVerified(md5c, sidx);
              nsel->items[i].solved = ns.sol.Length();

              if (sidx != 0) {
                // Since we found a verfying solution that's not the
                // default, move it first.
                vector<NamedSolution> newsols = {ns};
                newsols.reserve(sols.size());
                for (int j = 0; j < sols.size(); j++) {
                  if (sidx != j) newsols.push_back(sols[j]);
                }
                plr->SetSolutionSet(md5c, std::move(newsols));
                // 'sols' is invalid now.
                break;
              }
              break;
            }
          }
        }

        nsel->items[i].isdir = 0;
        nsel->items[i].md5 = md5c;
        nsel->items[i].fname = de->d_name;
        nsel->items[i].name = l->title;
        nsel->items[i].author = l->author;
        nsel->items[i].corrupted = l->iscorrupted();
        nsel->items[i].sizex = l->w;
        nsel->items[i].sizey = l->h;
        nsel->items[i].lev = std::move(l);
        nsel->items[i].myrating = plr->getrating(md5c);
        nsel->items[i].speedrecord = 0;
        nsel->items[i].date = 0;
        nsel->items[i].owned = false;
        nsel->items[i].managed = thisindex.get() != nullptr &&
	  thisindex->webcollection();

        /* failure result is ignored, because the
           votes are initialized to 0 anyway */
        if (thisindex.get() != nullptr) {
          int ow;
          thisindex->getentry(de->d_name,
                              nsel->items[i].votes,
                              nsel->items[i].date,
                              nsel->items[i].speedrecord,
                              ow);
          nsel->items[i].owned = plr->webid == ow;
        }

        i++;
      }
    }
  }

  /* some of the entries we saw may have been
     non-escape files, so now shrink nsel */
  nsel->number = i;

  closedir(d);

  DBTIME("cd got everything");

  nsel->Sort(getsort(sortby));

  DBTIME("cd sorted");

  sel = std::move(nsel);

  if (!sel->number) {
    Message::No(0, "There are no levels at all!!");

    /* FIXME crash if we continue  */
    exit(-1);
    /* could add a dummy entry? */
  }

  /* better not save our old show! */
  fix_show(true);

  return 1;
}

void LoadLevel_::draw() {
  sdlutil::clearsurface(screen, BGCOLOR);
  drawsmall();
}

void LoadLevel_::drawsmall() {
  Uint32 color =
    SDL_MapRGBA(screen->format, 0x22, 0x22, 0x44, 0xFF);

  int y = (screen->h - sel->botmargin) + 4 ;

  SDL_Rect dst;

  /* clear bottom */
  dst.x = 0;
  dst.y = y;
  dst.h = sel->botmargin - 4;
  dst.w = screen->w;
  SDL_FillRect(screen, &dst, BGCOLOR);

  /* now draw separator */
  dst.x = 8;
  dst.y = y;
  dst.h = 2;
  dst.w = screen->w - 16;
  SDL_FillRect(screen, &dst, color);

  if (sel->number == 0) {
    fon->draw(16, y + 8, WHITE "(" RED "No files!" POP ")" POP);
  } else if (sel->items[sel->selected].isdir) {
    fon->draw(16, y + 8, WHITE "(" BLUE "Directory" POP ")" POP);
  } else {
    if (showlev.get() == nullptr) fix_show(true);
    Drawing::drawsmall(y,
                       sel->botmargin,
                       color,
                       showlev.get(),
                       sel->items[sel->selected].solved,
                       sel->items[sel->selected].fname,
                       &sel->items[sel->selected].votes,
                       sel->items[sel->selected].myrating,
                       sel->items[sel->selected].date,
                       sel->items[sel->selected].speedrecord);
  }
}

void LoadLevel_::solvefrombookmarks(const string &filename,
                                    bool wholedir) {
  std::unique_ptr<Player> rp{Player::FromFile(filename)};
  if (!rp.get()) {
    Message::Quick(this,
                   "Couldn't open/understand that player file.",
                   "OK", "", PICS XICON POP);
    sel->Redraw();
    return;
  }

  int nsolved = 0;

  /* progress meter.. */
  int pe = 0; // SDL_GetTicks() + (PROGRESS_TICKS * 2);

  int total = 0;
  for (int i = 0; i < sel->number; i++) {
    if ((!sel->items[i].isdir) &&
        (!sel->items[i].solved)) {
      total++;
    }
  }

  /* check every solution in rp. */
  const vector<Solution> all = rp->AllSolutions();

  /* for each unsolved level, try to recover solution */
  int done = 0;
  string solveds;

  for (int i = 0; i < sel->number; i++) {
    if ((wholedir || i == sel->selected) &&
        !sel->items[i].isdir &&
        !sel->items[i].solved) {

      const Level *solveme = sel->items[i].lev.get();

      done++;

      int snn = 0;

      // XXX check for esc keypress
      for (const Solution &s : all) {
        Progress::drawbar((void*)&pe,
                          done, total,
                          GREY "(" + itos(nsolved) + ") " POP
                          + sel->items[i].name + " " +
                          GREY + "(sol #" + itos(snn) + ")"
                          "\n"
                          ALPHA100 GREEN +
                          solveds);

        snn++;

        /* printf("try %p on %p\n", s, solveme); */
        Solution out;
        if (Level::VerifyPrefix(solveme, s, &out) && out.Length() > 0) {
          sel->items[i].solved = out.Length();
          string af = sel->items[i].actualfile(path);

          /* XXX PERF md5s are stored */
          FILE *f = fopen(af.c_str(), "rb");
          if (!f) {
            Message::Bug(this, "couldn't open in recovery");
            sel->Redraw();
            continue;
          }
          string md5 = MD5::Hashf(f);
          fclose(f);

          /* extend progress msg */
          {
            if (solveds != "") solveds += GREY ", ";
            solveds += GREEN ALPHA100 + solveme->title;
            solveds = Font::truncate(
                solveds,
                /* keep right side, not left */
                -(-1 +
                  (screen->w /
                   (fonsmall->width - fonsmall->overlap))));
            /* force draw */
            pe = 0;
          }

          /* save solution now */
          {
            NamedSolution ns(out, "Recovered", plr->name,
                             time(0), false);
            plr->AddSolution(md5, std::move(ns), false);
          }
          nsolved++;

          /* then don't bother looking at the tail */
          break;
        }
      }
    }
  }

  if (nsolved > 0) {
    plr->writefile();
  } else {
    Message::Quick(this,
                   "Couldn't recover any new solutions.",
                   "OK", "", PICS EXCICON POP);
  }
}

string LoadLevel_::Loop() {
  sel->Redraw();

  SDL_Event event;

  Uint32 nextframe = SDL_GetTicks() + LOADFRAME_TICKS;

  /* last recovery file */
  string lastrecover;

  for (;;) {
    SDL_Delay(1);

    Uint32 now = SDL_GetTicks();

    if (now > nextframe) {
      Step();
      nextframe = now + LOADFRAME_TICKS;
      /* only draw the part that changed */
      drawsmall();
      SDL_Flip(screen);
    }

    while (SDL_PollEvent(&event)) {

      if (event.type == SDL_KEYDOWN) {
        int key = event.key.keysym.sym;
        /* breaking from here will allow the key to be
           treated as a search */

        switch (key) {
        case SDLK_DELETE: {
          string file =
            sel->items[sel->selected].actualfile(path);

          string md5 =
            sel->items[sel->selected].md5;

          if (sel->items[sel->selected].managed) {

            /* inside a web collection; we must own it */
            if (/* this should be a user class, not a specific define.. */
                plr->webid == SUPERUSER ||
                (sel->items[sel->selected].owned &&
                 plr->webid)) {

              Label message;
              message.text =
                PICS TRASHCAN POP
                " Really delete level from web collection?";
              Label message2;
              message2.text =
                GREY "    (moves it to the graveyard)";

              int IND = 2;

              string password;

              TextPassword pass;
              pass.question = "Server password:";
              pass.input = "";
              pass.explanation =
                "The server password. If you don't know it, too bad!";

              TextBox desc(42, 7);
              desc.indent = IND;
              desc.question = "Message. " GREY "(recommended)" POP;
              desc.explanation =
                "You can explain your deletion here.";

              VSpace spacer((int)(fon->height * 1.5f));
              VSpace spacer2((int)(fon->height * 1.5f));

              Okay ok;
              ok.text = "Delete Level";

              Cancel can;

              PtrList<MenuItem> *l = 0;

              PtrList<MenuItem>::push(l, &can);
              PtrList<MenuItem>::push(l, &ok);
              PtrList<MenuItem>::push(l, &spacer);

              if (!sel->items[sel->selected].owned)
                PtrList<MenuItem>::push(l, &pass);

              PtrList<MenuItem>::push(l, &desc);
              PtrList<MenuItem>::push(l, &spacer2);
              PtrList<MenuItem>::push(l, &message2);
              PtrList<MenuItem>::push(l, &message);


              /* display menu */
	      std::unique_ptr<Menu> mm = 
		Menu::Create(0, "Really delete?", l, false);
              InputResultKind res = mm->menuize();
              PtrList<MenuItem>::diminish(l);
	      mm.reset();

              if (res == InputResultKind::OK) {
                /* ask server */
                string res;
                if (Client::quick_rpc(plr, DELETE_RPC,
                                      (string)"pass=" +
                                      HTTPUtil::urlencode(pass.input) +
                                      (string)"&id=" +
                                      itos(plr->webid) +
                                      (string)"&seql=" +
                                      itos(plr->webseql) +
                                      (string)"&seqh=" +
                                      itos(plr->webseqh) +
                                      (string)"&md=" +
                                      MD5::Ascii(md5) +
                                      (string)"&text=" +
                                      HTTPUtil::urlencode(desc.get_text()),
                                      res)) {

                  Message::Quick(this, "Success!", "OK", "", PICS THUMBICON POP);

                } else {
                  Message::No(this, "Couldn't delete: " + res);
                  sel->Redraw();
                  continue;
                }
              } else {
                /* menu: cancel */
                sel->Redraw();
                continue;
              }

            } else {
              Message::No(this,
                          "In web collections, you can only delete\n"
                          "   a level that you uploaded. "
                          "(marked " PICS KEYPIC POP ")\n");
              sel->Redraw();
              continue;
            }

          } else {
            /* just a file on disk */
            if (!Message::Quick(this,
                                PICS TRASHCAN POP " Really delete " BLUE +
                                file + POP "?",
                                "Yes",
                                "Cancel", PICS QICON POP)) {
              sel->Redraw();
              continue;
            }
          }

          if (!util::remove(file)) {
            Message::No(this, "Error deleting file!");
            sel->Redraw();
            continue;
          }

          /* we've deleted, so we need to reload this dir. save
             lastfile as the file after or before the one we just
             deleted */
          if (sel->selected + 1 < sel->number)
            lastfile = sel->items[sel->selected + 1].fname;
          else if (sel->selected - 1 >= 0)
            lastfile = sel->items[sel->selected - 1].fname;

          ChangeDir(".");

          SelectLastFile();
          fix_show(true);
          sel->Redraw();
          break;
        }

          /* sorting options */
        case SDLK_t:
        case SDLK_n:
        case SDLK_s:
        case SDLK_d:
        case SDLK_g:
        case SDLK_a:
        case SDLK_e:
        case SDLK_v: {
          int resort = 0;

          /* XXX allow other sorts */
          if (event.key.keysym.mod & KMOD_CTRL) {
            resort = 1;
            switch (key) {
            default:
            case SDLK_n: sortby = SORT_DATE; break;
            case SDLK_a: sortby = SORT_ALPHA; break;
            case SDLK_v: sortby = SORT_SOLVED; break;
            case SDLK_e: sortby = SORT_WEBSOLVED; break;

            case SDLK_d: sortby = SORT_GD; break;
            case SDLK_s: sortby = SORT_GS; break;
            case SDLK_g: sortby = SORT_GR; break;

            case SDLK_t: sortby = SORT_AUTHOR; break;

            }
          } else if (event.key.keysym.mod & KMOD_ALT) {
            switch (key) {
            default: sortby = SORT_ALPHA; break;
            case SDLK_d: sortby = SORT_PD; break;
            case SDLK_s: sortby = SORT_PS; break;
            case SDLK_g: sortby = SORT_PR; break;
            }
            resort = 1;
          }

          if (resort) {
            /* focus follows currently selected file */
            lastfile = sel->items[sel->selected].fname;

            sel->Sort(getsort(sortby));

            SelectLastFile();
            sel->Redraw();

            continue;
          } else break;
        }
        case SDLK_BACKSPACE:
          /* might fail, but that's okay */
          ChangeDir("..");
          sel->Redraw();
          continue;
        case SDLK_KP_PLUS:
        case SDLK_SLASH:
        case SDLK_QUESTION:
          helppos++;
          helppos %= numhelp;
          sel->title = helptexts(helppos);
          sel->Redraw();
          continue;

        case SDLK_c:
          if ((event.key.keysym.mod & KMOD_CTRL) &&
              !sel->items[sel->selected].isdir &&
              sel->items[sel->selected].lev) {
            /* ctrl-c: comment on a level */

            if (plr->webid) {
              lastfile = sel->items[sel->selected].fname;

              string file =
                sel->items[sel->selected].actualfile(path);

              FILE *f = fopen(file.c_str(), "rb");
              if (!f) {
                Message::Bug(this, "Couldn't open file to comment on");

              } else {

                string md = MD5::Hashf(f);
                fclose(f);

                /* This does its own error reporting */
                CommentScreen::Comment(
		    plr, sel->items[sel->selected].lev.get(), md);

              }
            } else {
              Message::No(this,
                          "You must register with the server to comment.\n"
                          "   (Press " BLUE "R" POP " on the main menu.)");
            }
            sel->Redraw();
          } else break;
          continue;

        case SDLK_r:
          if ((event.key.keysym.mod & KMOD_CTRL) &&
              !sel->items[sel->selected].isdir &&
              sel->items[sel->selected].lev) {
            /* ctrl-r: rate a level */

            if (plr->webid) {
              lastfile = sel->items[sel->selected].fname;

              string file =
                sel->items[sel->selected].actualfile(path);

              /* XXX now in LLEntry, also comment */
              FILE *f = fopen(file.c_str(), "rb");
              if (!f) {
                Message::Bug(this, "Couldn't open file to rate");

              } else {

                /* first remove its rating. it will be
                   invalidated */
                sel->items[sel->selected].myrating = 0;

                string md = MD5::Hashf(f);
                fclose(f);

                std::unique_ptr<RateScreen> rs{
		  RateScreen::Create(plr,
				     sel->items[sel->selected].lev.get(),
				     md)};
                if (rs.get() != nullptr) {
                  rs->Rate();
                } else {
                  Message::Bug(this, "Couldn't create rate object!");
                }

                /* now restore the rating */
                sel->items[sel->selected].myrating = plr->getrating(md);
                /* XX resort? */
              }

            } else {
              Message::No(this,
                          "You must register with the server to rate levels.\n"
                          "   (Press " BLUE "R" POP " on the main menu.)");

            }

            sel->Redraw();
          } else break;
          continue;
        case SDLK_0:
          if (event.key.keysym.mod & KMOD_CTRL) {

            Label message1, message2, message3, message4;
            message1.text = PICS BOOKMARKPIC POP " Solve from bookmarks.";
            message2.text =
              "    Note: This will often solve loose or short levels";
            message3.text =
              "    that you've never seen before, which is a little";
            message4.text =
              "    bit like cheating, right?";

            VSpace spacer(fon->height);

            TextInput filename;
            filename.question = "Player file:";
            filename.explanation =
              "Name of the player file (.esp) to read solutions from.";
            filename.accept_on_enter = false;
            // XXX TODO: Set current player file as the default, since
            // that is usually what you want.
            filename.input = lastrecover;

            Toggle everything;
            everything.checked = false;
            everything.question = "Everything in the directory.";
            everything.explanation =
              "If checked, then try solving every level in the directory.\n"
              "Not recommended unless you are recovering a bookmarks file.";

            int what;
            Okay solve("Try to solve", &what, 1);
            solve.explanation =
              "Try to solve the level or levels.\n"
              "Saves solutions if successful!";

            Cancel can;

            PtrList<MenuItem> *l = 0;

            PtrList<MenuItem>::push(l, &can);
            PtrList<MenuItem>::push(l, &spacer);

            PtrList<MenuItem>::push(l, &solve);
            PtrList<MenuItem>::push(l, &everything);
            PtrList<MenuItem>::push(l, &filename);

            PtrList<MenuItem>::push(l, &spacer);
            PtrList<MenuItem>::push(l, &message4);
            PtrList<MenuItem>::push(l, &message3);
            PtrList<MenuItem>::push(l, &message2);
            PtrList<MenuItem>::push(l, &message1);

            /* display menu */
	    std::unique_ptr<Menu> mm =
	      Menu::Create(this, "Solve from bookmarks?", l, false);
            InputResultKind res = mm->menuize();

            if (res == InputResultKind::OK) {
              solvefrombookmarks(filename.input, everything.checked);
            }

            PtrList<MenuItem>::diminish(l);
	    mm.reset();

            fix_show();
            sel->Redraw();
          } else break;
          continue;
        case SDLK_u:
          /* holding ctrl, has registererd */
          if ((event.key.keysym.mod & KMOD_CTRL)) {
            /* ctrl-u */
            if (plr->webid) {

              /* could show message if not solved */
              if (!sel->items[sel->selected].isdir &&
                  sel->items[sel->selected].solved) {

                Label message;
                Label message2;
                Label message3;
                Label message4;
                Label message5;
                Label message6;
                message.text =
                  PICS QICON POP
                  " Upload this level to the internet?";
                message2.text =
                  "   " GREY "Only do this when the level's really done." POP;


                message3.text =
                  PICS EXCICON POP " " RED "Important" POP
                  ": Only upload levels that are your own work!" POP;

                message4.text =
                  "   " GREY "(not copied from the web without permission)" POP;

                message5.text =
                  "   By uploading, you agree to let anyone freely distribute";
                message6.text =
                  "   and/or modify your level.";

                int IND = 2;

                TextBox desc(42, 7);
                desc.indent = IND;
                desc.question = "Message. " GREY "(optional)" POP;
                desc.explanation =
                  "You can post a comment about the level you're uploading.";

                VSpace spacer((int)(fon->height * 0.8f));

                Okay ok;
                ok.text = "Upload Level";

                Cancel can;

                PtrList<MenuItem> *l = 0;

                PtrList<MenuItem>::push(l, &can);
                PtrList<MenuItem>::push(l, &ok);
                PtrList<MenuItem>::push(l, &spacer);

                PtrList<MenuItem>::push(l, &desc);
                PtrList<MenuItem>::push(l, &spacer);

                PtrList<MenuItem>::push(l, &message6);
                PtrList<MenuItem>::push(l, &message5);
                PtrList<MenuItem>::push(l, &spacer);

                PtrList<MenuItem>::push(l, &message4);
                PtrList<MenuItem>::push(l, &message3);
                PtrList<MenuItem>::push(l, &spacer);

                PtrList<MenuItem>::push(l, &message2);
                PtrList<MenuItem>::push(l, &message);


                /* display menu */
		std::unique_ptr<Menu> mm =
		  Menu::Create(0, "Really upload?", l, false);
                InputResultKind res = mm->menuize();
                PtrList<MenuItem>::diminish(l);
		mm.reset();

                if (res != InputResultKind::OK) {
                  sel->Redraw();
                  continue;
                }

                /* XXX could detect certain kinds of annoying
                   levels and warn... */

                std::unique_ptr<Upload> uu{Upload::Create()};

                if (!uu.get()) {
                  Message::Bug(this,
                               "Can't create upload object!");
                  sel->Redraw();
                  continue;
                }

                /* save spot */
                lastfile = sel->items[sel->selected].fname;

                string file =
                  sel->items[sel->selected].actualfile(path);

                /* don't bother with message; upload does it */
                switch (uu->Up(plr, file, desc.get_text())) {
                case UploadResult::OK:
                  break;
                default:
                  break;
                }
              } else {
                Message::No(this,
                            "Can't upload dirs or unsolved levels.");
              }
            } else {
              Message::No
                (this,
                 "You must register with the server to upload levels.\n"
                 "(Press " BLUE "R" POP " on the main menu.)");
            }
            sel->Redraw();
            continue;

          } else break;
        default:
          break;
        }
      }

      Selor::PERes pr = sel->DoEvent(event);
      switch (pr.type) {
      case Selor::PEType::SELECTED:
        if (sel->items[pr.u.i].isdir) {
          /* XXX test if ChangeDir failed, if so,
             display message. but why would it
             fail? */
          /* printf("chdir '%s'\n", sel->items[pr.u.i].fname.c_str()); */
          if (!ChangeDir(sel->items[pr.u.i].fname)) {
            Message::Quick(0, "Couldn't change to " BLUE +
                           sel->items[pr.u.i].fname,
                           "Sorry", "", PICS XICON POP);
          }
          /* printf("chdir success.\n"); */
          sel->Redraw();
          break;
        } else {
          string out = sel->items[pr.u.i].fname;
          lastfile = out;
          while (path) {
            out = stringpop(path) + string(DIRSEP) + out;
          }
          return out;
        }
      case Selor::PEType::EXIT: /* XXX */
      case Selor::PEType::CANCEL:
        return "";
      default:
      case Selor::PEType::NONE:
        break;
      }

    }

  } /* unreachable */

}

/* previously we only showed certain options when
   they were available. It might be good to do that
   still. */
const int LoadLevel_::numhelp = 2;
string LoadLevel_::helptexts(int i) {
  switch (i) {
  case 0:
    return
      WHITE
      "Use the " BLUE "arrow keys" POP " to select a level and "
      "press " BLUE "enter" POP " or " BLUE "esc" POP " to cancel."
                     "      " BLUE "?" POP " for more options.\n"
      WHITE
      "Press " BLUE "ctrl-u" POP
      " to upload a level you've created to the internet.\n"

      WHITE
      "Press " BLUE "ctrl-r" POP " to adjust your rating of a level,"
      " or "   BLUE "ctrl-c" POP " to publish a comment.\n";

  case 1:
    return
      WHITE "Sorting options: " BLUE "ctrl-"
            RED "d" POP ","
            GREEN "s" POP ","
            BLUE "g" POP POP
            " sorts by global " RED "difficulty" POP ","
                                GREEN "style" POP ","
                                BLUE "rigidity" POP ".\n"

      WHITE BLUE "alt-" RED "d" POP "," GREEN "s" POP "," BLUE "g" POP POP
      " sorts by personal. "
      // deprecated.
      //      BLUE "ctrl-m" POP " manages solutions (upload/download/view).\n"

      WHITE BLUE "ctrl-a" POP " sorts alphabetically, " BLUE "ctrl-t" POP " by author. "
            BLUE "ctrl-v" POP " shows unsolved levels first.";

  }

  return RED "help index too high";
}

}  // namespace

LoadLevel *LoadLevel::Create(Player *p, string dir, bool td, bool ac) {
  return LoadLevel_::Create(p, dir, td, ac);
}

LoadLevel::~LoadLevel() {}
