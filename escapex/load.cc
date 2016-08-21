
#include "escapex.h"
#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "load.h"
#include "../cc-lib/md5.h"

#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <memory>

#include "directories.h"

#include "extent.h"
#include "dircache.h"
#include "util.h"
#include "chars.h"

#include "message.h"
#include "upload.h"
#include "prompt.h"

#include "commenting.h"

#include "dirindex.h"
#include "optimize.h"
#include "smanage.h"
#include "client.h"

#include "menu.h"

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
struct llentry {
  string fname;
  string name;
  string md5;
  int isdir;
  
  string author;
  int sizex;
  int sizey;

  int solved;
  int total;

  bool corrupted;

  /* always owned by player; don't free */
  rating *myrating;
  RateStatus votes;
  int date;
  int speedrecord;
  bool owned;

  /* true if this is a 'managed' file 
     (part of a web collection) */
  bool managed;

  Level *lev;

  static int height() { return fon->height; }
  string display(bool selected);
  void draw(int x, int y, bool sel);
  string convert() { return fname; }
  bool matches(char k);

  ~llentry() { if (lev) lev->destroy(); }
  llentry() { lev = 0; }
  
  static void swap(llentry *l, llentry *r) {
#   define SWAP(f) {const auto f ## _tmp = l->f; l->f = r->f; r->f = f ## _tmp; }
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
			  const llentry &l,
			  const llentry &r) {

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
  static int cmp_bydate(const llentry &l,
			const llentry &r) {

    int order;
    if (default_dirs(order, l, r)) return order;
    
    if (l.date > r.date) return -1;
    else if (l.date < r.date) return 1;
    /* or by solved? */
    else return cmp_byname(l, r);
  }

  static int cmp_bywebsolved(const llentry &l,
			     const llentry &r) {
    
    int order;
    if (default_dirs(order, l, r)) return order;

    if (l.votes.solved < r.votes.solved) return -1;
    else if (l.votes.solved == r.votes.solved) return cmp_byname(l, r);
    else return 1;
  }

  static int cmp_bysolved(const llentry &l,
			  const llentry &r) {
    int order;
    if (default_dirs(order, l, r)) return order;

    if (l.solved < r.solved) return -1;
    else if (l.solved == r.solved) return cmp_byname(l, r);
    else return 1;

  }

  /* descending sort by personal rating field */
# define MINE(letter, field) \
  static int cmp_bymy ## letter(const llentry &l, \
		                const llentry &r) { \
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
  static int cmp_byglobal ## letter(const llentry &l, \
			            const llentry &r) { \
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

  static int cmp_byauthor(const llentry &l,
			  const llentry &r) {
    
    int order;
    if (default_dirs(order, l, r)) return order;

    int c = util::natural_compare(l.author, r.author);

    if (!c) return cmp_byname(l, r);
    else return c;
  }

  static int cmp_byname(const llentry &l,
			const llentry &r) {

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

  static int cmp_none(const llentry &l,
		      const llentry &r) {
    return 0;
  }

  static string none() { return ""; }
};

typedef Selector<llentry, string> selor;

struct LoadLevel_ : public LoadLevel {
  void draw() override;
  void screenresize() override {}

  ~LoadLevel_() override {
    if (showlev) showlev->destroy();
    sel->destroy();
    while (path) stringpop(path);
  }

  bool first_unsolved(string &file, string &title) override;

  string selectlevel() override;
  
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
  static int (*getsort(sortstyle s)) (const llentry &l,
				      const llentry &r) {
    switch (s) {
    default:
    case SORT_DATE: return llentry::cmp_bydate;
    case SORT_ALPHA: return llentry::cmp_byname;
    case SORT_SOLVED: return llentry::cmp_bysolved;
    case SORT_WEBSOLVED: return llentry::cmp_bywebsolved;
    case SORT_PD: return llentry::cmp_bymyd;
    case SORT_PS: return llentry::cmp_bymys;
    case SORT_PR: return llentry::cmp_bymyr;
    case SORT_GD: return llentry::cmp_byglobald;
    case SORT_GS: return llentry::cmp_byglobals;
    case SORT_GR: return llentry::cmp_byglobalr;
    case SORT_AUTHOR: return llentry::cmp_byauthor;
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
  int changedir(string, bool remember = true);

  /* this for solution preview */
  Uint32 showstart;
  Level *showlev;
  int solstep;
  Solution *showsol;
  /* if this isn't the same as sel->selected,
     then we are out of sync. */
  int showidx;

  selor *sel;
  string loop();

  /* if possible, select the last file seen here */
  void select_lastfile();
  
  /* called a few times a second, advancing through
     a solution if one exists. */
  void step();
  void fix_show(bool force = false);
  void drawsmall();

};

LoadLevel_::sortstyle LoadLevel_::sortby = LoadLevel_::SORT_DATE;
string LoadLevel_::lastdir;
string LoadLevel_::lastfile;

void LoadLevel_::select_lastfile() {
  for (int i = 0; i < sel->number; i++) {
    if (sel->items[i].fname == lastfile) sel->selected = i;
  }
}

void LoadLevel_::fix_show(bool force) {
  /* if we notice that we are out of sync with the selected
     level, switch to it. */
  
  /*
  printf("fix_show. sel->selected %d, showidx %d, showlev %p, sol %p\n",
	 sel->selected, showidx, showlev, showsol);
  */

  if (force || (sel->selected != showidx)) {

    showidx = sel->selected;
    if (showlev) showlev->destroy();
    showlev = 0;
    showsol = 0;

    /* directory might be totally empty?? */
    if (sel->number) {

      /*
      printf("about to read from showidx %d. there are %d", showidx,
	     sel->number);
      */
      if (!sel->items[showidx].isdir) {
	// printf("well the level is %p\n", sel->items[showidx].lev);
	showlev = sel->items[showidx].lev->clone();
	if ( (showsol = plr->getsol(sel->items[showidx].md5)) ) {
	  solstep = 0;
	  showstart = STEPS_BEFORE_SOL;
	}
      }
    } else {
      // printf("empty dir!\n");
    }
    
  }

  // printf("exit fix_show\n");

}

void LoadLevel_::step() {

  fix_show();

  /* now, if we have a lev, maybe make a move */
  if (showlev && showsol) {
    if (!showstart) {
      /* step */
      if (solstep < showsol->length) {
	dir d = showsol->dirs[solstep];
	showlev->move(d);
	solstep++;
      }

    } else showstart--;
  }
}


/* PERF could precompute most of this */
void llentry::draw(int x, int y, bool selected) {
  fon->draw(x, y, display(selected));
}

string llentry::display(bool selected) {
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
      pre + color + font::pad(name, ns) + (string)" " POP + 
      (string)(corrupted?RED:GREEN) + 
      font::pad(itos(sizex) + (string)GREY "x" POP +
		itos(sizey), ss) + POP +
      (string)" " BLUE + font::pad(author, as) + POP + myr + 
      (string)" " + ratings;

    return line;
  }
}

bool llentry::matches(char k) {
  if (name.length() > 0) return util::library_matches(k, name);
  else return (fname.length() > 0 && (fname[0] | 32) == k);
}

bool LoadLevel_::first_unsolved(string &file, string &title) {
  /* should use this natural sort, since this is used for the
     tutorials */
  sel->sort(getsort(SORT_ALPHA));

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
  ll->showlev = 0;
  ll->solstep = 0;
  ll->showsol = 0;
  ll->showidx = -1; /* start outside array */

  /* recover 'last dir' */
  string dir = default_dir;

  if (inexact) {
    if (lastdir != "") dir = lastdir;
    
    /* XXX should fall back (to argument?) if this fails;
       maybe the last directory was deleted? */
    do {
      if (ll->changedir(dir)) goto chdir_ok;
      printf("Dir missing? Up: %s\n", dir.c_str());
      dir = util::cdup(dir);
    } while (dir != ".");

    return 0;	

  chdir_ok:
    /* try to find lastfile in this dir, if possible */
    ll->select_lastfile();
  } else {
    if (!ll->changedir(dir, false)) return 0;
  }

  DBTIME("done");

  return ll;
}


string LoadLevel_::selectlevel() {
  return loop();
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

int LoadLevel_::changedir(string what, bool remember) {

  DBTIME_INIT;

  // printf("changedir '%s' with path %p\n", what.c_str(), path);
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

  selor *nsel = selor::create(n);

  nsel->botmargin = drawing::smallheight() + 16 ;

  nsel->below = this;

  nsel->title = helptexts(helppos);


  DBTIME("cd get index");

  /* get (just) the index for this dir, which allows us to
     look up ratings. note that there may be no index. */
  DirIndex *thisindex = 0;
  cache->getidx(where, thisindex);


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
	  DirIndex *iii = 0;

	  int dcp = SDL_GetTicks() + (PROGRESS_TICKS * 2);
	  if (cache->get(ldn, iii, ttt, sss, Progress::drawbar,
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
	      nsel->items[i].name = iii?(iii->title):dn;
	      
	      i++;
	    }
	  }
	}
      }

    } else {

      string contents = util::readfilemagic(ldn, LEVELMAGIC);

      /* try to read it, passing along corruption flag */
      Level *l = Level::fromstring(contents, allow_corrupted);

      if (l) {
	string md5c = MD5::Hash(contents);


	typedef PtrList<NamedSolution> solset;
	
	/* owned by player */
	solset *sols = plr->solutionset(md5c);

	nsel->items[i].solved = 0;

	/* XXX this isn't incorrect, but we should ignore
	   solutions marked as bookmarks for performance
	   and sanity sake */
	if (sols) {
	  if (sols->head->sol->verified ||
	      Level::verify(l, sols->head->sol)) {
	    sols->head->sol->verified = true;
	    nsel->items[i].solved = sols->head->sol->length;
	  } else if (sols->next) { 
	    /* first one didn't verify, but we should reorder
	       solutions so that one does, if possible */

	    solset *no = 0;
	    
	    while (sols) {
	      /* not trying bookmarks */
	      if (!sols->head->bookmark &&
		  Level::verify(l, sols->head->sol)) {
		/* ok! create the new list with this
		   at the head. */

		NamedSolution *ver = sols->head->clone();
		ver->sol->verified = true;

		/* get tail */
		sols = sols->next;

		solset *yes = 0;
		
		/* put the current tail there, cloning */
		while (sols) {
		  solset::push(yes, sols->head->clone());
		  sols = sols->next;
		}

		/* god this is annoying in C++ */
		while (no) {
		  solset::push(yes, no->head->clone());
		  no = no->next;
		}

		solset::push(yes, ver);

		/* now save this reordering and succeed */
		plr->setsolutionset(md5c, yes);
		nsel->items[i].solved = ver->sol->length;

		goto solset_search_done;

	      } else {
		solset::push(no, sols->head);
		sols = sols->next;
	      }
	    }

	    /* didn't find any, so free head;
	       solved stays 0 */
	    solset::diminish(no);

	  solset_search_done:;
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
	nsel->items[i].lev = l;
	nsel->items[i].myrating = plr->getrating(md5c);
	nsel->items[i].speedrecord = 0;
	nsel->items[i].date = 0;
	nsel->items[i].owned = false;
	nsel->items[i].managed = thisindex && thisindex->webcollection();

	/* failure result is ignored, because the
	   votes are initialized to 0 anyway */
	if (thisindex) {
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
  
  nsel->sort(getsort(sortby));

  DBTIME("cd sorted");

#if 0
  if (!nsel->number) {
    Message::no(0, "There are no levels at all!!");
    return 0;
  }
#endif

  if (sel) sel->destroy();
  sel = nsel;

  if (!sel->number) {
    Message::no(0, "There are no levels at all!!");

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
    if (!showlev) fix_show(true);
    drawing::drawsmall(y,
		       sel->botmargin,
		       color,
		       showlev,
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
    Message::quick(this, 
		   "Couldn't open/understand that player file.",
		   "OK", "", PICS XICON POP);
    sel->redraw();
    return;
  }

  int nsolved = 0;

  /* progress meter.. */
  int pe = 0; // SDL_GetTicks() + (PROGRESS_TICKS * 2);

  int total = 0;
  {
    for (int i = 0; i < sel->number; i++) {
      if ((!sel->items[i].isdir) &&
	  (!sel->items[i].solved)) {
	total++;
      }
    }
  }

  /* for each unsolved level, try to recover solution */
  int done = 0;
  string solveds;
  // XXX honor wholedir
  for (int i = 0; i < sel->number; i++) {
    if ((wholedir || i == sel->selected) &&
	!sel->items[i].isdir &&
	!sel->items[i].solved) {

      done++;

      /* check every solution in rp. */
      PtrList<Solution> *all = rp->all_solutions();

      /* we don't need to delete these solutions */
      int snn = 0;

      // XXX check for esc keypress
      while (all) {
	Solution *s = PtrList<Solution>::pop(all);

	/* PERF verify does its own cloning */
	Level *l = sel->items[i].lev->clone();

	Solution *out;

	Progress::drawbar((void*)&pe,
			  done, total, 
			  GREY "(" + itos(nsolved) + ") " POP
			  + sel->items[i].name + " " + 
			  GREY + "(sol #" + itos(snn) + ")"
			  "\n" 
			  ALPHA100 GREEN +
			  solveds);

	snn++;

	/* printf("try %p on %p\n", s, l); */
	if (Level::verify_prefix(l, s, out)) {

	  /* XX should be length of prefix that solves */
	  sel->items[i].solved = 1;
	  string af = sel->items[i].actualfile(path);

	  /* XXX PERF md5s are stored */
	  FILE *f = fopen(af.c_str(), "rb");
	  if (!f) {
	    Message::bug(this, "couldn't open in recovery");
	    sel->redraw();
	    continue;
	  }
	  string md5 = MD5::Hashf(f);

	  fclose(f);

	  /* extend progress msg */
	  {
	    if (solveds != "") solveds += GREY ", ";
	    solveds += GREEN ALPHA100 + l->title;
	    solveds = font::truncate(solveds, 
				     /* keep right side, not left */
				     -(
				       -1 + 
				       (screen->w /
					(fonsmall->width - fonsmall->overlap))));
	    /* force draw */
	    pe = 0;
	  }

	  /* save solution now */
	  {
	    NamedSolution ns(out, "Recovered", plr->name, 
			     time(0), false);
	    plr->addsolution(md5, &ns, false);
	  }
	  nsolved++;
	  out->destroy();

	  /* then don't bother looking at the tail */
	  PtrList<Solution>::diminish(all);
	}

	l->destroy();
      }
    }
  }

  if (nsolved > 0) {
    plr->writefile();
  } else {
    Message::quick(this,
		   "Couldn't recover any new solutions.",
		   "OK", "", PICS EXCICON POP);
  }
}

string LoadLevel_::loop() {

  sel->redraw();

  SDL_Event event;

  Uint32 nextframe = SDL_GetTicks() + LOADFRAME_TICKS;

  /* last recovery file */
  string lastrecover;

  for (;;) {
    SDL_Delay(1);

    Uint32 now = SDL_GetTicks();
  
    if (now > nextframe) {
      step(); 
      nextframe = now + LOADFRAME_TICKS;
      /* only draw the part that changed */
      drawsmall();
      SDL_Flip(screen);
    }
  
    while (SDL_PollEvent(&event)) {

      if ( event.type == SDL_KEYDOWN ) {
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
	      
	      label message;
	      message.text = 
		PICS TRASHCAN POP
		" Really delete level from web collection?";
	      label message2;
	      message2.text =
		GREY "    (moves it to the graveyard)";
	      
	      int IND = 2;
	      
	      string password;

	      textpassword pass;
	      pass.question = "Server password:";
	      pass.input = "";
	      pass.explanation =
		"The server password. If you don't know it, too bad!"; 

	      textbox desc(42, 7);
	      desc.indent = IND;
	      desc.question = "Message. " GREY "(recommended)" POP;
	      desc.explanation =
		"You can explain your deletion here.";
	      
	      vspace spacer((int)(fon->height * 1.5f));
	      vspace spacer2((int)(fon->height * 1.5f));
	      
	      okay ok;
	      ok.text = "Delete Level";
	      
	      cancel can;
	      can.text = "Cancel";
	      
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
	      menu *mm = menu::create(0, "Really delete?", l, false);
	      resultkind res = mm->menuize();
	      PtrList<MenuItem>::diminish(l);
	      mm->destroy();
	      
	      if (res == MR_OK) {
		
		/* ask server */
		string res;
		if (Client::quick_rpc(plr, DELETE_RPC,
				      (string)"pass=" +
				      httputil::urlencode(pass.input) +
				      (string)"&id=" +
				      itos(plr->webid) + 
				      (string)"&seql=" +
				      itos(plr->webseql) +
				      (string)"&seqh=" +
				      itos(plr->webseqh) +
				      (string)"&md=" +
				      MD5::Ascii(md5) +
				      (string)"&text=" +
				      httputil::urlencode(desc.get_text()),
				      res)) {
		
		  Message::quick(this, "Success!", "OK", "", PICS THUMBICON POP);

		} else {
		  Message::no(this, "Couldn't delete: " + res);
		  sel->redraw();
		  continue;
		}
	      } else {
		/* menu: cancel */
		sel->redraw();
		continue;
	      }

	    } else {
	      Message::no(this, 
			  "In web collections, you can only delete\n"
			  "   a level that you uploaded. "
			  "(marked " PICS KEYPIC POP ")\n");
	      sel->redraw();
	      continue;
	    }

	  } else {
	    /* just a file on disk */
	    if (!Message::quick(this,
				PICS TRASHCAN POP " Really delete " BLUE +
				file + POP "?",
				"Yes",
				"Cancel", PICS QICON POP)) {
	      sel->redraw();
	      continue;
	    }
	  }

	  if (!util::remove(file)) {
	    Message::no(this, "Error deleting file!");
	    sel->redraw();
	    continue;
	  }

	  /* we've deleted, so we need to reload this dir. save
	     lastfile as the file after or before the one we just
	     deleted */
	  if (sel->selected + 1 < sel->number)
	    lastfile = sel->items[sel->selected + 1].fname;
	  else if (sel->selected - 1 >= 0)
	    lastfile = sel->items[sel->selected - 1].fname;
	  
	  changedir(".");
	  
	  select_lastfile();
	  fix_show(true);
	  sel->redraw();
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

	    sel->sort(getsort(sortby));
	    
	    select_lastfile();
	    sel->redraw();

	    continue;
	  } else break;
	}
	case SDLK_BACKSPACE:
	  /* might fail, but that's okay */
	  changedir("..");
	  sel->redraw();
	  continue;
	case SDLK_KP_PLUS:
	case SDLK_SLASH:
	case SDLK_QUESTION:
	  helppos++;
	  helppos %= numhelp;
	  sel->title = helptexts(helppos);
	  sel->redraw();
	  continue;

	case SDLK_m:
	  if ((event.key.keysym.mod & KMOD_CTRL) &&
	      !sel->items[sel->selected].isdir) {

	    if (sel->items[sel->selected].solved) {
	      /* ctrl-m: manage solutions */
	      
	      smanage::manage(plr, sel->items[sel->selected].md5, 
			      sel->items[sel->selected].lev);

	    } else Message::no(this, "You must solve this level first.");

	    /* we probably messed this up */
	    fix_show(true);
	    sel->redraw();
	    continue;
	  } else break;
	  continue;
	case SDLK_c:
	  if ((event.key.keysym.mod & KMOD_CTRL) &&
	      !sel->items[sel->selected].isdir &&
	      sel->items[sel->selected].lev) {
	    /* ctrl-c: comment on a level */
	  
	    if (plr->webid) {
	      Level *l = sel->items[sel->selected].lev;

	      lastfile = sel->items[sel->selected].fname;

	      string file = 
		sel->items[sel->selected].actualfile(path);
	  
	      FILE *f = fopen(file.c_str(), "rb");
	      if (!f) {
		Message::bug(this, "Couldn't open file to comment on");
	    
	      } else {

		string md = MD5::Hashf(f);
		fclose(f);
	    
		/* This does its own error reporting */
		CommentScreen::comment(plr, l, md);

	      }
	    } else {
	      Message::no(this, "You must register with the server to comment.\n"
			  "   (Press " BLUE "R" POP " on the main menu.)");
	    }
	    sel->redraw();
	  } else break;
	  continue;

	case SDLK_r:
	  if ((event.key.keysym.mod & KMOD_CTRL) &&
	      !sel->items[sel->selected].isdir &&
	      sel->items[sel->selected].lev) {
	    /* ctrl-r: rate a level */

	    if (plr->webid) {
	  
	      Level *l = sel->items[sel->selected].lev;

	      lastfile = sel->items[sel->selected].fname;

	      string file = 
		sel->items[sel->selected].actualfile(path);

	      /* XXX now in llentry, also comment */
	      FILE *f = fopen(file.c_str(), "rb");
	      if (!f) {
		Message::bug(this, "Couldn't open file to rate");
	    
	      } else {

		/* first remove its rating. it will be
		   invalidated */
		sel->items[sel->selected].myrating = 0;
	    
		string md = MD5::Hashf(f);
		fclose(f);
				   
		std::unique_ptr<RateScreen> rs{RateScreen::Create(plr, l, md)};
		if (rs.get() != nullptr) {
		  rs->rate();
		} else {
		  Message::bug(this, "Couldn't create rate object!");
		}

		/* now restore the rating */
		sel->items[sel->selected].myrating = plr->getrating(md);
		/* XX resort? */
	      }

	    } else {
	      Message::no(this, 
			  "You must register with the server to rate levels.\n"
			  "   (Press " BLUE "R" POP " on the main menu.)");

	    }

	    sel->redraw();
	  } else break;
	  continue;
	case SDLK_0:
	  if (event.key.keysym.mod & KMOD_CTRL) {

	    label message1, message2, message3, message4;
	    message1.text = PICS BOOKMARKPIC POP " Solve from bookmarks.";
	    message2.text = "    Note: This will often solve loose or short levels";
	    message3.text = "    that you've never seen before, which is a little";
	    message4.text = "    bit like cheating, right?";

	    vspace spacer(fon->height);

	    textinput filename;
	    filename.question = "Player file:";
	    filename.explanation = 
	      "Name of the player file (.esp) to read solutions from.";
	    filename.accept_on_enter = false;
	    // XXX TODO: Set current player file as the default, since
	    // that is usually what you want.
	    filename.input = lastrecover;

	    toggle everything;
	    everything.checked = false;
	    everything.question = "Everything in the directory.";
	    everything.explanation = 
	      "If checked, then try solving every level in the directory.\n"
	      "Not recommended unless you are recovering a bookmarks file.";

	    int what;
	    okay solve("Try to solve", &what, 1);
	    solve.explanation = 
	      "Try to solve the level or levels.\n"
	      "Saves solutions if successful!";

	    cancel can;
	    can.text = "Cancel";

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
	    menu *mm = menu::create(this, "Solve from bookmarks?", l, false);
	    resultkind res = mm->menuize();

	    if (res == MR_OK) {
	      solvefrombookmarks(filename.input, everything.checked);
	    }

	    PtrList<MenuItem>::diminish(l);
	    mm->destroy();

	    fix_show();
	    sel->redraw();
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

		label message;
		label message2;
		label message3;
		label message4;
		label message5;
		label message6;
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
	      
		textbox desc(42, 7);
		desc.indent = IND;
		desc.question = "Message. " GREY "(optional)" POP;
		desc.explanation =
		  "You can post a comment about the level you're uploading.";
	      
		vspace spacer((int)(fon->height * 0.8f));
	      
		okay ok;
		ok.text = "Upload Level";
	      
		cancel can;
		can.text = "Cancel";
	      
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
		menu *mm = menu::create(0, "Really upload?", l, false);
		resultkind res = mm->menuize();
		PtrList<MenuItem>::diminish(l);
		mm->destroy();
	      
		if (res != MR_OK) {
		  sel->redraw();
		  continue;
		}

		/* XXX could detect certain kinds of annoying 
		   levels and warn... */

		std::unique_ptr<Upload> uu{Upload::Create()};

		if (!uu.get()) {
		  Message::bug(this,
			       "Can't create upload object!");
		  sel->redraw();
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
		Message::no(this, 
			    "Can't upload dirs or unsolved levels.");
	      }
	    } else {
	      Message::no
		(this, 
		 "You must register with the server to upload levels.\n"
		 "(Press " BLUE "R" POP " on the main menu.)");
	    }
	    sel->redraw();
	    continue;

	  } else break;
	default:
	  break;
	}
      }
    
      selor::peres pr = sel->doevent(event);
      switch (pr.type) {
      case selor::PE_SELECTED:
	if (sel->items[pr.u.i].isdir) {
	  /* XXX test if changedir failed, if so,
	     display message. but why would it
	     fail? */
	  /* printf("chdir '%s'\n", sel->items[pr.u.i].fname.c_str()); */
	  if (!changedir(sel->items[pr.u.i].fname)) {
	    Message::quick(0, "Couldn't change to " BLUE +
			   sel->items[pr.u.i].fname,
			   "Sorry", "", PICS XICON POP);
	  }
	  /* printf("chdir success.\n"); */
	  sel->redraw();
	  break;
	} else {
	  string out = sel->items[pr.u.i].fname;
	  lastfile = out;
	  while (path) {
	    out = stringpop(path) + string(DIRSEP) + out;
	  }
	  return out;
	}
      case selor::PE_EXIT: /* XXX */
      case selor::PE_CANCEL:
	return "";
      default:
      case selor::PE_NONE:
	;
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
