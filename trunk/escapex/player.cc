
#include "player.h"

#include "extent.h"
#include "chunks.h"
#include "checkfile.h"
#include "prefs.h"
#include "directories.h"

#include "../cc-lib/base64.h"
#include "../cc-lib/md5.h"

#ifdef WIN32
# include <time.h>
#endif

static constexpr int HASHSIZE = 512;

#define PLAYER_MAGIC "ESXP"
#define PLAYERTEXT_MAGIC "ESPt"
#define PLAYER_MAGICS_LENGTH 4
#define SOLMARKER "-- solutions"
#define RATMARKER "-- ratings"
#define PREFMARKER "-- prefs"

/* give some leeway for future expansion */
#define IGNORED_FIELDS 8

// TODO: Maybe namedsolution should be in its own file to simplify
// player interface.
NamedSolution *NamedSolution::clone() {
  return new NamedSolution(sol->clone(), name, author, date, bookmark);
}

NamedSolution::NamedSolution() {
  name = "";
  sol = 0;
  author = "";
  date = 0;
  bookmark = false;
}

NamedSolution::NamedSolution(Solution *s, string na, 
			     string au, int da, bool bm) {
  name = na;
  sol = s;
  author = au;
  date = da;
  bookmark = bm;
}

string NamedSolution::tostring() {
  string solstring = sol->tostring();

  return
    sizes(date) +
    sizes(name.length()) + name +
    sizes(author.length()) + author +
    sizes(solstring.length()) + solstring +
    sizes(bookmark ? 1 : 0);
}

NamedSolution *NamedSolution::fromstring(string s) {
  unsigned int idx = 0;
  if (idx + 4 > s.length()) return 0; int d = shout(4, s, idx);
  
  if (idx + 4 > s.length()) return 0; int nl = shout(4, s, idx);
  if (idx + nl > s.length()) return 0;
  string na = s.substr(idx, nl);
  idx += nl;
  
  if (idx + 4 > s.length()) return 0; int dl = shout(4, s, idx);
  if (idx + dl > s.length()) return 0;
  string de = s.substr(idx, dl);
  idx += dl;
  
  if (idx + 4 > s.length()) return 0; int sl = shout(4, s, idx);
  if (idx + sl > s.length()) return 0;
  string ss = s.substr(idx, sl);
  idx += sl;
  
  Solution *so = Solution::fromstring(ss);
  if (!so) return 0;
  
  bool bm;
  if (idx + 4 > s.length()) bm = false;
  else {
    int bl = shout(4, s, idx);
    idx += 4;
    bm = !!bl;
  }
  
  return new NamedSolution(so, na, de, d, bm);
}

void NamedSolution::destroy() {
  sol->destroy();
}

int NamedSolution::compare(NamedSolution *l, NamedSolution *r) {
  /* PERF */
  return l->tostring().compare(r->tostring());
}

namespace {

typedef PtrList<NamedSolution> NSList;

struct hashsolsetentry {
  string md5;
  NSList *solset;
  static unsigned int hash(string k);
  string key() { return md5; }
  void destroy() { 
    while (solset) NSList::pop(solset)->destroy();
    delete this; 
  }
  hashsolsetentry(string m, NSList *s) : md5(m), solset(s) {}
  static int compare(hashsolsetentry *l, hashsolsetentry *r) {
    return l->md5.compare(r->md5);
  }
};

struct hashratentry {
  string md5;
  Rating *rat;
  static unsigned int hash(string k);
  string key() { return md5; }
  void destroy() { delete rat; delete this; }
  hashratentry(string m, Rating *r) : md5(m), rat(r) {}
  static int compare(hashratentry *l, hashratentry *r) {
    return l->md5.compare(r->md5);
  }
};

struct playerreal : public Player {
  static playerreal *Create(const string &n);

  Chunks *getchunks() { return ch; }

  Solution *getsol(string md5);

  Rating *getrating(string md5);
  
  void putrating(string md5, Rating *rat);

  bool writefile();

  static playerreal *FromFile(const string &file);
  /* call with file already open with cursor
     after the player magic. (Also pass filename
     since the player remembers this.)
     caller will close checkfile */
  static playerreal *fromfile_text(string fname, CheckFile *);

  /* XX this one is wrong now; it returns "levels solved"
     not total number of solutions */
  int num_solutions() { return sotable->items; }
  int num_ratings() { return ratable->items; }

  PtrList<Solution> *all_solutions();

  virtual ~playerreal() {
    sotable->destroy();
    ratable->destroy();
    if (ch) ch->destroy();
  };

 private:

  void deleteoldbackups();
  static string backupfile(string fname, int epoch);
  bool writef(string);
  bool writef_text(string);

  hashtable<hashsolsetentry, string> *sotable;
  hashtable<hashratentry, string> *ratable;

  virtual PtrList<NamedSolution> *solutionset(string md5);
  virtual void setsolutionset(string md5, PtrList<NamedSolution> *);

  virtual void addsolution(string md5, NamedSolution *ns, bool def_candidate);
  virtual bool hassolution(string md5, Solution *what);

  Chunks *ch;

};

bool playerreal::hassolution(string md5, Solution *what) {
  string whats = what->tostring();

  for (NSList *l = solutionset(md5); l; l = l->next) {
    // printf(" %s == %s ?\n", Base64::Encode(whats).c_str(), (Base64::Encode(l->head->sol->tostring())).c_str());
    if (l->head->sol->tostring() == whats) return true;
  }

  return false;

}

PtrList<Solution> *playerreal::all_solutions() {
  PtrList<Solution> *l = nullptr;

  for (int i = 0; i < sotable->allocated; i++) {
    PtrList<hashsolsetentry> *col = sotable->data[i];
    while (col) {
      NSList *these = col->head->solset;
      while (these) {
	l = new PtrList<Solution>(these->head->sol, l);
	these = these->next;
      }
      col = col->next;
    }
  }
  return l;
}

/* XXX these two should be in some general util file ... */
/* endianness doesn't matter here */
unsigned int hashsolsetentry::hash(string k) {
  return *(unsigned int*)(k.c_str());
}

unsigned int hashratentry::hash(string k) {
  return *(unsigned int*)(k.c_str());
}

playerreal *playerreal::Create(const string &n) {
  std::unique_ptr<playerreal> p{new playerreal()};
  if (!p.get()) return 0;
  
  p->name = n;
  
  p->sotable = hashtable<hashsolsetentry,string>::create(HASHSIZE);
  p->ratable = hashtable<hashratentry,string>::create(HASHSIZE);

  p->ch = Chunks::create();

  p->webid = 0;
  p->webseqh = 0;
  p->webseql = 0;

  if (!p->sotable) return 0;
  if (!p->ratable) return 0;
  if (!p->ch) return 0;

  /* set default preferences */
  Prefs::defaults(p.get());

  return p.release();
}

Solution *playerreal::getsol(string md5) {
  NSList *l = solutionset(md5);
  /* first try to find a non-bookmark Solution */
  for (NSList *tmp = l; tmp; tmp = tmp->next) {
    if (!tmp->head->bookmark) return tmp->head->sol;
  }
  /* otherwise just return the first bookmark */
  if (!l) return 0;
  else return l->head->sol;
}

NSList *playerreal::solutionset(string md5) {
  hashsolsetentry *he = sotable->lookup(md5);

  if (he) return he->solset;
  else return 0;
}

/* maintain the invariant that if the list exists, it
   is non-empty */
void playerreal::setsolutionset(string md5, NSList *ss) {
  if (ss) {
    hashsolsetentry *he = sotable->lookup(md5);
    if (he) {
      NSList *old = he->solset;
      he->solset = ss;
      /* delete old */
      while (old) NSList::pop(old)->destroy();
    } else {
      sotable->insert(new hashsolsetentry(md5, ss));
    }
  } else {
    /* otherwise, we are removing it */
    sotable->remove(md5);
  }
}


Rating *playerreal::getrating(string md5) {
  hashratentry *re = ratable->lookup(md5);
  
  if (re) return re->rat;
  else return nullptr;
}

void playerreal::addsolution(string md5, NamedSolution *ns,
			     bool def_candidate) {
  hashsolsetentry *he = sotable->lookup(md5);

  if (he && he->solset) {

    NamedSolution *headsol = he->solset->head;

    // Reasons we might add the solution.
    // XXX This was partly written code; didn't compile... -tom7 14 Aug 2016
    // bool is_bookmark = sol->bookmark;
    

    /* always add, even if it's worse */
    if (def_candidate) {

      /* only if it doesn't already exist..? */
#     if 0
      for (PtrList<NamedSolution> *tmp = he->solset;
	   tmp; tmp = tmp->next) {

      }
#     endif

      /* put it at end, so it doesn't take over
	 default */
      PtrList<NamedSolution>::push_tail(he->solset,
					ns->clone());

    /* XXX this code path is not used now */
    /* only added if it's better than the default, or
       if the default is a bookmark */
    } else if (ns->sol->length < headsol->sol->length ||
	       (!ns->bookmark && headsol->bookmark)) {
      /* replace */
      he->solset->head = ns->clone();
      headsol->destroy();
    } else {
      /* discard this solution */
    }

  } else {
    /* there's no solution set; create a new one. */
    NamedSolution *nsmine = ns->clone();
    NSList *l = new NSList(nsmine, 0);

    sotable->insert(new hashsolsetentry(md5, l));
  }
}

void playerreal::putrating(string md5, Rating *rat) {
  hashratentry *re = ratable->lookup(md5);

  if (re && re->rat) {
    /* overwrite */
    delete re->rat;
    re->rat = rat;
  } else ratable->insert(new hashratentry(md5, rat));

}

/* in seconds */
#define BACKUP_FREQ ((24 * (60 * (60 /* minutes */) /* hours */) /* days */) * 5)
#define N_BACKUPS 4

string playerreal::backupfile(string fname, int epoch) {
  return fname + ".~" + itos(epoch);
}

/* get rid of old backups, if any */
void playerreal::deleteoldbackups() {
  DIR *dir = opendir(".");
  if (!dir) return;

  /* XX must agree with backupfile */
  string basename = 
#   ifdef WIN32
        util::lcase(
#   else
	(
#   endif
	  fname + ".~");

  dirent *de;
  int n = 0;
  int oldest = (time(0) / BACKUP_FREQ) + 1 ;
  while ((de = readdir(dir))) {
    string f = 
#     ifdef WIN32
        util::lcase(
#     else
	(
#     endif
      de->d_name);

	if (f.substr(0, basename.length()) ==
	    basename) {
	  string sage = f.substr(basename.length(),
				 f.length() - basename.length());
	  int age = atoi(sage.c_str());
  
	  /* check that it's a valid number ... */
	  if (age && sage == itos(age)) {
	    /* printf("saw '%s' with age %d\n", f.c_str(), age); */
	    n++;
	    if (age < oldest) oldest = age;
	  }
	}
  } /* while */

  closedir(dir);

  if (n > N_BACKUPS) {
    
    string delme = basename + itos(oldest);
    if (util::existsfile(delme) && util::remove(delme)) {
      /* try deleting again */
      /* printf("deleted backup #%d\n", oldest); */
      deleteoldbackups();
    }
  }
}

bool playerreal::writefile() {

  /* Back up the player file. */
  if (Prefs::getbool(this, PREF_BACKUP_PLAYER)) {
    int epoch = time(0) / BACKUP_FREQ;
    
    /* did we already back up in this epoch? */
    string tf = backupfile(fname, epoch);
    if (!util::existsfile(tf)) {
      writef(tf);
 
    }
    deleteoldbackups();
  }

  /* anyway, always write the real file */
  return writef(fname);
}

/* now always write as text file */
bool playerreal::writef(string file) {
  return writef_text(file);
}

bool playerreal::writef_text(string file) {
  FILE *f = fopen(file.c_str(), "wb");

  if (!f) return 0;

  fprintf(f, "%s\n", PLAYERTEXT_MAGIC);
  fprintf(f,
	  "%d\n"
	  "%d\n"
	  "%d\n", webid, webseqh, webseql);

  /* write ignored fields; for later expansion... */
  for (int u = 0 ; u < IGNORED_FIELDS; u++) fprintf(f, "0\n");
  
  fprintf(f, "%s\n", name.c_str());

  fprintf(f, SOLMARKER "\n");
  /* fprintf(f, "%d\n", sotable->items); */

  {
  for (int i = 0; i < sotable->allocated; i++) {
    PtrList<hashsolsetentry>::sort(hashsolsetentry::compare, sotable->data[i]);
    for (PtrList<hashsolsetentry> *tmp = sotable->data[i]; 
	 tmp; 
	 tmp = tmp->next) {
      fprintf(f, "%s * %s\n", MD5::Ascii(tmp->head->md5).c_str(),
	      /* assume at least one solution */
	      Base64::Encode(tmp->head->solset->head->tostring()).c_str());
      /* followed by perhaps more solutions marked with @ */
      /* sort them first, in place */
      NSList::sort(NamedSolution::compare, tmp->head->solset->next);

      for (NSList *rest = tmp->head->solset->next;
	   rest;
	   rest = rest->next) {
	fprintf(f, "  %s\n", 
		Base64::Encode(rest->head->tostring()).c_str());
      }
      /* end it (makes parsing easier) */
      fprintf(f, "!\n");
    }
  }
  }

  fprintf(f, RATMARKER "\n");
  /* fprintf(f, "%d\n", ratable->items); */

  /* ditto... */

  for (int i = 0; i < ratable->allocated; i++) {
    PtrList<hashratentry>::sort(hashratentry::compare, ratable->data[i]);
    for (PtrList<hashratentry> *tmp = ratable->data[i]; 
	 tmp; 
	 tmp = tmp->next) {
      string md5ascii = MD5::Ascii(tmp->head->md5).c_str();
      fprintf(f, "%s %s\n",
	      md5ascii.c_str(),
	      Base64::Encode(tmp->head->rat->tostring()).c_str());
    }
  }

  fprintf(f, PREFMARKER "\n");

  /* write chunks */
  /* ch->tostring() */
  fprintf(f, "%s\n", Base64::Encode(ch->tostring()).c_str());


  fclose(f);
  return 1;
}

#define FF_FAIL(s) do { printf("Bad player: %s: %s\n", \
			       fname.c_str(), s);      \
		        return 0; } while (0)
// #define FF_FAIL(s) return 0;

playerreal *playerreal::fromfile_text(string fname, CheckFile *cf) {
  std::unique_ptr<playerreal> p {playerreal::Create("")};
  if (!p.get()) FF_FAIL("out of memory?");
  p->fname = fname;

  string s;

  /* strip newline after magic */
  if (!(cf->getline(s) && s == "")) FF_FAIL("newline after magic");

  if (!cf->getline(s)) FF_FAIL("no webid"); p->webid = util::stoi(s);
  if (!cf->getline(s)) FF_FAIL("no seqh");  p->webseqh = util::stoi(s);
  if (!cf->getline(s)) FF_FAIL("no seql");  p->webseql = util::stoi(s);

  /* ignored fields for now */
  for (int z = 0; z < IGNORED_FIELDS; z++) {
    if (!cf->getline(s)) FF_FAIL("ignored fields");
  }

  if (!cf->getline(p->name)) FF_FAIL("player name");

  /* expect solution marker now */
  if (!cf->getline(s) || s != SOLMARKER) FF_FAIL("solution marker");

  /* now read solutions until -- ratings */

  for ( ;; ) {
    string l;
    if (!cf->getline(l)) FF_FAIL("expected solution");
    
    /* maybe this is the end? */
    if (l == RATMARKER) break;
    
    string mda = util::chop(l);
    string md;
    if (!MD5::UnAscii(mda, md)) FF_FAIL(((string)"bad md5 " + mda).c_str());
    
    string next = util::chop(l);
    
    if (next == "*") {
      /* default first */
      string solstring = Base64::Decode(util::chop(l));
      NamedSolution *ns = NamedSolution::fromstring(solstring);
      if (!ns) FF_FAIL("bad namedsolution");

      NSList *solset = new NSList(ns, 0);
      NSList **etail = &solset->next;
      
      /* now, any number of other solutions */
      for (;;) {
	if (!cf->getline(l)) FF_FAIL("expected more solutions");
	string tok = util::chop(l);
	if (tok == "!") break;
	else {
	  NamedSolution *ns = NamedSolution::fromstring(Base64::Decode(tok));
	  if (!ns) FF_FAIL("additional solution was bad");
	  /* and append it */
	  *etail = new NSList(ns, 0);
	  etail = &((*etail)->next);
	}
      }
      
      /* add a whole solution set */
      p->sotable->insert(new hashsolsetentry(md, solset));

    } else {
      /* old style singleton solutions */
      string solstring = Base64::Decode(next);
      Solution *sol = Solution::fromstring(solstring);
      if (!sol) FF_FAIL("bad oldstyle solution");
      NamedSolution ns(sol, "Untitled", p->name, 0);
      p->addsolution(md, &ns, false);
      sol->destroy();
    }
  }

  /* already read rating marker */

  for ( ;; ) {
    string l;
    if (!cf->getline(l)) FF_FAIL("expected rating");

    if (l == PREFMARKER) break;

    string md = util::chop(l);
    if (!MD5::UnAscii(md, md)) FF_FAIL("bad rating md5");

    string ratstring = Base64::Decode(util::chop(l));
    Rating *rat = Rating::FromString(ratstring);

    if (!rat) FF_FAIL("bad rating");

    /* ignore rest of line */
    p->putrating(md, rat);
  }

  /* already read pref marker */

  string cs; 
  if (!cf->getline(cs)) FF_FAIL("expected prefs");
  p->ch = Chunks::fromstring(Base64::Decode(cs));

  if (!p->ch) FF_FAIL("bad prefs");

  return p.release();
}


playerreal *playerreal::FromFile(const string &file) {
  CheckFile *cf = CheckFile::create(file);
  if (!cf) return nullptr;
  Extent<CheckFile> ecf(cf);

  string s;
  if (!cf->read(PLAYER_MAGICS_LENGTH, s)) return nullptr;
  
  /* binary or text format? */
  if (s == PLAYERTEXT_MAGIC) return fromfile_text(file, cf);
  else return nullptr;
}  

}  // namespace

Player *Player::Create(const string &n) {
  return playerreal::Create(n);
}

Player *Player::FromFile(const string &file) {
  return playerreal::FromFile(file);
}
