
#include "player.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <assert.h>

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

NamedSolution::NamedSolution() {}

NamedSolution::NamedSolution(Solution s, string na,
                             string au, int da, bool bm) {
  name = na;
  sol = std::move(s);
  author = au;
  date = da;
  bookmark = bm;
}

string NamedSolution::ToString() const {
  const string solstring = sol.ToString();

  return
    sizes(date) +
    sizes(name.length()) + name +
    sizes(author.length()) + author +
    sizes(solstring.length()) + solstring +
    sizes(bookmark ? 1 : 0);
}

// static
bool NamedSolution::FromString(const string &s, NamedSolution *ns) {
  unsigned int idx = 0;
  if (idx + 4 > s.length()) return false;
  int d = shout(4, s, idx);

  if (idx + 4 > s.length()) return false;
  int nl = shout(4, s, idx);
  if (idx + nl > s.length()) return false;
  string name = s.substr(idx, nl);
  idx += nl;

  if (idx + 4 > s.length()) return false;
  int dl = shout(4, s, idx);
  if (idx + dl > s.length()) return false;
  string de = s.substr(idx, dl);
  idx += dl;

  if (idx + 4 > s.length()) return false;
  int sl = shout(4, s, idx);
  if (idx + sl > s.length()) return false;
  string ss = s.substr(idx, sl);
  idx += sl;

  Solution so;
  if (!Solution::FromString(ss, &so)) return false;

  bool bm;
  if (idx + 4 > s.length()) bm = false;
  else {
    int bl = shout(4, s, idx);
    idx += 4;
    bm = !!bl;
  }

  *ns = NamedSolution(std::move(so), name, de, d, bm);
  return true;
}

// static
int NamedSolution::Compare(NamedSolution *l, NamedSolution *r) {
  /* PERF */
  return l->ToString().compare(r->ToString());
}

namespace {

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

struct Player_ : public Player {
  static Player_ *Create(const string &n);

  Chunks *getchunks() override { return ch; }

  // XXX this used to return a non-bookmark if it exists. The
  // solutions should now be kept in a canonical order where real
  // solutions go fist.
  const Solution *GetSol(const string &md5) const override {
    const vector<NamedSolution> &sols = this->SolutionSet(md5);
    if (sols.empty()) return nullptr;
    return &sols[0].sol;
  }

  int GetSolLength(const string &md5) const override {
    if (const Solution *s = GetSol(md5))
      return s->Length();
    return 0;
  }

  void SetDefaultVerified(const string &md5) override {
    SetVerified(md5, 0);
  }

  void SetVerified(const string &md5, int idx) override {
    auto it = soltable.find(md5);
    if (it == soltable.end()) return;
    assert(idx >= 0 && idx < it->second.size());
    it->second[idx].sol.verified = true;
  }

  Rating *getrating(const string &md5) const override;

  void putrating(string md5, Rating *rat) override;

  bool writefile() override;

  static Player_ *FromFile(const string &file);
  /* call with file already open with cursor
     after the player magic. (Also pass filename
     since the player remembers this.)
     caller will close checkfile */
  static Player_ *fromfile_text(string fname, CheckFile *);

  /* XX this one is wrong now; it returns "levels solved"
     not total number of solutions */
  int num_solutions() const override { return soltable.size(); }
  int num_ratings() const override { return ratable->items; }

  vector<Solution> AllSolutions() const override {
    vector<Solution> out;

    for (const auto &p : soltable) {
      for (const NamedSolution &ns : p.second) {
        out.push_back(ns.sol);
      }
    }

    return out;
  }

  virtual ~Player_() {
    ratable->destroy();
    if (ch) ch->destroy();
  };

  bool HasSolution(const string &md5, const Solution &what) override {
    for (const NamedSolution &ns : SolutionSet(md5)) {
      if (Solution::Equal(ns.sol, what)) return true;
    }
    return false;
  }

  const vector<NamedSolution> &SolutionSet(const string &md5) const override {
    auto it = soltable.find(md5);
    if (it == soltable.end()) return empty_solutionset;
    else return it->second;
  }

 private:

  void deleteoldbackups();
  static string backupfile(string fname, int epoch);
  bool writef_text(const string &f);

  // Keys are (raw) MD5 strings. The order of the solutions
  // matters; the first one is the default solution.
  unordered_map<string, vector<NamedSolution>> soltable;
  hashtable<hashratentry, string> *ratable;

  const vector<NamedSolution> empty_solutionset;

  void SetSolutionSet(const string &md5, vector<NamedSolution> ns) override {
    if (ns.empty()) {
      soltable.erase(md5);
    } else {
      soltable[md5] = std::move(ns);
    }
  }

  void AddSolution(const string &md5, NamedSolution ns,
                   bool def_candidate) override;

  Chunks *ch;
};

unsigned int hashratentry::hash(string k) {
  return *(unsigned int*)(k.c_str());
}

Player_ *Player_::Create(const string &n) {
  std::unique_ptr<Player_> p{new Player_()};
  if (!p.get()) return 0;

  p->name = n;

  p->ratable = hashtable<hashratentry,string>::create(HASHSIZE);

  p->ch = Chunks::create();

  p->webid = 0;
  p->webseqh = 0;
  p->webseql = 0;

  if (!p->ratable) return 0;
  if (!p->ch) return 0;

  /* set default preferences */
  Prefs::defaults(p.get());

  return p.release();
}

Rating *Player_::getrating(const string &md5) const {
  const hashratentry *re = ratable->lookup(md5);

  if (re) return re->rat;
  else return nullptr;
}

void Player_::AddSolution(const string &md5, NamedSolution ns,
                          bool def_candidate) {
  vector<NamedSolution> &row = soltable[md5];
  if (row.empty()) {
    // Always add if we have no solutions.
    row.push_back(std::move(ns));
  } else {
    // First is the default, so that's what we compare against
    // when thinking about replacing the default.
    const NamedSolution &first = row[0];
    // Is the default not even a solution?
    const bool first_real_solution =
      !ns.bookmark && first.bookmark;
    // Is it a solution and faster than whatever the default is
    // (bookmark or not)?
    const bool faster_solution =
      !ns.bookmark &&
      ns.sol.Length() < first.sol.Length();
    // I made the solution, and the default is by someone else.
    const bool is_my_solution =
      !ns.bookmark &&
      ns.author == this->name &&
      first.author != this->name;

    if (def_candidate &&
        (first_real_solution || faster_solution || is_my_solution)) {
      // Make default by inserting at the beginning.
      row.insert(row.begin(), std::move(ns));
    } else {
      // Since bookmarks are inserted manually, don't dedupe.
      if (!ns.bookmark) {
        // For full solutions, don't insert duplicates. It's a
        // duplicate if it has the exact same moves and either has the
        // default name or has the same name/author as the existing
        // one. We treat untitled solutions specially because they are
        // inserted after solving a level, which happens when watching
        // a solution and pressing enter at the end.
        const bool untitled = ns.name == "Untitled";

        for (const NamedSolution &other : row) {
          if ((untitled ||
               (other.author == ns.author &&
                other.name == ns.name)) &&
              !other.bookmark &&
              Solution::Equal(other.sol, ns.sol)) {
            return;
          }
        }
      }
      // Otherwise, just add it at the end.
      row.push_back(std::move(ns));
    }
  }
}

void Player_::putrating(string md5, Rating *rat) {
  hashratentry *re = ratable->lookup(md5);
  if (re != nullptr && re->rat) {
    /* overwrite */
    delete re->rat;
    re->rat = rat;
  } else ratable->insert(new hashratentry(md5, rat));

}

/* in seconds */
#define BACKUP_FREQ ((24 * (60 * (60 /* minutes */) /* hours */) /* days */) * 5)
#define N_BACKUPS 4

string Player_::backupfile(string fname, int epoch) {
  return fname + ".~" + itos(epoch);
}

/* get rid of old backups, if any */
void Player_::deleteoldbackups() {
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

bool Player_::writefile() {

  /* Back up the player file. */
  if (Prefs::getbool(this, PREF_BACKUP_PLAYER)) {
    int epoch = time(0) / BACKUP_FREQ;

    /* did we already back up in this epoch? */
    string tf = backupfile(fname, epoch);
    if (!util::existsfile(tf)) {
      writef_text(tf);
    }
    deleteoldbackups();
  }

  /* anyway, always write the real file */
  return writef_text(fname);
}

/* now always write as text file */
bool Player_::writef_text(const string &file) {
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

  // Sort so that text formats diff better (e.g. in version control)
  map<string, const vector<NamedSolution> *> sorted_sols;
  for (const auto &p : soltable) {
    if (!p.second.empty()) {
      sorted_sols[p.first] = &p.second;
    }
  }

  for (const auto &p : sorted_sols) {
    // We checked above that we have at least one solution.
    fprintf(f, "%s * %s\n", MD5::Ascii(p.first).c_str(),
            Base64::Encode(p.second->at(0).ToString()).c_str());
    /* followed by perhaps more solutions marked with @ */

    for (int i = 1; i < p.second->size(); i++) {
      const NamedSolution &ns = p.second->at(i);
      fprintf(f, "  %s\n", Base64::Encode(ns.ToString()).c_str());
    }

    /* end it (makes parsing easier) */
    fprintf(f, "!\n");
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

Player_ *Player_::fromfile_text(string fname, CheckFile *cf) {
  std::unique_ptr<Player_> p {Player_::Create("")};
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

  for (;;) {
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
      string nsolstring = Base64::Decode(util::chop(l));
      NamedSolution ns;
      if (!NamedSolution::FromString(nsolstring, &ns)) {
        FF_FAIL("bad namedsolution");
      }

      vector<NamedSolution> solutionset = {std::move(ns)};

      /* now, any number of other solutions */
      for (;;) {
        if (!cf->getline(l)) FF_FAIL("expected more solutions");
        string tok = util::chop(l);
        if (tok == "!") break;
        else {
          NamedSolution ns;
          if (!NamedSolution::FromString(Base64::Decode(tok), &ns)) {
            FF_FAIL("additional solution was bad");
          }
          solutionset.push_back(std::move(ns));
        }
      }

      /* add a whole solution set */
      p->soltable[md] = std::move(solutionset);
    } else {
      // XXX2016 this can probably be deleted?
      /* old style singleton solutions */
      string solstring = Base64::Decode(next);
      Solution sol;
      if (!Solution::FromString(solstring, &sol)) {
        FF_FAIL("bad oldstyle solution");
      }
      p->AddSolution(md,
                     NamedSolution(sol, "Untitled", p->name, 0),
                     false);
    }
  }

  /* already read rating marker */

  for (;;) {
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


Player_ *Player_::FromFile(const string &file) {
  std::unique_ptr<CheckFile> cf{CheckFile::Create(file)};
  if (cf.get() == nullptr) return nullptr;

  string s;
  if (!cf->read(PLAYER_MAGICS_LENGTH, s)) return nullptr;

  /* binary or text format? */
  if (s == PLAYERTEXT_MAGIC) return fromfile_text(file, cf.get());
  else return nullptr;
}

}  // namespace

Player *Player::Create(const string &n) {
  return Player_::Create(n);
}

Player *Player::FromFile(const string &file) {
  return Player_::FromFile(file);
}
