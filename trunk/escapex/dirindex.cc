#include "dirindex.h"
#include "util.h"
#include "hashtable.h"
#include "extent.h"
#include "checkfile.h"

/* no reason for this to be particularly big,
   since it is limited to a single directory */
#define HASHSIZE 257

#define INDEX_IGNORED_FIELDS 6

#define INDEXMAGIC "ESXI"
#define INDEX2MAGIC "ESXi" /* now obsolete. but don't reuse */
#define INDEX3MAGIC "ESX!"

namespace {
struct RAEntry {
  static unsigned int hash(string k) {
    return hash_string(k);
  }

  string key() {
    return filename;
  }
  string filename;
  RateStatus v;
  int date = 0;
  int speedrecord = 0;

  int owner = 0;

  void destroy() { delete this; }

  RAEntry(string s, RateStatus vv, int d, int sr, int o)
    : filename(s), v(vv), date(d), speedrecord(sr), owner(o) {}
  RAEntry() {}
};

struct DirIndex_ : public DirIndex {
  static DirIndex_ *Create();

  void writefile(string) override;

  ~DirIndex_() override {
    if (tab) tab->destroy();
  }
  void addentry(string filename, RateStatus v,
		int date, int speedrecord, int owner) override;

  static void writeone(RAEntry *i, FILE *f);

  bool getentry(string filename, RateStatus &v, int &, int &, int &o) override;

  bool webcollection() const override { return isweb; }

  /* mapping filenames to RAEntry */
  hashtable<RAEntry, string> *tab = nullptr;

  int isweb = 0;
};


bool DirIndex_::getentry(string filename,
			 RateStatus &v, int &d, int &sr, int &o) {
  if (RAEntry *e = tab->lookup(filename)) {
    v = e->v;
    d = e->date;
    sr = e->speedrecord;
    o = e->owner;
    return true;
  } else return false;
}

DirIndex_ *DirIndex_::Create() {
  std::unique_ptr<DirIndex_> dr{new DirIndex_};

  if (dr.get() == nullptr) return nullptr;

  dr->title = "No name";
  dr->isweb = 0;
  dr->tab = hashtable<RAEntry, string>::create(HASHSIZE);

  if (!dr->tab) return nullptr;

  return dr.release();
}

/* argument to hashtable::app */
void DirIndex_::writeone(RAEntry *i, FILE *f) {
  fprintf(f, "%s %d %d %d %d %d %d %d %d %d\n",
          i->filename.c_str(),
          i->v.nvotes,
          i->v.difficulty,
          i->v.style,
          i->v.rigidity,
          i->v.cooked,
          i->v.solved,
          i->date,
          i->speedrecord,
          i->owner);
}

void DirIndex_::writefile(string fname) {
  FILE *f = fopen(fname.c_str(), "wb");
  if (!f) return; /* XXX? */

  fprintf(f, INDEX3MAGIC "\n");

  /* single line gives title */
  fprintf(f, "%s\n", title.c_str());

  /* a few ignored lines */
  for (int i = 0; i < INDEX_IGNORED_FIELDS; i++) fprintf(f, "\n");

  /* XXX sort first */

  /* then write each file */
  hashtable_app<RAEntry, string, FILE *>(tab, writeone, f);

  fclose(f);

}

void DirIndex_::addentry(string f, RateStatus v,
                       int date, int speedrecord, int owner) {
  tab->insert(new RAEntry(f, v, date, speedrecord, owner));
}

}  // namespace

DirIndex *DirIndex::FromFile(const string &f) {
  std::unique_ptr<DirIndex_> dr{DirIndex_::Create()};

  if (dr.get() == nullptr) return nullptr;

  /* read old index files */
  string iii = util::readfilemagic(f, INDEXMAGIC);

  /* chop off magic, then erase leading whitespace */
  if (iii != "") {
    dr->title = util::losewhitel(iii.substr
                                 (strlen(INDEXMAGIC),
                                  iii.length() -
                                  strlen(INDEXMAGIC)));

    /* hashtable remains empty */
    return dr.release();
  } else {
    std::unique_ptr<CheckFile> cf{CheckFile::Create(f)};

    if (cf.get() == nullptr) return nullptr;

    /* check that it starts with v2 magic */
    string s;
    if (!cf->read(strlen(INDEX3MAGIC), s) ||
        s != INDEX3MAGIC) return nullptr;

    /* strip newline */
    if (!(cf->getline(s) && s == "")) return nullptr;

    if (!(cf->getline(dr->title))) return nullptr;

    for (int i = 0; i < INDEX_IGNORED_FIELDS; i++)
      if (!cf->getline(s)) return nullptr;

    while (cf->getline(s)) {
      RAEntry *rr = new RAEntry;
      Extent<RAEntry> re(rr);

      rr->filename = util::chop(s);
      rr->v.nvotes = util::stoi(util::chop(s));
      rr->v.difficulty = util::stoi(util::chop(s));
      rr->v.style = util::stoi(util::chop(s));
      rr->v.rigidity = util::stoi(util::chop(s));
      rr->v.cooked = util::stoi(util::chop(s));
      rr->v.solved = util::stoi(util::chop(s));
      rr->date = util::stoi(util::chop(s));
      rr->speedrecord = util::stoi(util::chop(s));
      rr->owner = util::stoi(util::chop(s));

      re.release();
      dr->tab->insert(rr);
    }

    dr->isweb = 1;
    return dr.release();
  }
}

bool DirIndex::isindex(const string &f) {
  return util::hasmagic(f, INDEXMAGIC) ||
         util::hasmagic(f, INDEX3MAGIC);
}

DirIndex *DirIndex::Create() {
  return DirIndex_::Create();
}
