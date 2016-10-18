#include "dirindex.h"

#include <unordered_map>

#include "util.h"
#include "checkfile.h"

#define INDEX_IGNORED_FIELDS 6

#define INDEXMAGIC "ESXI"
#define INDEX2MAGIC "ESXi" /* now obsolete. but don't reuse */
#define INDEX3MAGIC "ESX!"

namespace {
struct RAEntry {
  // XXX map key; delete?
  string filename;
  RateStatus v;
  int date = 0;
  int speedrecord = 0;
  int owner = 0;

  RAEntry(string s, RateStatus vv, int d, int sr, int o)
    : filename(s), v(vv), date(d), speedrecord(sr), owner(o) {}
  RAEntry() {}
};

struct DirIndex_ : public DirIndex {
  static DirIndex_ *Create();

  void WriteFile(const string &) override;

  ~DirIndex_() override {}

  void AddEntry(const string &filename, RateStatus v,
		int date, int speedrecord, int owner) override;

  bool GetEntry(const string &filename, 
		RateStatus &v, int &date, int &speed, int &o) override;

  bool WebCollection() const override { return isweb; }

  /* mapping filenames to RAEntry */
  unordered_map<string, RAEntry> tab;

  int isweb = 0;
};


bool DirIndex_::GetEntry(const string &filename,
			 RateStatus &v, int &d, int &sr, int &o) {
  auto it = tab.find(filename);
  if (it == tab.end()) return false;

  v = it->second.v;
  d = it->second.date;
  sr = it->second.speedrecord;
  o = it->second.owner;
  return true;
}

DirIndex_ *DirIndex_::Create() {
  std::unique_ptr<DirIndex_> dr{new DirIndex_};

  if (dr.get() == nullptr) return nullptr;

  dr->title = "No name";
  dr->isweb = 0;

  return dr.release();
}

void DirIndex_::WriteFile(const string &fname) {
  FILE *f = fopen(fname.c_str(), "wb");
  if (!f) return; /* XXX? */

  fprintf(f, INDEX3MAGIC "\n");

  /* single line gives title */
  fprintf(f, "%s\n", title.c_str());

  /* a few ignored lines */
  for (int i = 0; i < INDEX_IGNORED_FIELDS; i++) fprintf(f, "\n");

  /* XXX sort first */

  /* then write each file */
  for (const auto &p : tab) {
    const RAEntry &ent = p.second;
    fprintf(f, "%s %d %d %d %d %d %d %d %d %d\n",
	    ent.filename.c_str(),
	    ent.v.nvotes,
	    ent.v.difficulty,
	    ent.v.style,
	    ent.v.rigidity,
	    ent.v.cooked,
	    ent.v.solved,
	    ent.date,
	    ent.speedrecord,
	    ent.owner);
  }

  fclose(f);
}

void DirIndex_::AddEntry(const string &f, RateStatus v,
			 int date, int speedrecord, int owner) {
  tab[f] = RAEntry(f, v, date, speedrecord, owner);
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
      RAEntry rr;

      rr.filename = util::chop(s);
      rr.v.nvotes = util::stoi(util::chop(s));
      rr.v.difficulty = util::stoi(util::chop(s));
      rr.v.style = util::stoi(util::chop(s));
      rr.v.rigidity = util::stoi(util::chop(s));
      rr.v.cooked = util::stoi(util::chop(s));
      rr.v.solved = util::stoi(util::chop(s));
      rr.date = util::stoi(util::chop(s));
      rr.speedrecord = util::stoi(util::chop(s));
      rr.owner = util::stoi(util::chop(s));

      dr->tab.insert({rr.filename, rr});
    }

    dr->isweb = 1;
    return dr.release();
  }
}

bool DirIndex::IsIndex(const string &f) {
  return util::hasmagic(f, INDEXMAGIC) ||
         util::hasmagic(f, INDEX3MAGIC);
}

DirIndex *DirIndex::Create() {
  return DirIndex_::Create();
}
