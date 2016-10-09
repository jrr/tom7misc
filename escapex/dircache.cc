#include "escapex.h"

#include <memory>
#include <unordered_map>
#include <sys/stat.h>

#include "level.h"
#include "loadlevel.h"
#include "../cc-lib/md5.h"
#include "directories.h"
#include "util.h"
#include "dircache.h"
#include "progress.h"

namespace {

struct DirEntry {
  string dir;

  std::unique_ptr<DirIndex> index;

  /* these counts include recursive traversals */
  int total = 0;
  int solved = 0;

  DirEntry(string d, std::unique_ptr<DirIndex> i, int t, int s) :
    dir(d), index(std::move(i)), total(t), solved(s) {
    // printf("dircached %s\n", d.c_str());
  }
};

struct DirCache_ : public DirCache {
  Player *plr = nullptr;

  unordered_map<string, std::unique_ptr<DirEntry>> table;

  static DirCache_ *Create(Player *p) {
    std::unique_ptr<DirCache_> dc{new DirCache_()};
    if (!dc.get()) return nullptr;

    dc->plr = p;

    return dc.release();
  }

  std::unique_ptr<DirIndex> GetIdx(const string &dir) override;
  DirIndex *Get(const string &dir,
                int &tot, int &sol,
                void (*prog)(void *d, int n, int total,
                             const string &, const int) = 0,
                void *pd = 0) override;
};

/* make sure it starts with ./ */
static string normalize(string dir) {
  if (dir == "") return ".";
  if (dir[0] != '.') return "." DIRSEP + dir;
  else return dir;
}

/* read index, but don't put in table */
std::unique_ptr<DirIndex> DirCache_::GetIdx(const string &dir_in) {
  const string dir = normalize(dir_in);
  string ifile = dir + (string)DIRSEP WEBINDEXNAME;
  std::unique_ptr<DirIndex> idx{DirIndex::FromFile(ifile)};

  /* also try old name */
  if (idx.get() == nullptr) {
    idx.reset(DirIndex::FromFile(dir + (string)DIRSEP DIRINDEXNAME));
  }
  return idx;
}

DirIndex *DirCache_::Get(const string &dir_in,
                         int &tot, int &sol,
                         void (*prog)(void *d, int n, int total,
                                      const string &, const int),
                         void *pd) {
  // printf("get: %s\n", dir.c_str());

  const string dir = normalize(dir_in);
  // printf("normalized: %s\n", dir.c_str());
  
  auto it = table.find(dir);
  if (it == table.end()) {
    /* no entry. put it in the cache. */

    if (util::existsfile(dir + DIRSEP + IGNOREFILE)) {
      /* ignored dir */
      table[dir] = make_unique<DirEntry>(dir, nullptr, 0, 0);
      tot = 0;
      sol = 0;
      return nullptr;
    }

    /* printf("uncached and not ignored: '%s'\n", dir.c_str()); */

    /* calculate size (for callback) */
    int total = 0;
    if (prog) total = dirsize(dir.c_str());
    /* printf("  DIRCACHE: %d total\n", total); */

    std::unique_ptr<DirIndex> didx = GetIdx(dir);

    /*
       if (didx) printf("%s index: %s\n", ifile.c_str(), didx->title.c_str());
       else printf("%s no index\n", ifile.c_str());
    */

    /* init array */
    DIR *d = opendir(dir.c_str());
    if (d == nullptr) return nullptr;
    dirent *dire;

    int ttt = 0, sss = 0;
    int num = 0;

    while ( (dire = readdir(d)) ) {
      num++;
      if (prog) prog(pd, num, total, dir, PROGRESS_TICKS);

      string dn = dire->d_name;
      string ldn = dir + (string)DIRSEP + dn;

      if (util::isdir(ldn)) {

        /* can't include . or .., dumb to
           include CVS and .svn */
        if (!(dn == "." ||
              dn == "CVS" ||
              dn == ".svn" ||
              dn == "..")) {

          int tsub, ssub;
          if (Get(ldn, tsub, ssub, prog, pd) != nullptr) {
            ttt += tsub;
            sss += ssub;
          }
        }

      } else {
        string contents = util::readfilemagic(ldn, LEVELMAGIC);

        std::unique_ptr<Level> l = Level::FromString(contents);

        if (l.get() != nullptr) {
          string md5c = MD5::Hash(contents);

          ttt++;

          const Solution *s = plr->GetSol(md5c);
          if (s != nullptr) {
            if (s->verified) {
              sss++;
            } else {
              if (Level::Verify(l.get(), *s)) {
                plr->SetDefaultVerified(md5c);
                sss++;
              }
            }
          }
        }
      }
    }

    closedir(d);

    DirIndex *ret = didx.get();
    table[dir] = make_unique<DirEntry>(dir, std::move(didx), ttt, sss);

    tot = ttt;
    sol = sss;
    return ret;
  } else {
    /* memoized */
    tot = it->second->total;
    sol = it->second->solved;
    return it->second->index.get();
  }
}

}  // namespace

DirCache::~DirCache() {}

DirCache *DirCache::Create(Player *p) {
  return DirCache_::Create(p);
}
