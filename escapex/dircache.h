
#ifndef _ESCAPE_DIRCACHE_H
#define _ESCAPE_DIRCACHE_H

/* Loads the escape files in a directory (recursively)
   to check their solutions. Reports how many levels
   there are and how many are solved; caches this. Also
   loads the dirindex, if present (see dirindex.h).

   Note: This is being replaced by leveldb for the 4.0
   series.
*/

#include <memory>

#include "player.h"
#include "dirindex.h"

#define IGNOREFILE ".escignore"

/* abstract interface */
struct DirCache {
  static DirCache *Create(Player *p);
  virtual ~DirCache();

  virtual std::unique_ptr<DirIndex> GetIdx(const std::string &dir) = 0;

  /* lookup dir in the cache, sticking the result in idx.
     If successful, returns a pointer to the dirindex entry, which
     remains owned by the cache.
     optionally provide a callback function for progress */
  virtual DirIndex *Get(
      const std::string &dir, int &tot, int &sol,
      void (*prog)(void *data, int n, int total,
		   const std::string &subdir, int tks) = nullptr,
      void *prog_data = nullptr) = 0;
};

#endif
