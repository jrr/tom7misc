#ifndef _ESCAPE_DIRINDEX_H
#define _ESCAPE_DIRINDEX_H

/* a directory index provides meta-information about a directory in a
   managed collection (ie, triage). it includes:
     * a nice name (title) for the directory
     * global ratings for each level
     * dates that the levels were created (can be used to sort)
     * current speed records for each level
     * (your idea here)

   indices are written as index.esi in each managed directory.

   Note: This is being replaced by leveldb for the 4.0 series.
*/

#include "escape-util.h"

#define DIRINDEXNAME "index.esi"
#define WEBINDEXNAME "webindex.esi"

/* all as totals */
struct RateStatus {
  int nvotes = 0;
  int difficulty = 0;
  int style = 0;
  int rigidity = 0;
  int cooked = 0;
  int solved = 0;

  RateStatus() {}
};

struct DirIndex {
  /* make an empty index, suitable for later writing to disk */
  static DirIndex *Create();

  virtual ~DirIndex() {}

  /* read from disk */
  static DirIndex *FromFile(const string &f);

  static bool IsIndex(const string &f);

  virtual void WriteFile(const string &f) = 0;
  virtual void AddEntry(const string &filename, RateStatus v,
                        int date, int speedrecord, int owner) = 0;

  virtual bool GetEntry(const string &filename, RateStatus &v,
                        int &date, int &speedrecord, int &owner) = 0;

  /* true if this is a managed collection */
  virtual bool WebCollection() const = 0;

  string title;
};

#endif
