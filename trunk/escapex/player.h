
#ifndef __PLAYER_H
#define __PLAYER_H

#include "level.h"
#include "rating.h"
#include "chunks.h"
#include "solution.h"

/* Database for a single player.
   Stores the player's solutions, ratings, and preferences.
*/

// A named solution might actually solve
// the level, or it might be a mere bookmark.
// Value semantics.
struct NamedSolution {
  string name;
  Solution sol;
  /* only when bookmark = false */
  string author;
  int date = 0;
  bool bookmark = false;
  NamedSolution(Solution s, string na = "Untitled",
                string au = "Unknown", int da = 0,
                bool bm = false);
  string ToString() const;
  static bool FromString(const string &s, NamedSolution *ns);
  static int Compare(NamedSolution *l, NamedSolution *r);
  NamedSolution(); /* needed for selector */
  ~NamedSolution() {}
};

/* interface only */
struct Player {
  string fname;
  string name;

  /* online stuff */
  /* Unique id online */
  int webid;
  /* secrets */
  int webseqh;
  int webseql;

  static Player *Create(const string &n);
  static Player *FromFile(const string &file);

  /* returns the chunk structure for this player. After making any
     changes (insertions, modifications), call writefile to save them.
     The Chunks object remains owned by the Player. */
  virtual Chunks *GetChunks() = 0;

  /* Get the default solution (if any) for the level whose md5
     representation is "md5", or returns null. Solution remains
     owned by Player. */
  virtual const Solution *GetSol(const string &md5) const = 0;
  // Return the length of the default solution, or 0 if none.
  // (All valid solutions have positive length.)
  virtual int GetSolLength(const string &md5) const = 0;

  // Set the default solution (returned by GetSol) verified
  // for this level.
  // XXX this is gross; verification should be cached elsewhere
  virtual void SetDefaultVerified(const string &md5) = 0;
  // Set the solution within the SolutionSet at this index as
  // verified. Doesn't invalidate solution set reference. XXX
  // also gross.
  virtual void SetVerified(const string &md5, int idx) = 0;

  virtual Rating *getrating(const string &md5) const = 0;

  /* Always overwriting an existing rating.
     there is just one rating per level. Takes ownership of
     the Rating object. */
  virtual void PutRating(const string &md5, Rating *rat) = 0;

  /* Record a change on disk. this will also manage
     backups of the player file. */
  virtual bool WriteFile() = 0;

  virtual int num_solutions() const = 0;
  virtual int num_ratings() const = 0;

  /* For solution recovery; get every solution in
     the player, regardless of the level it is for */
  virtual vector<Solution> AllSolutions() const = 0;

  /* Return a reference to the solution set (perhaps empty) for the
     level indicated. The first solution, if any, is the default. */
  virtual const vector<NamedSolution> &SolutionSet(const string &md5) const = 0;

  // Overwrites the solution set for a particular level MD5.
  virtual void SetSolutionSet(const string &md5,
                              vector<NamedSolution> solset) = 0;

  /* Simply add a new solution to the set. If def_candidate is true
     and the solution is not a bookmark, it might be made the
     default solution. The solution is always added. */
  virtual void AddSolution(const string &md5, NamedSolution ns,
                           bool def_candidate = false) = 0;

  /* is this solution already in the solution set? */
  virtual bool HasSolution(const string &md5, const Solution &what) = 0;

  virtual ~Player() {};
};

#endif
