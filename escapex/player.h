
#ifndef _ESCAPE_PLAYER_H
#define _ESCAPE_PLAYER_H

#include <string>

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
  std::string name;
  Solution sol;
  /* only when bookmark = false */
  std::string author;
  int date = 0;
  bool bookmark = false;
  NamedSolution(Solution s, std::string na = "Untitled",
                std::string au = "Unknown", int da = 0,
                bool bm = false);
  std::string ToString() const;
  static bool FromString(const std::string &s, NamedSolution *ns);
  static int Compare(NamedSolution *l, NamedSolution *r);
  NamedSolution(); /* needed for selector */
  ~NamedSolution() {}
};

/* interface only */
struct Player {
  std::string fname;
  std::string name;

  /* online stuff */
  /* Unique id online */
  int webid = 0;
  /* secrets */
  int webseqh = 0;
  int webseql = 0;

  static Player *Create(const std::string &n);
  static Player *FromFile(const std::string &file);

  /* returns the chunk structure for this player. After making any
     changes (insertions, modifications), call writefile to save them.
     The Chunks object remains owned by the Player. */
  virtual Chunks *GetChunks() = 0;

  /* Get the default solution (if any) for the level whose md5
     representation is "md5", or returns null. Solution remains
     owned by Player. */
  virtual const Solution *GetSol(const std::string &md5) const = 0;
  // Return the length of the default solution, or 0 if none.
  // (All valid solutions have positive length.)
  virtual int GetSolLength(const std::string &md5) const = 0;

  // Set the default solution (returned by GetSol) verified
  // for this level.
  // XXX this is gross; verification should be cached elsewhere
  virtual void SetDefaultVerified(const std::string &md5) = 0;
  // Set the solution within the SolutionSet at this index as
  // verified. Doesn't invalidate solution set reference. XXX
  // also gross.
  virtual void SetVerified(const std::string &md5, int idx) = 0;

  virtual Rating *getrating(const std::string &md5) const = 0;

  /* Always overwriting an existing rating.
     there is just one rating per level. Takes ownership of
     the Rating object. */
  virtual void PutRating(const std::string &md5, Rating *rat) = 0;

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
  virtual const vector<NamedSolution> &SolutionSet(const std::string &md5) const = 0;

  // Overwrites the solution set for a particular level MD5.
  virtual void SetSolutionSet(const std::string &md5,
                              std::vector<NamedSolution> solset) = 0;

  /* Simply add a new solution to the set. If def_candidate is true
     and the solution is not a bookmark, it might be made the
     default solution. The solution is always added. */
  virtual void AddSolution(const std::string &md5, NamedSolution ns,
                           bool def_candidate = false) = 0;

  /* is this solution already in the solution set? */
  virtual bool HasSolution(const std::string &md5, const Solution &what) = 0;

  virtual ~Player() {};
};

#endif
