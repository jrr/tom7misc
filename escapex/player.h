
#ifndef __PLAYER_H
#define __PLAYER_H

#include "level.h"
#include "hashtable.h"
#include "rating.h"
#include "util.h"
#include "chunks.h"

/* Database for a single player.
   Stores his solutions, mainly.
   Also now stores his ratings.
*/

/* A named solution might actually solve 
   the level, or it might be a mere bookmark. */
struct NamedSolution {
  string name;
  Solution *sol;
  /* only when bookmark = false */
  string author;
  int date;
  bool bookmark;
  NamedSolution(Solution *s, string na = "Untitled", 
                string au = "Unknown", int da = 0, 
                bool bm = false);
  string tostring();
  static NamedSolution *fromstring(string);
  void destroy();
  static int compare(NamedSolution *, NamedSolution *);
  NamedSolution(); /* needed for selector */

  /* deep clone; copies solution too */
  NamedSolution *clone();
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

  /* returns the chunk structure for this
     player. After making any changes
     (insertions, modifications), call
     writefile to save them. Don't free
     the chunks */
  virtual Chunks *getchunks() = 0;

  /* get the default solution (if any) for the map whose md5
     representation is "md5". don't free the solution! */
  virtual Solution *getsol(string md5) = 0;

  virtual rating * getrating(string md5) = 0;
  
  /* always overwriting an existing rating. 
     there is just one rating per level. */
  virtual void putrating(string md5, rating * rat) = 0;

  /* record a change on disk. this will also manage
     backups of the player file. */
  virtual bool writefile() = 0;

  virtual int num_solutions() = 0;
  virtual int num_ratings() = 0;

  /* for solution recovery; get every solution in
     the player, regardless of the level it is for */
  virtual PtrList<Solution> *all_solutions() = 0;

  /* return the solution set (perhaps empty) for the level indicated.
     the list and its contents remain owned by the player */
  virtual PtrList<NamedSolution> *solutionset(string md5) = 0;

  /* frees anything that it overwrites, including the solutions. So
     calling setsolutionset on the set returned from solutionset
     without cloning the solutions first will prematurely free them */
  virtual void setsolutionset(string md5, PtrList<NamedSolution> *) = 0;

  /* simply add a new solution to the set. copies ns, so it remains
     owned by the caller. If def_candidate is true, this might be made
     the default solution if it is not a bookmark, and it is better
     than the current default. */
  virtual void addsolution(string md5, NamedSolution *ns, 
                           bool def_candidate = false) = 0;

  /* is this solution already in the solution set? */
  virtual bool hassolution(string md5, Solution *what) = 0;

  virtual ~Player() {};
};

#endif
