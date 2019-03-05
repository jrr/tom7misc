/* New version of Learnfun in 2018.

   The chief difference between this and the classic version
   (in learnfun.*) is that we try to find lexicographic
   orderings on memories that don't just go up, but that go
   up MORE than when the player does nothing.

   TODO implement me!

*/
#ifndef __LEARNFUN2_H
#define __LEARNFUN2_H

#include <vector>

#include "pftwo.h"

struct WeightedObjectives;
struct ObjectiveEnumerator;
struct Learnfun2 {
  explicit Learnfun2(const string &game,
		     const vector<pair<uint8, uint8>> &movie);
  
  // Caller owns new-ly allocated pointer.
  WeightedObjectives *MakeWeighted();
  
 private:
  RandomPool random_pool;
  EmulatorPool emu_pool;
};

#endif
