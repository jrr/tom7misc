/* This is a library version of the classic learnfun step from the
   original Learnfun & Playfun paper.

   I haven't changed anything about it, except to make it a library so
   that it can just be run when pftwo starts up (if there is no
   objectives file).

   I know from experiments that hand-writing objective functions seems
   to produce much better results than this algorithm. So there's
   probably significant improvement to be had here.
*/
#ifndef __LEARNFUN_H
#define __LEARNFUN_H

#include <vector>

#include "pftwo.h"

struct WeightedObjectives;
struct ObjectiveEnumerator;
struct Learnfun {
  // Argument must outlast object.
  explicit Learnfun(const vector<vector<uint8>> &memories);
  
  // Caller owns new-ly allocated pointer.
  WeightedObjectives *MakeWeighted();
  
 private:
  const vector<vector<uint8>> &memories;
  vector<vector<int>> objectives;

  void MakeObjectives(const vector<vector<uint8>> &memories);
  void GenerateNthSlices(int divisor, int num,
			 ObjectiveEnumerator *obj);
  void GenerateOccasional(int stride, int offsets, int num,
			  ObjectiveEnumerator *obj);
  
  void PrintAndSave(const vector<int> &ordering);
};

#endif
