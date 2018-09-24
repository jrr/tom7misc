
#ifndef __AUTOUTIL_H
#define __AUTOUTIL_H

#include <unordered_map>
#include <vector>
#include "../cc-lib/gtl/top_n.h"

// Utilities for AutoLives, AutoCamera, etc.
// T must have fields called 'loc' (int) and 'score' (float).
// T must have a default constructor that zeroes the score field.
template<typename T, typename GetLoc>
std::vector<T> MergeAndBest(const std::vector<std::vector<T>> &vv,
			    GetLoc getloc,
			    float min_score,
			    int n_best);

// Template implementations follow.

namespace internal {
template<typename T>
struct BetterScore {
  bool operator() (const T *a, const T *b) const {
    return a->score > b->score;
  }
};
}

template<typename T>
std::vector<T> MergeAndBest(const std::vector<std::vector<T>> &vv,
			    float min_score,
			    int n_best) {
  std::unordered_map<int, T> merged;
  for (const auto &v : vv) {
    for (const T &l : v) {
      const int loc = l.loc;
      T *t = &merged[loc];
      t->loc = loc;
      t->score += l.score;
    }
  }

  using BS = typename internal::BetterScore<T>;
  BS better_score;
  gtl::TopN<const T *, BS> topn(n_best, better_score);
  for (const auto &p : merged) {
    if (p.second.score >= min_score) {
      topn.push(&p.second);
    }
  }

  std::unique_ptr<std::vector<const T *>> best(topn.Extract());
  std::vector<T> res;
  res.reserve(best->size());
  for (const T *t : *best) res.push_back(*t);
  return res;
}

#endif
