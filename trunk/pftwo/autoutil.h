
#ifndef __AUTOUTIL_H
#define __AUTOUTIL_H

#include <unordered_map>
#include <vector>
#include "../cc-lib/gtl/top_n.h"

// Utilities for AutoLives, AutoCamera, etc.
// T must have a default constructor that zeroes the score field.
template<typename T, typename GetScore, typename GetLoc>
std::vector<T> MergeAndBest(const std::vector<std::vector<T>> &vv,
			    GetLoc getloc,
			    GetScore getscore,
			    float min_score,
			    int n_best);

// Template implementations follow.

namespace internal {
template<typename T, typename GetScore>
struct BetterScore {
  BetterScore(const GetScore &getscore) : getscore(getscore) {}
  bool operator() (const T *a, const T *b) const {
    return getscore(*a) > getscore(*b);
  }
  const GetScore &getscore;
};
}

template<typename T, typename ScoreField, typename LocField>
std::vector<T> MergeAndBest(const std::vector<std::vector<T>> &vv,
			    LocField loc_field,
			    ScoreField score_field,
			    float min_score,
			    int n_best) {
  std::unordered_map<int, T> merged;
  for (const auto &v : vv) {
    for (const T &l : v) {
      const int loc = loc_field(l);
      T *t = &merged[loc];
      loc_field(*t) = loc;
      score_field(*t) += score_field(l);
    }
  }

  using BS = typename internal::BetterScore<T, ScoreField>;
  BS better_score{score_field};
  gtl::TopN<const T *, BS> topn(n_best, better_score);
  for (const auto &p : merged) {
    if (score_field(p) >= min_score) {
      topn.push(&p.second);
    }
  }

  std::unique_ptr<std::vector<const T *>> best(topn.Extract());
  std::vector<T> res;
  res.reserve(best.size());
  for (const T *t : *best) res.push_back(*t);
  return res;
}
  


#endif
