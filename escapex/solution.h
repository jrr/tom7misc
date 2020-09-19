#ifndef __SOLUTION_H
#define __SOLUTION_H

#include <string>
#include "level-base.h"
#include <assert.h>
#include <vector>

/* a solution is just a list of moves */
struct Solution {
  /* true if verified in this run of the program.
     XXX this is a little awkward, since we don't
     have the level that this is purportedly a
     solution for handy. perhaps this should be in
     the playerdb instead. */
  // It would at least be better in NamedSolution...
  bool verified = false;

  dir At(int n) const { return dirs[n]; }
  int Length() const { return dirs.size(); }
  void Truncate(int n) {
    assert(n <= Length());
    assert(n >= 0);
    dirs.resize(n);
    verified = false;
  }

  std::string ToString() const;

  static bool FromString(const std::string &s, Solution *sol);

  Solution() {}
  ~Solution() {}

  static Solution Empty() { return Solution{}; }

  bool IsEmpty() const { return dirs.empty(); }

  void Clear() {
    dirs.clear();
    verified = false;
  }

  void Append(dir d) {
    dirs.push_back(d);
    verified = false;
  }

  // Same moves; ignores the "verified" field.
  static bool Equal(const Solution &l, const Solution &r);

  // XXX should be thin wrapper on vector::begin()
  // so that we can use ranged-for too.
  struct iter {
    int pos;
    const Solution *sol;

    iter(const Solution &s) : pos(0), sol(&s) {}
    bool hasnext() { return pos < (int)sol->dirs.size(); }
    void next() { pos++; }
    dir item() { return sol->dirs[pos]; }
  };

  void Appends(const Solution &s) {
    // XXX probably buggy if *this == s.
    for (iter i{s}; i.hasnext(); i.next()) {
      Append(i.item());
    }
  }

private:
  std::vector<dir> dirs;
};

#endif
