
#ifndef __UNION_FIND_H
#define __UNION_FIND_H

#include <vector>

struct UnionFind {
  explicit UnionFind(int size) {
    arr.reserve(size);
    for (int i = 0; i < size; i++) arr.push_back(-1);
  }

  int Find(int a) {
    if (arr[a] == -1) return a;
    else return arr[a] = Find(arr[a]);
  }

  void Union(int a, int b) {
    if (Find(a) != Find(b)) arr[Find(a)] = b;
  }

 private:
  std::vector<int> arr;
};

#endif
