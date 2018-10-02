// Some native utilities on the std::vector type.
// Everything in here is single-threaded to improve portability; for
// multithreaded stuff, see threadutil.h.

#ifndef __VECTOR_UTIL_H
#define __VECTOR_UTIL_H

#include <vector>

template<class A, class F>
static auto Map(const std::vector<A> &vec, const F &f) ->
  std::vector<decltype(f(vec[0]))> {
  using B = decltype(f(vec[0]));
  std::vector<B> ret;
  ret.resize(vec.size());
  for (int i = 0; i < vec.size(); i++) {
    ret[i] = f(vec[i]);
  }
  return ret;
}

template<class A, class F>
static void App(const std::vector<A> &vec, const F &f) {
  for (const auto &elt : vec) f(elt);
}

#endif
