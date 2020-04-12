/*
  - Some "stream" of values of a given type
  - Either we need to include variables from the context,
    or we bake this into generation of function values..
  - I think the environment approach makes more sense.
    
  - We also want to satisfy some properties ("tests"), right?
  - We're not "just" doing theorem proving here. We want to
    be able to generate semi-ridiculous programs that pass
    all the tests. So really it should be a more loosely coupled
    loop: Generate a program, see if it passes tests, repeat.
  - So, separate this into two concerns.

*/

#include <utility>
#include <functional>
#include <optional>

using namespace std;

// Using optional. This is better because than HasNext/Next because we
// can statically connect the two. It also avoids cases where we have
// to duplicate work in the two functions.

template<typename T>
struct Stream {
  constexpr optional<pair<T, Stream<T>>> Next() const;
};

template<>
struct Stream<int> {
  constexpr Stream(int z) : z(z) {}
  constexpr optional<pair<int, Stream<int>>> Next() const {
    return optional(make_pair(z, Stream<int>(z + 1)));
  }
 private:
  int z = 0;
};

template<typename T, bool (*F)(T)>
struct Filter {
  constexpr Filter(Stream<T> s) : s(s) {}

  constexpr optional<pair<T, Filter>> Next() const {
    // We can't assign to s, but we can make local copies.
    auto ss = s;
    for (;;) {
      auto po = ss.Next();
      if (!po) return {};
      auto p = *po;
      if (F(p.first)) return optional(make_pair(p.first, Filter(p.second)));
      else ss = p.second;
    }
  }
  private:
  Stream<T> s;
};

// TODO: Stream of function/callables?
// The art here is that we want to generate functions that have some chance
// of passing the tests! So for a function int -> int, we don't e.g. want to
// first try all the different functions that return a specific constant integer.
// Best would be if the "tests" were somehow analyzable. If a test was given
// not as ([](std::function<int(int)>f){ return f(10) == 20 && f(2) == 4; })
// but something data structure like And(Eq(App(Fn, Int(10)), Int(20)),
// Eq(App(Fn, Int(10)), Int(20))), then we could do a more directed job of
// producing the function. But this could be significantly limiting, and runs
// the risk of missing the "joke" anyway.
//
// Note that even without changing the interface, it would be possible to
// extract properties from the test as sort of "hints." We can pass functions
// that just record their arguments, for example (at least if we allow some
// kind of effects?)

// Clearly some kinds of effects are allowed...
constexpr int Test(int x) {
  int res = 0;
  for (int i = 0; i < x; i++) {
    res += i;
  }
  return res;
}

static_assert(Test(3) == 0 + 1 + 2);

// And calling functions...
template<class F>
constexpr int TestC(F f) {
  return f(0);
}

static_assert(TestC([](int _){ return 1; }) == 1);

template<class F>
constexpr int Record(F f) {
  int x = 0;
  auto Leak = [&x](int arg) { x = arg; return 0; };
  f(Leak);
  return x;
}

template<template<typename> class F, class A>
struct Recorder {
  static int Savey(F<A> f) {
    int z = 0;
    f([&z](int arg) { z = arg; return 2; });
    return z;
  }
  /*
  constexpr Recorder(F f) { f([this](int arg) { this->x = arg; return 1; }); }
  constexpr int Leak(int arg) {
    x = arg;
    return 0;
  }
  int x;
  */
};

// "template deduction guides"?
// template<class F>
// Recorder(F f) -> Recorder(F

constexpr bool Pointers() {
  int x = 1;
  int *y = nullptr;
  int *n = &x;
  *n = 3;
  // delete n;
  return *y == *y;
}

static_assert(Pointers());

#if 0
template<class F>
constexpr bool IsFactLike(F f) {
  return f(3) == 3 * 2 * 1;
}
#endif
template<class F>
struct IsFactLike {
  constexpr bool operator()(F f) {
    return f(3) == 3 * 2 * 1;
  }
};

// static_assert(IsFactLike([](int x) { return 6; }));
#if 0
template<class A>
constexpr int Zzz() {
  // Recorder r(IsFactLike);
  Recorder<IsFactLike, A>::Savey(IsFactLike());
  return 0;
}
#endif

#if 0
constexpr int Hmm() {
  return Record(IsFactLike);
}
static_assert(Hmm() == 3);
#endif
  


constexpr bool Cool() {
  int *p = nullptr;
  return *p == *p;
}
static_assert(Cool());
