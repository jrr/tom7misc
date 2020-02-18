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

#if 0
template<typename T>
struct Stream {
  constexpr bool HasNext() const;
  constexpr std::pair<T, Stream<T>> Next() const;
};

// Here's a really simple demonstration that you can enumerate
// integers with constexpr.
template<>
struct Stream<int> {
  constexpr Stream(int z) : z(z) {}
  constexpr bool HasNext() const { return true; }
  constexpr std::pair<int, Stream<int>> Next() const {
    return make_pair(z, Stream<int>(z + 1));
  }
 private:
  int z = 0;
};

// Here's an example of constructing a 
template<typename T, bool (*F)(T)>
struct Filter {
  constexpr Filter(Stream<T> s) : s(s) {}

  // Note: We can write HasNext using the same approach below,
  // but this requires us to repeat the loop (in the idiom where
  // you repeatedly call HasNext() and Next(), we do twice as
  // much work as necessary). So probably better to structure
  // the streams around std::optional or something.
  
  constexpr std::pair<T, Filter> Next() const {
    // We can't assign to s, but we can make local copies.
    auto ss = s;
    for (;;) {
      auto p = ss.Next();
      if (F(p.first)) return make_pair(p.first, Filter(p.second));
      else ss = p.second;
    }
  }
  private:
  Stream<T> s;
};


/*
struct IntStream {
  constexpr int Next() {
    return x++;
  }
  constexpr int x = 0;
};
*/
  
/*
template<typename T, typename Env>
constexpr T Get() {
  return T();
}
*/
#endif

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
// that just record their arguments, for example (at least if we allow
// 
