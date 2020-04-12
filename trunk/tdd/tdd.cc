#include "tdd.h"

#include <tuple>

constexpr bool Even(int i) { return (i & 1) == 0; }

int main(int argc, char **argv) {
  constexpr Stream<int> is(0);
  constexpr auto a = is.Next();
  static_assert(a);
  static_assert(a->first == 0);
  constexpr auto b = a->second.Next();
  static_assert(b);
  static_assert(b->first == 1);

  // constexpr auto even = [](int i) { return (i & 1) == 0; };
  constexpr Filter<int, Even> filter(is);

  constexpr auto aa = filter.Next();
  static_assert(aa);
  constexpr auto bb = aa->second.Next();
  static_assert(bb);

  static_assert(aa->first == 0);
  static_assert(bb->first == 2);
  
  return 0;
}
