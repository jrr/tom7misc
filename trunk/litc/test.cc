
#include <stdlib.h>
#include <stdio.h>

// Here's how you do 
template<char... Cs>
struct Get;

template<>
struct Get<> {
  static int Ret() {
    return 0;
  }
};

template<char C, char... Cs>
struct Get<C, Cs...> {
  static int Ret() {
    return 10 * Get<Cs...>::Ret() + (C - '0');
  }
};

#if 0
// This doesn't work, despite some documentation suggesting
// that this is a permitted "overload" for user-defined literals?
template<char... Cs>
int operator "" _a() {
  return Get<Cs...>::Ret();
}
#endif

// Since C++14 the body of constexpr is quite unconstrained!
constexpr int Self(const char *s, size_t len) {
  // This is like the C++11 way.
  /*
  return (len == 0) ? 0 :
    (10 * Self(s + 1, len - 1) + (s[0] - '0'));
  */

  // And this is a nice recursive way.
  /*
  if (len == 0) {
    return 0;
  } else {
    return 10 * Self(s + 1, len - 1) + (s[0] - '0');
  }
  */

  // But now you can just like write C++ code.
  int ret = 0;
  for (int i = len - 1; i >= 0; i--) {
    ret *= 10;
    ret += (s[i] - '0');
  }
  return ret;
}

constexpr int operator "" _a(const char *s, size_t len) {
  return Self(s, len);
}

template<int X>
int Func() {
  int z = X;
  return z * 10;
}

int main(int argc, char **argv) {

  // int x = "1234"_a;
  // int x = Get<'4', '3', '2', '1'>::Ret();
  // This demonstrates that the result of the user-defined
  // literal is constexpr.
  int x = Func<"1234"_a>();
  
  printf("What you got was: %d\n", x);
}
