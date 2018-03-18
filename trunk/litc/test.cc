
#include <stdlib.h>
#include <stdio.h>
#include <string>

using namespace std;

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

template<char... Cs>
struct Stringifier {
  static string Ret() {
    string s;
    for (char c : std::initializer_list<char>{Cs...})
      s += c;
    return s;
  }
};

template<char... Cs>
constexpr string Stringify() {
  string s;
  for (char c : {Cs...})
    s += c;
  return s;
}

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


// This doesn't work, despite some documentation suggesting
// that this is a permitted "overload" for user-defined literals?
//
// I guess this is possible for integer literals but not string. :(
#if 0
template<char... Cs>
int operator ""_t() {
  return 0; // Get<Cs...>::Ret();
}
#endif

// Converts Hex into bytes and invokes T.
template<template<char...> class Op, char... Hex>
struct MakeBytes;

template<template<char...> class Op>
struct MakeBytes<Op> {
  static string Ret() { return Op<>::Ret(); }
};

/*
// The static assert just fails when this is encountered -- how to
// make that only happen if the template actually gets instantiated?
template<template<char...> class Op, char C>
struct MakeBytes<Op, C> {
  static_assert(false, "MakeBytes got an odd number of hex nybbles");
};
*/

template<char Ch, char Cl>
struct Unhex {
  static constexpr char Value =
    (((int)Ch | 4400) % 55) * 16 +
    (((int)Cl | 4400) % 55);
};

#if 0
// Ignore apostrophe in literals.
// (How to make this preferred over the next?)
template<template<char...> class Op, char... Hex>
struct MakeBytes<Op, '\'', Hex...> {
  static string Ret() {
    return MakeBytes<Op, Hex...>::Ret();
  }
};
#endif

template<template<char...> class Op, char Ch, char Cl, char... Hex>
struct MakeBytes<Op, Ch, Cl, Hex...> {
  using C = Unhex<Ch, Cl>;

  template<char... Cs>
  struct PushOp {
    static string Ret() {
      return Op<C::Value, Cs...>::Ret();
    };
  };

  static string Ret() {
    return MakeBytes<PushOp, Hex...>::Ret();
  }
};

template<char... Cs>
string operator ""_ss() {
  return Stringify<Cs...>();
  /*
  string s;
  for (char c : {Cs...})
    s += c;
  return s;
  */
}

template<char... Cs>
struct Parse;

// TODO: Should allow '0X' as well.
template<char... Cs>
struct Parse<'0', 'x', Cs...> {
  static string Ret() {
    return MakeBytes<Stringifier, Cs...>::Ret();
  }
};

template<char... Cs>
string operator ""_sss() {
  return Parse<Cs...>::Ret();
}

int main(int argc, char **argv) {

  // int x = Get<'4', '3', '2', '1'>::Ret();
  // This demonstrates that the result of the user-defined
  // literal is constexpr.
  int x = Func<"1234"_a>();
  int y = 0;
  // auto s = 0x696e'7420'6628'696e'7420'7929'7b0a'2020'7265'7475'726e'2079'202b'2031'3b0a'7d0a_sss;
  // auto s = 0x696e74_sss;
  auto s = 0x696e74206628696e742079297b0a202072657475726e2079202b20313b0a7d0a_sss;
  printf("What you got was: %d, %d, %s\n", x, y, s.c_str());
}
