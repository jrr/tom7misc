
#include <cstdio>
#include <cstdint>

using int64 = int64_t;

// constexpr int64 START = 1583000000;
// constexpr int64 END = 1592058229;
// constexpr int64 STRIDE = 500000;

constexpr int64 START = 1591990000;
constexpr int64 END = 1592000000;
constexpr int64 STRIDE = 1000;

#if 1
// Unrelated C++ puzzle/gcc bug
struct A {
  int a : 3;
  int b : 5;
};

int Test() {
  // ...
  A x;
  auto &[a, b] = x;

  // I think this is supposed to be lillegal... bitfield reference?
  // (Const references are ok.)
  // We do get an error if &a is specified explicitly.
  return [&](){
    return 0;
  }();
}
#endif

int main(int argc, char **argv) {

  for (int64 lo = START; lo < END; lo += STRIDE) {
    printf("update tempo.reading set sample_key = "
	   "((id * 31337) ^ (probeid * 82129) + (timestamp * 257)) "
	   "mod 65521 where timestamp between %lld and %lld;\n",
	   lo, lo + STRIDE);
  }
    
  return 0;
}
