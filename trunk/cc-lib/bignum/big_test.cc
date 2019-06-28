
#include "big.h"
#include <cstdint>

#include "../base/logging.h"

using int64 = int64_t;

static BigQ FromInts(int64 a, int64 b) {
  BigZ n = BzFromInteger(a);
  BigZ d = BzFromInteger(b);
  BigQ r = BqCreate(n, d);
  BzFree(n);
  BzFree(d);
  return r;
}
#if 0
// Only thing we need for chess playing is this function.
// It takes n (number of moves) and figures out which 1/n region
// the rational r falls within. It then scales that region such
// that it now nominally spans [0,1] and returns the new rational
// r' within that.
static BigQ Forward(const BigQ r, int n) {
  // We have r = m/d.
  // First find k such that k/n <= r < (k+1)/n.


  for (int k = 0; k < n; k++) {
    // Invariant: r is known to be >= k/n.
    // Next bound to consider is (k + 1)/n.
    BigQ bound = FromInts(k + 1, n);
    BqCmp cmp = BqCompare(r, bound);
    if (cmp == BQ_LT) {
      // The first time this happens, we know r is in the
      // interval k/n to k+1/n.

      // HERE.
      
    }
    BqDelete(bound);
  }

  // Should be impossible if r is in [0, 1).
}
#endif

int main(int argc, char **argv) {
  BigInt i{1234567LL};

  printf("Integer: %s\n", i.ToString().c_str());
  
  BigQ r = FromInts(12345, 99999);

  char *s = BqToString(r, 0);
  printf("Rational: %s\n", s);
  free(s);

  BqDelete(r);
}
