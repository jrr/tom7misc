
#include <string>
#include <vector>
#include <cstdint>
#include <deque>

#include "../../cc-lib/base/logging.h"

#include "../../cc-lib/bignum/big.h"

using namespace std;

// Only thing we need for chess playing is this function.
// It takes n (number of moves) and figures out which 1/n region
// the rational r falls within. It then scales that region such
// that it now nominally spans [0,1] and returns the new rational
// r' within that.
static BigRat Forward(const BigRat &r, int n) {
  // We have r = m/d.
  // First find k such that k/n <= r < (k+1)/n.

  for (int k = 0; k < n; k++) {
    // Invariant: r is known to be >= k/n.
    CHECK(1 != BigRat::Compare(BigRat(k, n), r));
    // Next bound to consider is (k + 1)/n.
    BigRat ubound(k + 1, n);
    int cmp = BigRat::Compare(r, ubound);
    if (cmp == BQ_LT) {
      // The first time this happens, we know r is in the
      // interval k/n to k+1/n.
      // So r' = (r - k/n)
      //         ---------
      //         (1 / n)
      // which is the same as n * (r - k/n)
      // which is nr - k.

      BigRat nr = BigRat::Times(BigRat(n, 1), r);
      BigRat rr = BigRat::Minus(nr, BigRat(k, 1));
      return rr;
    }
  }

  // Should be impossible if r is in [0, 1).
  return BigRat{};
}


static void TestBigRat() {
  BigRat two_fifths(2, 5);
  BigRat next = Forward(two_fifths, 3);
  printf("Result: %s\n", next.ToString().c_str());
}

int main(int argc, char **argv) {
  TestBigRat();
  return 0;
}

