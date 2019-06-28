
#include "big.h"
#include <cstdint>

#include "../base/logging.h"

using int64 = int64_t;

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
  printf("Start.\n");
  fflush(stdout);
  {
    BigInt i{1234567LL};
    BigInt j{33LL};
    BigInt k = BigInt::Times(i, j);
  
    printf("Integer: %s %s %s\n",
	   i.ToString().c_str(),
	   j.ToString().c_str(),
	   k.ToString().c_str());
    fflush(stdout);
  }

  BigRat sum;
  for (int i = 0; i < 10000; i++) {
    // + 1/1, - 1/3, + 1/5
    BigRat term{(i & 1) ? -1 : 1,
	i * 2 + 1};
    sum = BigRat::Plus(sum, term);
    if (i < 50) {
      BigRat tpi = BigRat::Times(sum, BigRat{4,1});
      printf("Approx pi: %s = %f\n",
	     tpi.ToString().c_str(),
	     tpi.ToDouble());
      fflush(stdout);
    } else if (i % 1000 == 0) {
      printf("%d...\n", i);
      fflush(stdout);
    }
    
  }

  BigRat res = BigRat::Times(sum, BigRat(4, 1));
  printf("Final approx pi: %s\n",
	 res.ToString().c_str());
  fflush(stdout);


  // This sequence converges REALLY slow!
  BigRat pi_lb(314, 100);
  BigRat pi_ub(315, 100);

  CHECK(BigRat::Compare(pi_lb, pi_ub) == -1);
  CHECK(BigRat::Compare(pi_lb, res) == -1);
  CHECK(BigRat::Compare(res, pi_ub) == -1);
}
