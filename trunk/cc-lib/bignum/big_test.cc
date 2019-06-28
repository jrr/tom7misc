
#include "big.h"
#include <cstdint>

#include "../base/logging.h"

using int64 = int64_t;

int main(int argc, char **argv) {
  printf("Start.\n");
  fflush(stdout);
  {
    BigInt i{1234567LL};
    BigInt j{33LL};
    BigInt k = BigInt::Times(i, j);
    BigInt m("102030405060708090987654321");
    
    printf("Integer: %s %s %s\n%s\n",
	   i.ToString().c_str(),
	   j.ToString().c_str(),
	   k.ToString().c_str(),
	   m.ToString().c_str());
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
