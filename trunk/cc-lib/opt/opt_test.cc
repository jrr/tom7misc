#include <stdio.h>
#include <string.h>
#include <cstdint>

#include <math.h>

#include "opt/opt.h"
#include "base/logging.h"

// sin(x^3 - 3x + 3) + x^2
// The parabola takes over quickly, with a single global
// minimum around -0.467979 (Wolfram Alpha), but lots of
// local minima.
static double Test1(const std::array<double, 1> &args) {
  double x = args[0];
  return sin(x * x * x - 3.0 * x + 3) + x * x;
}

static double Test1v(const std::vector<double> &args) {
  CHECK(args.size() == 1);
  double x = args[0];
  return sin(x * x * x - 3.0 * x + 3) + x * x;
}

int main(int argc, char **argv) {

  const auto [args, v] =
    Opt::Minimize<1>(Test1, {-1000.0}, {1000.0}, 1000);

  printf("Found minimum at f(%.5f) = %.5f\n"
	 "(Expected f(%.5f) = %.5f)\n",
	 args[0],
	 v,
	 -0.467979,
	 Test1({-0.467979}));

  const auto [vargs, vv] =
    Opt::Minimize(1, Test1v, {-1000.0}, {1000.0}, 1000);

  CHECK(vargs.size() == 1);
  printf("Vector version: f(%.5f) = %.5f\n",
	 vargs[0], vv);


  // TODO: Check some 2+ dimensional inputs too!
  
  return 0;
}

