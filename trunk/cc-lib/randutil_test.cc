#include "randutil.h"

#include <unordered_map>

#include "arcfour.h"
#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;

// Regression: Shuffling an empty vector used to hang because
// it used unsigned integers.
static void TestShuffleCornerCases() {
  ArcFour rc("corner");
  vector<int> v;
  Shuffle(&rc, &v);
  CHECK(v.empty());

  v.push_back(7);
  Shuffle(&rc, &v);
  CHECK(v.size() == 1);
  CHECK(v[0] == 7);
}

// Test that Shuffle produces each permutation with even
// probability. It's pretty easy to get this wrong!
static void TestShuffle(int n, double absolute_error) {
  ArcFour rc(StringPrintf("test_%d", n));

  unordered_map<string, int64> counts;
  static constexpr int ITERS = 10000000;
  for (int iter = ITERS; --iter;) {
    vector<char> vec;
    vec.reserve(n);
    for (int i = 0; i < n; i++) vec.push_back('a' + i);
    Shuffle(&rc, &vec);
    string s;
    s.reserve(n);
    for (char c : vec) s += c;
    counts[s]++;
  }
  auto factorial = [](int j) {
    int64 ret = 1;
    for (int i = 2; i <= j; i++) ret *= i;
    return ret;
  };

  double correct_prob = 1.0 / factorial(n);
  for (const auto &p : counts) {
    double observed_prob = p.second / (double)ITERS;
    printf("%s: %lld (p = %.8f)\n", p.first.c_str(),
	   p.second, p.second / (double)ITERS);
    if (fabs(observed_prob - correct_prob) > 
	absolute_error) {
      printf(" (but wanted %.8f.) "
	     "... this is outside the allowed error "
	     "of %.8f.\n", correct_prob, absolute_error);
      exit(-1);
    }
  }
}

int main(int argc, char **argv) {
  TestShuffleCornerCases();
  TestShuffle(2, 0.0005);
  TestShuffle(3, 0.0005);
  TestShuffle(4, 0.0005);
}
