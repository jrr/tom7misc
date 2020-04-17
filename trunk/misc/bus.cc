#include <algorithm>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <set>

#include "base/logging.h"
#include "util.h"

using namespace std;

vector<int> Factorize(int w) {
  for (int i = 2; i < w; i++) {
    // PERF: Skip easy non-primes
    if (w % i == 0) {
      int ww = w / i;
      vector<int> rest = Factorize(ww);
      rest.push_back(i);
      return rest;
    }
  }
  return {w};
}

// Call f on all factorizations (not using 1) of the
// number n, which is given as its prime factorization.
template<class F>
void NonUnityProducts(const vector<int> &factors, F f) {
  // First, the full list...
  f(factors);
  // Now, we can multiply together any two factors.
  // Choose a first one and the second after it.
  for (int i = 0; i < factors.size(); i++) {
    for (int j = i + 1; j < factors.size(); j++) {
      // PERF could make fewer copies of the factors..
      vector<int> fs;
      fs.reserve(factors.size() - 1);
      for (int x = 0; x < factors.size(); x++) {
	if (x == i) fs.push_back(factors[i] * factors[j]);
	else if (x != j) fs.push_back(factors[x]);
      }
      NonUnityProducts(fs, f);
    }
  }
}

template<class F>
void SubsetsProductingTo(int target,
			 int nfactors,
			 int *factors,
			 vector<int> &children, F f) {
  if (target == 1) f(children);
  if (nfactors == 0) {
    return;
  }
  // consider using the factor.
  int factor = *factors;
  if (factor < target) {
    children.push_back(factor);
    /*
    printf("Try factor %d, target now %d, %d factors left..\n", factor, target - factor,
	   nfactors - 1);
    */
    SubsetsSummingTo(target - factor, nfactors - 1, &factors[1], children, f);
    children.pop_back();
  }
  // consider skipping.
  SubsetsSummingTo(target, nfactors - 1, &factors[1], children, f);
}

int main(int argc, char **argv) {
  for (int bus = 0; bus < 100; bus++) {
    printf("bus %d:\n", bus);
    for (int wiz = 1; wiz < 100; wiz++) {
      // sum of kids ages is bus, so bounded by that
      vector<int> factors = Factorize(wiz);
      
      /*
      printf("Wiz %d Factors: ", wiz);
      for (int f : factors) printf("%d ", f);
      printf("\n");
      */

      unordered_map<int, std::set<vector<int>>> sols;
      auto Consider = [bus, wiz, &sols](vector<int> children) {
	  std::sort(children.begin(), children.end());
	  /*
	  int s = 0;
	  printf("bus %d, wiz %d. %d children aged: ", bus, wiz,
		 (int)children.size());
	  for (int c : children) {
	    printf("%d ", c);
	    s += c;
	  }
	  printf("\n");
	  CHECK(s == bus) << s << " " << bus << " " << wiz;
	  */

	  int num_children = children.size();
	  // XXX need to dedup; there can be multiple ways of arriving
	  // at the same set of ages.
	  sols[num_children].insert(children);
	};
      
      vector<int> ctmp;
      // printf("** try to sum to %d with %d factors **\n", bus,
      // (int)factors.size());

      NonUnityProducts(factors,
		       [bus, &Consider](const vector<int> &children) {
	int sum = 0;
	for (int c : children) sum += c;
	if (sum > bus) return;

	// Pad with 1s if not...
	vector<int> cs = children;
	int rest = bus - sum;
	while(rest--) cs.push_back(1);
	Consider(std::move(cs));
      });
    
      int singletons = 0;
      for (const auto &[nc, vs] : sols) {
	if (vs.size() > 1) {
	  printf("Multiple solutions for bus=%d, wiz=%d:\n", bus, wiz);

	  for (const vector<int> &v : vs) {
	    printf("  [len %d] ", (int)v.size());
	    for (int c : v) printf("%d ", c);
	    printf("\n");
	  }
	} else {
	  singletons++;
	}
      }
      // printf("Singletons: %d\n", singletons);
    }
  }
  return 0;
}
