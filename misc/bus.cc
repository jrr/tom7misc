#include <vector>
#include <string>
#include <unordered_map>

using namespace std;

vector<int> Factorize(int w) {
  for (int i = 2; i < w; i++) {
    if (w % i == 0) {
      int ww = w / i;
      vector<int> rest = Factorize(ww);
      rest.push_back(i);
      return rest;
    }
  }
  return {w};
}

template<class F>
void SubsetsSummingTo(int target,
		      int nfactors,
		      int *factors,
		      vector<int> &children, F f) {
  if (target == 0) f(children);
  if (nfactors == 0) {
    while (target--)
      children.push_back(1);
    f(children);
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
  for (int bus = 0; bus < 1000; bus++) {
    printf("bus %d:\n", bus);
    for (int wiz = 1; wiz < 1000; wiz++) {
      // sum of kids ages is bus, so bounded by that
      vector<int> factors = Factorize(wiz);
      
      /*
      printf("Wiz %d Factors: ", wiz);
      for (int f : factors) printf("%d ", f);
      printf("\n");
      */

      unordered_map<int, vector<vector<int>>> sols;
      
      vector<int> ctmp;
      // printf("** try to sum to %d with %d factors **\n", bus, (int)factors.size());
      SubsetsSummingTo(bus, factors.size(), factors.data(), ctmp,
		       [bus, wiz, &sols](const vector<int> &children) {

 	  int s = 0;
	  printf("bus %d, wiz %d: ", bus, wiz);
	  for (int c : children) {
	    printf("%d ", c);
	    s += c;
	  }
	  printf(" = %d\n", s);

	  const int num_children = children.size();
	  if (num_children == 0) return;
	  sols[num_children].push_back(children);
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
