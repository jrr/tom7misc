
#include "edit-distance.h"

#include <string>

using namespace std;

#define score(c, d) (((c)==(d))?0:1)
#define gapscore 1

#define int_min(a, b) (((a)<(b))?(a):(b))
#define int_min_3(a, b, c) (((a)<(b))?int_min(a,c):int_min(b,c))
#define ba(x, y) (a[(y) * n1 + (x)])

#define aft(i) a[(parity & (n1 + 1)) + (i)]
#define fore(i) a[((~parity) & (n1 + 1)) + (i)]

#define swaplines() parity = ~parity

int EditDistance::Distance(const string &s1, const string &s2) {
  const int n1 = s1.size();
  const int n2 = s2.size();
  
  int *a = (int *) malloc((n1 + 1) * 2 * sizeof(int));

  /* parity will always be either 0 or 0xFFFFFFFF */
  int parity = 0;

  // Initialize base cases.
  for (int i = 0; i <= n1; i++)
    aft(i) = gapscore * i;

  // Now, compute each row from the previous row.
  for (int y = 1; y <= n2; y ++) {
    // We know what the first column of every row is.
    int last = gapscore * y;
    fore(0) = last;

    for (int x = 1; x <= n1; x ++) {
      int xi = x - 1;
      int yi = y - 1;

      int diag = aft(x - 1) + score(s1[xi], s2[yi]);

      int up   = aft(x) + gapscore;
      int left = last + gapscore;

      last = int_min_3(up, left, diag);

      fore(x) = last;
    }

    // Move fore to aft in preparation for next line.
    swaplines();
  }

  // At this point we swapped our final line into aft, so we want
  // the last cell of aft.

  int ans = aft(n1);
  free(a);
  return ans;
}
