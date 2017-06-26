#include <stdlib.h>
/* C array-oriented version of GPSA */

#define score(c, d) subst[(c) * radix + (d)]

#define int_max(a, b) ((a>b)?(a):(b))
#define int_max_3(a, b, c) ((a>b)?int_max(a,c):int_max(b,c))
#define ba(x, y) (a[(y) * n1 + (x)])

#define aft(i) a[(parity & (n1 + 1)) + (i)]
#define fore(i) a[((~parity) & (n1 + 1)) + (i)]

#define swaplines() parity = ~parity;

int ml_bestalignment (int radix, int * subst,
		      int gapscore,
		      int n1, char * s1,
		      int n2, char * s2) {

  int * a = (int *) malloc((n1 + 1) * 2 * sizeof(int));

  /* parity will always be either 0 or 0xFFFFFFFF */
  int parity = 0;

  /* initialize base cases */
  { int i;
  for(i = 0; i <= n1; i++) aft(i) = gapscore * i; }

  {
    /* now, compute each row from the previous row */
    int y;
    for(y = 1; y <= n2; y ++) {
      int x;
      /* we know what the first column of every
	 row is. */
      int last = gapscore * y;
      fore(0) = last;

      for(x = 1; x <= n1; x ++) {

	__asm__ (" /* inner loop */ ");

	int xi = x - 1;
	int yi = y - 1;

	int diag = aft(x - 1) + score (s1[xi], s2[yi]);
      
	int up   = aft(x) + gapscore;
	int left = last + gapscore;

	last = int_max_3(up, left, diag);
      
	fore(x) = last;

	__asm__ (" /* end */ ");
      }

      /* now, move fore to aft in preparation
	 for next line */
      swaplines();
    }
  }

  /* at this point we swapped our final line
     into aft, so we want the last cell of aft */

  { int ans = aft(n1);

  free(a);

  return ans;
  }
}
