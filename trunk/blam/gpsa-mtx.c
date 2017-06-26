#include <stdlib.h>
/* C and x86 assembly array-oriented version of GPSA */


// #define score(c, d) (((c)==(d))?4:-2)
// #define gapscore (-4)

#define int_max(a, b) ((a>b)?(a):(b))
#define int_max_3(a, b, c) ((a>b)?int_max(a,c):int_max(b,c))
#define ba(x, y) (a[(y) * n1 + (x)])

#define aft(i) a[(parity & (n1 + 1)) + (i)]
#define fore(i) a[((~parity) & (n1 + 1)) + (i)]

#define swaplines() parity = ~parity;

#define score(c, d) subst[(c) * radix + (d)]

int ml_bestalignment_mtx (int radix, int * subst,
			  int gapscore,
			  int n1, char * s1,
			  int n2, char * s2) {

#define gapscore (-8)
#define radix 23

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
      int lastu = aft(0);

      for(x = 1; x <= n1; x ++) {

	  __asm__ (" /* inner loop */ ");
	int xi = x - 1;
	int yi = y - 1;

	int diag = lastu + score (s1[xi], s2[yi]);
      
	lastu = aft(x);

	int up   = lastu + gapscore;

	/* logically, 'left'. */
	last = last + gapscore;

	/* if diag or up is bigger than left,
	   stick that in last instead */
	__asm__ 
	  ("cmpl    %1, %0 \n\t"
	   "cmovl   %1, %0 \n\t"
	   "cmpl    %2, %0 \n\t"
	   "cmovl   %2, %0 \n\t"

	   /* only output into last */
	   : 
	   /* read all of these */
	   : "r" (last), "r" (diag), "r" (up)
	   /* just condition code is clobbered */
	   : "cc"
	   );

	fore(x) = last;

	__asm__ (" /* end inner loop */ ");

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
