#include <stdlib.h>
/* C array-oriented version of GPSA */


#define score(c, d) (((c)==(d))?4:-2)
#define gapscore (-4)

#define int_max(a, b) ((a>b)?(a):(b))
#define int_max_3(a, b, c) ((a>b)?int_max(a,c):int_max(b,c))
#define ba(x, y) (a[(y) * n1 + (x)])


int ml_bestalignment (int n1, char * s1,
		      int n2, char * s2) {

# if 0
  printf("%d x %d\n", n1, n2);
  write(1, s1, n1);
  printf("\n");
  write(1, s2, n2);
  printf("\n");
# endif

  int * a = (int *) malloc((n1 + 1) * (n2 + 1) * sizeof(int));

  /* initialize base cases */
  { int i;
    for(i = 0; i <= n1; i++) a[i] = gapscore * i; }
  { int y;
    for(y = 0; y <= n2; y++) a[y * n1] = gapscore * y; }

  { int x, y;
    for(x = 1; x <= n1; x++)
      for(y = 1; y <= n2; y++) {

      int xi = x - 1;
      int yi = y - 1;

      int diag = ba(x - 1, y - 1)
	+ score (s1[xi], s2[yi]);
      
      int up   = ba(x, y - 1) + gapscore;
      int left = ba(x - 1, y) + gapscore;

      int res = int_max_3(up, left, diag);
      
      a[y * n1 + x] = res;
    } }

  { int ans = ba(n1, n2);

    free(a);

    return ans;
  }
}
