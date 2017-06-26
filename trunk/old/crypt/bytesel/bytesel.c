/*
   C Implementation of Tom7-skipstate generator (using LFSR).
   This code is distributed under the GNU public license;
   see http://www.gnu.org/copyleft/gpl.html.

   I can't make any claims about the security of this algorithm because
   I am not a trained cryptographer, though it produces (as far as I can
   tell) statistically random output. This code should be thought about,
   but not used unless you know more about this than me.

     Tom 7

   http://www.andrew.cmu.edu/~twm/

*/

#include <stdio.h>
#include <stdlib.h>

#define INPUT_SKIPSTATE 1

#define s(a,b,c) m=a[(b)];a[(b)]=a[(c)];a[(c)]=m;
// swap bytes b and c in array a

typedef struct {
     unsigned long data;
     unsigned long mask;
     unsigned char len;
     unsigned char inputs;
} LFSR;

unsigned char           generator(LFSR * g);
unsigned char skipstate_generator(LFSR * g);

LFSR G[8] = {
     {0,(1<<31)|(1<<6)|(1<<4)|(1<<2)|(1<<1)|1,31,0},
     {0,(1<<29)|(1<<5)|(1<<3)|1,29,0},
     {0,(1<<30)|(1<<12),30,0},
     {0,(1<<18)|(1<<4)|(1<<1)|1,18,0},
     {0,(1<<16)|(1<<4),16,0},
     {0,(1<<12)|(1<<3)|(1<<2)|1,12,0},
     {0,(1<<31)|(1<<6)|(1<<2)|(1<<1)|1,31,0},
     {0,(1<<22)|(1<<4),22,0}
};

/*
(no keys here; just some arbitrary initialization vectors for the LFSRs.
 This is just for demonstration!)

 G[0] is our skipstate generator; G[1] drives it.
*/

int main (int argc, char**argv) {
     int x,z,c,a,m,s;
     unsigned char output=0,counter=0;
     unsigned char P[256],K[256],j;
     char * key = argv[1];

     if (argc < 2) { printf("%s key\n", argv[0]); exit(-1); }

     s = strlen(key);
     for (x=c=0;x<8;x++) {
          G[x].data |= key[c++%s] << 24;
          G[x].data |= key[c++%s] << 16;
          G[x].data |= key[c++%s] << 8 ;
          G[x].data |= key[c++%s]      ;
          a = key[c++%s]; while (a--) generator(&G[x]);
     }

     for (x=0;x<256;x++) K[P[x]=x]=key[x%s];
     for (x=j=0;x<256;x++) { j+=P[x]+K[x]; s(P,x,j); }

/* Filled LFSR's, P-Box with key-dependent material */

     if (argc>2) z = atoi(argv[2]); else z=1000;

     while (z--) {
          /* set up generator input */
          for (x=0;x<8;x++) G[x].inputs = (output>>x)&1;
          output = counter++;
          for (x=0;x<8;x++) output ^= skipstate_generator(&G[x])<<x;
          output = P[output];
//          printf("%0X", output);
          for (x=0;x<8;x++) putchar('0' + ((output>>x)&1));
     }
     return 0;
}

unsigned char skipstate_generator(LFSR * g) {
     if (g->inputs & INPUT_SKIPSTATE) generator(g);
     g->inputs &= ~INPUT_SKIPSTATE;
     return generator(g);
}

unsigned char generator(LFSR * g) {
     if (!g->data) g->data--; /* keys of all 0's would ruin this algorithm! */
     if (g->data & 1) {
          g->data = ((g->data ^ g->mask) >> 1)
               | (1<<g->len); /* g->len is already len-1 */
          return 1;
     } else {
          g->data>>=1; return 0;
     }
}
