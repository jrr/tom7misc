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

#define INPUT_SKIPSTATE 1

typedef struct {
     unsigned long data;
     unsigned long mask;
     unsigned char len;
     unsigned char inputs;
} LFSR;

unsigned char           generator(LFSR * g);
unsigned char skipstate_generator(LFSR * g);

LFSR G[2] = {
     {0xDEADBEEF,(1<<31)|(1<<6)|(1<<4)|(1<<2)|(1<<1)|1,31,0},
     {0xFEEDBABE,(1<<29)|(1<<5)|(1<<3)|1,29,0},
};

/*
(no keys here; just some arbitrary initialization vectors for the LFSRs.
 This is just for demonstration!)

 G[0] is our skipstate generator; G[1] drives it.
*/

main (int argc, char**argv) {
     int z;

     if (argc>=2) z = atoi(argv[1]); else z=1000;

     while (z--) {
          G[0].inputs = generator(&G[1]);
          putchar('0' + skipstate_generator(&G[0]));
     }
}

unsigned char skipstate_generator(LFSR * g) {
     if (g->inputs & INPUT_SKIPSTATE) generator(g);
     g->inputs &= ~INPUT_SKIPSTATE;
     return generator(g);
}

unsigned char generator(LFSR * g) {
     if (!g->data) g->data++; /* keys of all 0's would ruin this algorithm! */
     if (g->data & 1) {
          g->data = ((g->data ^ g->mask) >> 1)
               | (1<<g->len); /* g->len is already len-1 */
          return 1;
     } else {
          g->data>>=1; return 0;
     }
}
