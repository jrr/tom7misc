/*
   C Implementation of a Tom7-bridge device using skipstate LFSRs.
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
unsigned char          sgenerator(LFSR * g);

LFSR G[5] ={
     {0xFEEDBABE,(1<<30)|(1<<12),30,0},
     {0xDEAFDAD ,(1<<18)|(1<<4)|(1<<1)|1,18,0},
     {0xFEEDF00D,(1<<16)|(1<<4),16,0},
     {0xBADCAB  ,(1<<12)|(1<<3)|(1<<2)|1,12,0},
     {0xDEADBEEF,(1<<31)|(1<<6)|(1<<4)|(1<<2)|(1<<1)|1,31,0},
};

/*
(no keys here; just some arbitrary initialization vectors for the LFSRs.
 This is just for demonstration!)
*/

main (int argc, char**argv) {
     int z,last=0;

     if (argc>=2) z = atoi(argv[1]); else z=1000;

   while (z--) {
        G[0].inputs = G[0].inputs = last; /* top, left*/
        G[2].inputs = G[3].inputs = sgenerator(&G[0]); /* middle, bottom */
        G[4].inputs = sgenerator(&G[1]) ^ sgenerator(&G[2]); /* right */
        last = sgenerator(&G[3]) ^ sgenerator(&G[4]);
        putchar('0'+last);
   }

}

unsigned char sgenerator(LFSR * g) {
     if (g->inputs & INPUT_SKIPSTATE) generator(g);
     g->inputs &= ~INPUT_SKIPSTATE;
     return generator(g);
}

unsigned char generator(LFSR * g) {
     if (!g->data) g->data++;
     if (g->data & 1) {
          g->data = ((g->data ^ g->mask) >> 1)
               | (1<<g->len); /* g->len is already len-1 */
          return 1;
     } else {
          g->data>>=1; return 0;
     }
}
