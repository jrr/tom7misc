/*
   C Implementation of Tom7-fselect stream cipher.
   This code is distributed under the GNU public license;
   see http://www.gnu.org/copyleft/gpl.html.

   I can't make any claims about the security of this algorithm because
   I am not a trained cryptographer, though it produces (as far as I can
   tell) statistically random output. This code should be thought about,
   but not used unless you know more about this than me.

   The biggest fault I see with this is that the generators are not
   all relatively prime in length -- Applied Cryptography did not have
   enough given for me to pick 7 that were. The period is still something
   pretty huge, at least 2^127 bits before it repeats.

     Tom 7

   http://www.andrew.cmu.edu/~twm/

*/

#include <stdio.h>

typedef struct {
     unsigned long data;
     unsigned long mask;
     unsigned char len;
} LFSR;

int func_thresh(char * bits);
int func_xor(char * bits);
int func_sum(char * bits);
int func_notthresh(char * bits);

unsigned char generator(LFSR * gen);

int (*func[])(char * bits) = {
     func_thresh,
     func_xor,
     func_sum,
     func_notthresh,
};

LFSR G[7] ={
     {0,(1<<31)|(1<<6)|(1<<4)|(1<<2)|(1<<1)|1,31},
     {0,(1<<29)|(1<<5)|(1<<3)|1,29},
     {0,(1<<30)|(1<<12),30},
     {0,(1<<18)|(1<<4)|(1<<1)|1,18},
     {0,(1<<16)|(1<<4),16},
     {0,(1<<12)|(1<<3)|(1<<2)|1,12},
     {0,(1<<31)|(1<<6)|(1<<2)|(1<<1)|1,31},

/*     {0,(1<<22)|(1<<4),22}, */
/*     {1329078,(1<<10)|(1<<1),10}, */

};

unsigned char SUM_CARRY=0;
#define CARRY_LEN 7  /* that's 111 binary, not 7 bits. */

/* G[5], G[6] are selector bits for the functions.
   G[0] through G[4] are the inputs for f[0,1,2,3] */

main (int argc, char**argv) {
   unsigned char bits[5], output, *key;
   unsigned int x,z,a,s,c;

     if (argc>=2) key = argv[1]; else { printf("%s key\n",argv[0]); exit(-1); }
    
     s = strlen(key);
     for (x=c=0;x<7;x++) {
          G[x].data |= key[c++%s] << 24;
          G[x].data |= key[c++%s] << 16;
          G[x].data |= key[c++%s] << 8 ;
          G[x].data |= key[c++%s]      ;
          a = key[c++%s]; while (a--) generator(&G[x]);
     }

     if (argc>=3) z = atoi(argv[2]); else z=1000;
     if (argc>=4) a = atoi(argv[3]); else a=-1;

if (a == -1) {

/*   for (x=0;x<4;x++) init[x](); */
   while (z--) {
     int f1, f2;
     printf("BITS: ");
        for(x=0;x<5;x++) {
	  bits[x] = generator(&G[x]);
	  printf("%d", bits[x]);
  
	}
	printf("\n");
	//        output = func[(generator(&G[5])<<1)|generator(&G[6])](bits);
	f1 = func_xor(bits);
	f2 = func_sum(bits);
	printf("XOR: %d, SUM: %d (%d)\n",f1,f2,SUM_CARRY);
	
	//        putchar('0'+output);
   }

} else while (z--) {
	putchar('0' +generator(&G[a%7]));
   }
}

int func_thresh(char * bits) {
     int c=0,x;
     for(x=0;x<5;x++) if (bits[x]) if (++c==3) return 1;
     return 0;
}
int func_xor(char * bits) {
     int c=0,x;
     for(x=0;x<5;x++) c ^= bits[x];
     return c&1;
}
int func_sum(char * bits) {
     int c=0,x;
     for(x=0;x<5;x++)
          if (bits[x]) if (++SUM_CARRY>CARRY_LEN) SUM_CARRY=0;
     return SUM_CARRY&1;
}
int func_notthresh(char * bits) { 
     int c=0,x;
     for(x=0;x<5;x++) if (!bits[x]) if (++c==3) return 1;
     return 0;
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
