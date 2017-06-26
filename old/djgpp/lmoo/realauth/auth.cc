
#ifndef LUDUS_OFFICIAL_BUILD
#error The auth module should not be built for unofficial clients/servers.
#endif

#include "global.h"
#include "auth.h"

uchar sub[4][16] =
{
  {14,1,2,3,4,12,13,15,0,5,6,7,8,9,10,11,},
  {8,3,4,5,9,10,1,14,13,0,15,2,6,7,11,12,},
  {4,5,1,14,9,12,15,0,2,3,6,10,11,7,8,13,},
  {12,13,1,11,3,2,9,10,14,6,7,8,15,0,4,5,},
};

uchar perm[32] =
{ 16, 7,20,21,29,12,28,17, 1,15,23,26, 5,18,31,10,
   2, 8,24,14,32,27, 3, 9,19,13,30, 6,22,11, 4,25,
};

/* Encryption/Decryption Functions */

ulong func (ulong a, ulong k) {
     ulong temp;
     temp = permute(subst(a)) ^ k;
     return temp;
}


wint enc_block ( wint b, wint k ) {
     wint result=b,temp;
     int i;
     for (i=0;i<16;i++) {
           temp.h = result.l;
           temp.l = func(result.l,kcompress(k,i)) ^ result.h;
           result = temp;
        }
     result.h ^= result.l;
     result.l ^= result.h;
     result.h ^= result.l;

     return result;
}

wint dec_block ( wint b, wint k ) {
     wint result=b,temp;
     int i;
     for (i=0;i<16;i++) {
           temp.h = result.l;
           temp.l = func(result.l,kcompress(k,15-i)) ^ result.h;
           result = temp;
        }
     result.h ^= result.l;
     result.l ^= result.h;
     result.h ^= result.l;

     return result;
}


ulong kcompress(wint k, int i) {
     return ROL((i%2)?k.h:k.l, i);
}

ulong subst( ulong in ) {
     ulong out=0,x;

     for (x=0;x<8;x++) {
         out |= ( sub[3&(ROL(in,(x<<2)))][(  (in>>(32-(x<<2))) & 15)] ) << (x<<2) ;
     }
     return ROL(out,3);
}

ulong permute( ulong in ) {
        ulong out = 0,x;
        for (x=0;x<32;x++) {
            out |= bit(in,perm[x]) << x;
        }
     return out;
}


/* This is debugging-only stuff. */

void print_wint (wint x) {
     printf ("0x%04X%04X%04X%04X\n", (uint)x.h>>16,(uint)x.h&0xFFFF,
                                     (uint)x.l>>16,(uint)x.l&0xFFFF);
}

wint keyfromtext(uchar * m) {
     uchar map[256] = {
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,3,4,5,6,7,8,9,10,11,12,0,0,0,0,0,
       0,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
          35,36,37,38,0,0,0,0,0,
       0,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
          61,62,63,0, /* ... */
     };
     uchar pad[11] = {0};
     wint k;
     strncpy((char*)pad,(char*)m,11);
     
     k.h = (map[pad[0]]<<26)
         | (map[pad[1]]<<20)
         | (map[pad[2]]<<14)
         | (map[pad[3]]<<8 )
         | (map[pad[4]]<<2 )
         | (map[pad[5]]>>3 );
     k.l = (map[pad[5]]<<28)
         | (map[pad[6]]<<22)
         | (map[pad[7]]<<16)
         | (map[pad[8]]<<10)
         | (map[pad[9]]<<4 )
         | (map[pad[10]]>>2 );

     return k;

}

