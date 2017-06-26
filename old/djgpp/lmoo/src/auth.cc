/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */

#ifndef LUDUS_OFFICIAL_BUILD
#error The auth module should not be built for unofficial clients/servers.
#endif

#include "global.h"
#include "auth.h"

/* These are stubs for the real algorithms, which may or may not contain
   unexportable code. */

ulong func (ulong a, ulong) {
     return a;
}

wint enc_block ( wint b, wint ) {
     return b;
}

wint dec_block ( wint b, wint ) {
     return b;
}

ulong kcompress(wint k, int ) {
     return k.h;
}

ulong subst( ulong in ) {
     return in;
}

ulong permute( ulong in ) {
     return in;
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

