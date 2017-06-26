#ifndef __TM7_DMG_H
#define __TM7_DMG_H

#include <stdlib.h>

#define MAXINT 0x7FFFFFFF
// #define TRACE(m) printf(#m "\n"); m; printf("-\n")
#define TRACE(m) m

struct chord {
     signed int notes[3];
     chord () { notes[0] = notes[1] = notes[2] = 0; }
};

struct scheme {
     int length;
     scheme * next;
     chord chords[4];
     int method;
     scheme () { next = NULL; method = -1;}
};

extern scheme * SCHEMES;
extern int numschemes;

#endif
