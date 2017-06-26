/* ----------------------------------------------=[ dirt.h ]=-------------

     Header file for dirty rectangles code.

 -------------------=[ LUDUS ]=------------------------------------------ */

#ifndef LUDUS_DIRT_H
#define LUDUS_DIRT_H

#include <allegro.h>

 ////////////////////////
////////////////////////

  extern BITMAP * buffer;
  extern PALETTE pal;

/////////////////////////
////////////////////////

#define abs(c)   ( ((c)>0)?(c):-(c) )
#define max(a,b) ( ((a)>(b)) ? (a) : (b) )
#define min(a,b) ( ((a)<(b)) ? (a) : (b) )

#define MAX_DIRTY 10

typedef struct {
     int x,y;
     int w,h;
} rectangle;

void dirtyspritemove(int x1, int y1, int x2, int y2, int xsize, int ysize),
     thisisdirty(int,int,int,int),
     dirty_draw(),
     force_fs(),
     dirt_init();

#ifdef LUDUS_DEBUG

     void dirt_profile();

#endif


#endif
