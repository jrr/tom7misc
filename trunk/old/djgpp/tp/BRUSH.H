// --=[TP]=-----------------------------------------------------------------
//              TomPaint II Enchanted - Brush class
// ------------------------------------------------------------=[brush.h]=--
#ifndef __TM7_BRUSH_H
#define __TM7_BRUSH_H
#include "tp.h"
class brush {
public:
      brush();
     ~brush();

      BITMAP * behind;
      int oldc;
      int oldx, oldy;
void draw    (BITMAP*,int,int);
void gdraw   (BITMAP*,int,int);
void move    (BITMAP*,int,int);
void reset   ();
};
void brushdraw(BITMAP*,int,int,int);
#endif
