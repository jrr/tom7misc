#include "tp.h"

brush::brush() {
     behind = NULL;
     reset();
}

brush::~brush() {
     if (behind) destroy_bitmap(behind);
}

void brush::reset() {
     oldc = oldx = oldy = -1;
     if (behind) {destroy_bitmap(behind);
     behind = NULL; }
}

void brush::draw (BITMAP*dest,int x, int y) {
   putpixel(dest,x,y,pally.fgcolor);
}

void brush::gdraw (BITMAP*dest,int x, int y) {
   if (dest==screen)
      rectfill(screen,x*glassy.maglevel,y*glassy.maglevel,(x+1)*glassy.maglevel,(y+1)*glassy.maglevel, pally.fgcolor);
   else
      putpixel(dest,x,y,pally.fgcolor);
}


void brush::move (BITMAP*dest,int x, int y) {

if (oldc != -1) {
   // erase old brush
   putpixel(dest,oldx,oldy,oldc);
}
   oldc=getpixel(dest,x,y);
   putpixel(dest,oldx=x,oldy=y,pally.fgcolor);
}

//////****HAAACK****///////
void brushdraw (BITMAP*a, int b, int c, int d) {
     if (!d) {
     brushy.draw(a,b,c);
     } else if (d==1) { brushy.gdraw(a,b,c); }

}
