#include "tp.h"

linetool::linetool() {
   if (beetmap[currenttool=0]) destroy_bitmap(beetmap[currenttool]);
   beetmap[currenttool] = load_bitmap("linetool.pcx", NULL);
}

linetool::~linetool() {
   for (int n=0;n<maxbeetmaps;n++)
   if (beetmap[n]) destroy_bitmap(beetmap[n]);
}

void linetool::clickleft(int x, int y) {
  startx = x;
  starty = y;
  oldx   = x;
  oldy   = y;
}

void linetool::stillleft(int x, int y) {
if (oldx != x || oldy != y) {
   xor_mode(TRUE);
   show_mouse(NULL);
   mline(screen,startx,starty,oldx,oldy,255);
   mline(screen,startx,starty,oldx = x, oldy=y, 255);
   show_mouse(screen);
   xor_mode(FALSE);
   }
};

void linetool::releaseleft(int x, int y) {
   xor_mode(TRUE);
   show_mouse(NULL);
   mline(screen,startx,starty,oldx,oldy,255);
   xor_mode(FALSE);
   mdoline(screen,startx,starty,x,y);
   mdoline(offscreen,startx,starty,x,y);
   show_mouse(screen);
}

void linetool::clickright(int x, int y) {

}

void linetool::stillright(int x, int y) {

}

void linetool::releaseright(int x, int y) {  }

