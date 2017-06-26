#include "tp.h"
#include "allegro.h"
#include "paintbsh.h"

paintbrush::paintbrush() {
   if (beetmap[currenttool=0]) destroy_bitmap(beetmap[currenttool]);
   beetmap[currenttool] = load_bitmap("paintbsh.pcx", NULL);
   beetmap[1] = load_bitmap("circle.pcx", NULL);
   beetmap[2] = load_bitmap("bucket.pcx",NULL);
   beetmap[3] = load_bitmap("magnify.pcx",NULL);
   maxbeetmaps=4;
}

paintbrush::~paintbrush() {
   for (int n=0;n<maxbeetmaps;n++)
   if (beetmap[n]) destroy_bitmap(beetmap[n]);
}

void paintbrush::clickleft(int x, int y) {
  oldx = x;
  oldy = y;
  show_mouse(NULL);
  brushy.draw(screen,oldx,oldy);
  brushy.draw(offscreen,oldx,oldy);
/*  putpixel(screen,oldx,oldy,pally.fgcolor);
  putpixel(offscreen,oldx,oldy,pally.fgcolor);
*/
  show_mouse(screen);
}

void paintbrush::stillleft(int x, int y) {

if (x != oldx || y != oldy) {
   show_mouse(NULL);
   mdoline(screen,oldx,oldy,x,y);
//   line(screen,oldx,oldy,x,y,pally.fgcolor);
   mdoline(offscreen,oldx,oldy,x,y);
   show_mouse(screen);

   oldx=x;
   oldy=y;
   }
};

void paintbrush::releaseleft(int x, int y) {
// backup
}

void paintbrush::clickright(int x, int y) {
  oldx = x;
  oldy = y;
  show_mouse(NULL);

  putpixel(screen,oldx,oldy,pally.bgcolor);
  putpixel(offscreen,oldx,oldy,pally.bgcolor);
  show_mouse(screen);
}

void paintbrush::stillright(int x, int y) {
if (x != oldx || y != oldy) {
   show_mouse(NULL);
   line(screen,oldx,oldy,x,y,pally.bgcolor);
   line(offscreen,oldx,oldy,x,y,pally.bgcolor);
   show_mouse(screen);

   oldx=x;
   oldy=y;
   }
}
void paintbrush::releaseright(int x, int y) {  }

