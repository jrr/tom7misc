#include "tp.h"

#define XSIZEN (SCREEN_W/(level<<1))
#define YSIZEN (SCREEN_H/(level<<1))

#define XSIZER (SCREEN_W/level)
#define YSIZER (SCREEN_H/level)

#define FIXUPX(c)  (((c)<(DRAWMAXX-XSIZER))?(((c)>0)?(c):0):(DRAWMAXX-XSIZER))
#define FIXUPY(c)  (((c)>(MENU_HEIGHT))?(((c)<(SCREEN_H-YSIZER))?(c):(SCREEN_H-YSIZER)):(MENU_HEIGHT))

magnify::magnify() {
   if (beetmap[currenttool=0]) destroy_bitmap(beetmap[currenttool]);
   beetmap[currenttool] = load_bitmap("magnify.pcx", NULL);
   level = glassy.maglevel;
   started = 0;
   old_retrace = 0;
}

void magnify::onselect() {
   started = 0;
   glassy.off();
}

magnify::~magnify() {
   for (int n=0;n<maxbeetmaps;n++)
     if (beetmap[n]) destroy_bitmap(beetmap[n]);
}

void magnify::clickless(int x, int y) {
     if (oldx == FIXUPX(x-XSIZEN) && oldy == FIXUPY(y-YSIZEN)/* || retrace_count < old_retrace*/) return;
     xor_mode(TRUE);
     show_mouse(NULL);
     if (started) rect(screen,oldx,oldy,oldx+XSIZER,oldy+YSIZER,255);
       else started = 1;
     oldx = FIXUPX(x-XSIZEN);
     oldy = FIXUPY(y-YSIZEN);
     rect(screen,oldx ,oldy ,oldx+XSIZER,oldy+YSIZER,255);
     show_mouse(screen);
     xor_mode(FALSE);
}

void magnify::clickleft(int x, int y) {

}

void magnify::stillleft(int x, int y) {
     clickless(x,y);
}

void magnify::releaseleft(int x, int y) {
     show_mouse(NULL);
     xor_mode(TRUE);
     rect(screen,oldx,oldy,oldx+XSIZER,oldy+YSIZER,255);
     glassy.on(FIXUPX(x-XSIZEN),FIXUPY(y-YSIZEN));
     started = 0;
     xor_mode (FALSE);
     show_mouse(screen);
}
