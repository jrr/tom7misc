// -=[pmethod.cc]=---------------------------------------------------------
//                     This is the wrapper class for the painting tools.
// ------------------------------------------------------------------------

#include "pmethod.h"

#define HOLD_WAIT 42 // in 75ths of seconds

paintmethod:: paintmethod(){
   justclicked = menuout = currenttool = 0;
   backsave = NULL;
   maxbeetmaps = 1;
   beetmap[0] = create_bitmap(24,24);
   clear_to_color(beetmap[0],255);
   line (beetmap[0],0,0,24,0,0);
   line (beetmap[0],0,0,0,24,0);
   line (beetmap[0],0,0,24,24,0);
}
paintmethod::~paintmethod(){
   destroy_bitmap(beetmap[0]);
   if (backsave) destroy_bitmap(backsave);
}

void paintmethod::stillleft    (int x,int y) {}
void paintmethod::stillright   (int x,int y) {}
void paintmethod::clickleft    (int x,int y) {}
void paintmethod::clickright   (int x,int y) {}
void paintmethod::releaseleft  (int x,int y) {}
void paintmethod::releaseright (int x,int y) {}
void paintmethod::clickless    (int x,int y) {}

void paintmethod::rightmenu    ()            {}
void paintmethod::onselect     ()            {}
//void paintmethod::drawbrush    (int x,int y) {}

void paintmethod::makemenu     (int x, int y) {
if (maxbeetmaps>1) {
int ox = x, oy = y;
  oldtool=currenttool;
  x -= ((maxbeetmaps)*24 -12); y += 12;
  if (!backsave) backsave = create_bitmap(5+(maxbeetmaps*24),5+24);
  show_mouse(NULL);
  blit(screen,backsave,x,y,0,0,5+maxbeetmaps*24,5+24);
  rectfill(screen,x,y,x+4+maxbeetmaps*24,y+4+24,255);
  rect    (screen,x,y,x+4+maxbeetmaps*24,y+4+24,  0);
  for (int n=maxbeetmaps;n--;)
      blit(beetmap[n],screen,0,0,x+2+(n*24),y+2,24,24);
  line    (screen,x+2,y+26,x+maxbeetmaps*24+2,y+26,0 );
  line    (screen,x+maxbeetmaps*24+2,y+2,x+maxbeetmaps*24+2,y+26,0);
  selectmenuitem(ox,oy,oldtool);
  show_mouse(screen);
//  lastclick = 0;
  menuout = 1;
  gottrace = 0;
}
}

void paintmethod::selectmenuitem(int x,int y, int n) {
   x -= ((maxbeetmaps-(n+1))*24 +10); y += 14;
   blit(beetmap[n],screen,0,0,x,y,24,24);
   xor_mode(TRUE);
   rectfill(screen,x+1,y+1,x+23,y+23,255);
   xor_mode(FALSE);
}

void paintmethod::deselectmenuitem(int x,int y, int n) {
   x -= ((maxbeetmaps-(n+1))*24 +10); y += 14;
      blit(beetmap[n],screen,0,0,x,y,24,24);
}

int  paintmethod::yourmenu     (int xa, int ya, int bbb, int xxx, int yyy) {
static int wasjusthere;
int x = xa, y = ya;
if (menuout && maxbeetmaps>1) {
   if (!bbb) { if (wasjusthere) currenttool=oldtool;
               releasemenu(x,y); return 1; }
   x -= ((maxbeetmaps)*24 -12); y += 12;
   if ( (xxx < x) || (yyy < y) || (xxx > (x+(maxbeetmaps*24)) )
             || (yyy > (y+24)) ) {
             if (wasjusthere) { show_mouse(NULL);
                                deselectmenuitem(xa,ya,oldtool);
                                show_mouse(screen); }
             wasjusthere=0;
             return 1;
   } else {
             int r = (xxx-(x+3))/24;
             if ((r != oldtool) || (!wasjusthere)) {
                show_mouse(NULL);
                deselectmenuitem(xa,ya,oldtool);
                  selectmenuitem(xa,ya,oldtool=r);
                wasjusthere=1;
                show_mouse(screen);
                return 1;
             }
   }
   return 1;
} // maxbeetmaps
return 0;
}

void paintmethod::releasemenu (int x,int y) {
show_mouse(NULL);

 blit(backsave,screen,0,0,12+ x-(maxbeetmaps*24),y+12,5+maxbeetmaps*24,5+24);
    draw(x,y);
    menuout=0;
// show_mouse(screen); // (called by draw())

}

int  paintmethod::clickyou     (int x, int y, int bbb, int xxx, int yyy) {
if (bbb &&
       (xxx > x) && (yyy > y) && (xxx < (x+24)) && (yyy < (y+24))) {
          if (!justclicked) { justclicked = 1;
              gottrace = retrace_count + HOLD_WAIT;
              if (bbb & 1) return 2; // 2 means "select me"
              if (bbb & 2) rightmenu();
              return 1; // I parsed it, but don't select me.
            } else { if (gottrace < retrace_count) makemenu(x,y); return 1; }
  }
justclicked = 0;
return 0;
}

void paintmethod::deselect     (int x, int y) {draw(x,y);}
void paintmethod::select       (int x, int y) {
     show_mouse(NULL);
     xor_mode(TRUE);
     rectfill(screen,x+1,y+1,x+23,y+23,255);
     xor_mode(FALSE);
     show_mouse(screen);
     onselect();
}

void paintmethod::draw(int x, int y) {
     show_mouse(NULL);
     blit(beetmap[currenttool],screen,0,0,x,y,24,24);
     show_mouse(screen);
}

