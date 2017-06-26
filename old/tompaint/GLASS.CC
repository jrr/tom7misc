#include "tp.h"

#define XSIZE (SCREEN_W-(48+100))
#define YSIZE (SCREEN_H-(16))

#define XSIZEO (XSIZE / maglevel)
#define YSIZEO (YSIZE / maglevel)

glass::glass() {
   shadow = NULL;
   ison = 0;
   maglevel = 6;
   DESTX =(100);
   DESTY =(16);
}

glass::~glass() {
   if (shadow) destroy_bitmap(shadow);
}

void glass::on(int x1, int y1) {
   shadow = create_bitmap(XSIZEO,YSIZEO);
   //blit(buffer,shadow,x1,y1,0,0,XSIZEO,YSIZEO);
   topx = x1;
   topy = y1;
   ison = 1;
   draw();

}

void glass::draw() {
   if (!ison) return;
   vline(screen,DESTX-1,DESTY,SCREEN_H,0);
   blit(buffer,shadow,topx,topy,0,0,XSIZEO,YSIZEO);
   update();
}

void glass::update() {
   if (!ison) return;
   stretch_blit(shadow,screen,0,0,XSIZEO,YSIZEO,DESTX,DESTY,XSIZE,YSIZE);
}

void glass::off() {
    ison = 0;
    if (shadow) destroy_bitmap(shadow);
    drawscreen();
}
