// --=[TP]=-----------------------------------------------------------------
//              TomPaint II Enchanted - Palette
// ---------------------------------------------------------=[palette.cc]=--
#include "tp.h"
#define PALSIZE ((16*10) + 4)
#define XX ((c-startc))
#define PALTOP (COLORS_TOP+38)

#define PAL_HOLD_WAIT 55

   #define palx (SCREEN_W-(50+PALSIZE)) // little buffer
   #define paly (SCREEN_H-(2+ PALSIZE))


palbut::palbut(){
   menuout = startc = clicking = 0;
   behind = NULL;
   palheight = 16;
}

palbut::~palbut(){
   if (behind) destroy_bitmap(behind);
}

void palbut::setbg(int newc) {
if (newc == bgcolor) return;
bgcolor = newc;
drawbg();
}

void palbut::setfg(int newc) {
if (newc == fgcolor) return;
show_mouse(NULL);
int c = fgcolor;
    if (c >= startc && c <= (startc+(4*palheight))) {
          rectfill (screen,    (SCREEN_W-43) + ((XX/palheight)*10),
                                 PALTOP +       ((XX%palheight)*10),
                            8 + (SCREEN_W-43) + ((XX/palheight)*10),
                            8 +  PALTOP       + ((XX%palheight)*10),
                            c);
          rect (screen,         (SCREEN_W-44) + ((XX/palheight)*10),
                                 (PALTOP-1)       + ((XX%palheight)*10),
                            10 + (SCREEN_W-44) + ((XX/palheight)*10),
                            9 +  PALTOP       + ((XX%palheight)*10),
                            255);

    }
    c = fgcolor = newc;
    if (c >= startc && c <= (startc+(4*palheight))) {
          rect (screen,    (SCREEN_W-43) + ((XX/palheight)*10),
                                 PALTOP +       ((XX%palheight)*10),
                            8 + (SCREEN_W-43) + ((XX/palheight)*10),
                            8 +  PALTOP       + ((XX%palheight)*10),
                            255);
          rect (screen,         (SCREEN_W-44) + ((XX/palheight)*10),
                                 (PALTOP-1)       + ((XX%palheight)*10),
                            10 + (SCREEN_W-44) + ((XX/palheight)*10),
                            9 +  PALTOP       + ((XX%palheight)*10),
                            0);
          }
rectfill(screen,SCREEN_W-38,COLORS_TOP+8,SCREEN_W-10,COLORS_TOP+28,fgcolor);
show_mouse(screen);
}

void palbut::stillheld(int mouseb, int x, int y) {
     if (menuout) {
     if (x != oldx || y != oldy) {
     show_mouse(NULL);
       if (mouse_b & 1) setfg(getpixel(screen,x,y));
       if (mouse_b & 2) setbg(getpixel(screen,x,y));
     show_mouse(screen);
     oldx = x;
     oldy = y;
     }
     } else {

       if ((retrace_count > clickedat)) {
       //  grab an appropriate screenbit
       show_mouse(NULL);
         if (!behind) behind=create_bitmap(PALSIZE,PALSIZE);
           blit(screen,behind,palx,paly,0,0,PALSIZE,PALSIZE);
//         textout(screen,font,"Poop",100,100,0);
         rectfill(screen,palx,paly,palx+PALSIZE,paly+PALSIZE,255);
         rect    (screen,palx,paly,palx+PALSIZE-1,paly+PALSIZE-1,0);
         for (int c=0; c<256;c++)
         rectfill(screen,2+ palx + (c>>4)*10,2+   paly+(c&15)*10,
                       11+palx + (c>>4)*10,11+ paly+(c&15)*10, c);

       show_mouse(screen);
         menuout=1;
       }
     }
}

void palbut::released (void) {
     if (menuout) {
        show_mouse(NULL);
        blit(behind,screen,0,0,palx,paly,PALSIZE,PALSIZE);
        show_mouse(screen);
     }
     clicking = menuout = 0;
}

void palbut::draw (void) {
     show_mouse(NULL);

       rectfill(screen,SCREEN_W-48,PALTOP-2,SCREEN_W,SCREEN_H,255);
       rect    (screen,SCREEN_W-48,PALTOP-2,SCREEN_W,SCREEN_H,0  );


       int c;
       for (c=startc;c<255 && c<(startc+(4*palheight));c++) {
           rectfill (screen,    (SCREEN_W-43) + ((XX/palheight)*10),
                                 PALTOP +       ((XX%palheight)*10),
                            8 + (SCREEN_W-43) + ((XX/palheight)*10),
                            8 +  PALTOP       + ((XX%palheight)*10),
                            c);
       }
       c = fgcolor;
       if (c >= startc && c <= (startc+(4*palheight))) {
          rect (screen,    (SCREEN_W-43) + ((XX/palheight)*10),
                                 PALTOP +       ((XX%palheight)*10),
                            8 + (SCREEN_W-43) + ((XX/palheight)*10),
                            8 +  PALTOP       + ((XX%palheight)*10),
                            255);
          rect (screen,         (SCREEN_W-44) + ((XX/palheight)*10),
                                 (PALTOP-1)       + ((XX%palheight)*10),
                            10 + (SCREEN_W-44) + ((XX/palheight)*10),
                            9 +  PALTOP       + ((XX%palheight)*10),
                            0);

       }
       rect(screen,SCREEN_W-48,PALTOP+(palheight*10),SCREEN_W-24,SCREEN_H-1,0);
       rect(screen,SCREEN_W-24,PALTOP+(palheight*10),SCREEN_W,   SCREEN_H-1,0);
       drawbg();
     show_mouse(screen);
}

int palbut::doclick(int b, int x, int y) {
// check my Domain.
if (!toolson) return 0; // no domain at all. tools are off.
if (!b) {
   if (clicking) released();
   return (((x>(SCREEN_W-48))&&(y>COLORS_TOP))||menuout)?1:0;

} else {
if ( (x< (SCREEN_W-48)) || (oldx == x && oldy == y &&  (clicking && b))) {
   // still.
   if (((x > (SCREEN_W-48))&&(y > PALTOP)) || menuout) {stillheld(b,x,y);
   return 1; } else return 0;
//if (menuout || (retrace_count > clickedat)) {
 //  stillheld(b,x,y);
} else {

//} else {
   if (x > (SCREEN_W-48)) {
   // clicked at right.
     if (y > PALTOP && y < (PALTOP + (palheight*10))) {
     // clicked in color list
     int ymod = (y - PALTOP) / 10;
     int xmod = (x-(SCREEN_W-43)) / 10;
     if (b & 1) { int fg = (xmod * palheight) + (ymod) + startc;
                                setfg((fg>255)?255:fg);
                                }
     else { bgcolor = (xmod * palheight) + (ymod) + startc;
                                fgcolor = (fgcolor>255)?255:fgcolor;
                                drawbg(); }
     } else if (y >= (PALTOP + (palheight*10))) {
     // clicked in arrow boxies
     if (!clicking) {
        if (x > (SCREEN_W-24)) {
           // right

           startc += palheight;
           if (startc > (255-(4*palheight))) startc = (255-(4*palheight));
        } else {
           startc -= palheight;
           if (startc < 0) startc = 0;
        }
        draw();
     }
     } else return 0;
if (!clicking) {  clickedat = retrace_count + PAL_HOLD_WAIT; // incremented 70 times/sec
   clicking = 1;}
   oldx = x;
   oldy = y;

   return 1;
   }
   oldx = x;
   oldy = y;
   return (menuout||(x>(SCREEN_W-48)))?1:0;
   }
//}
} // b
return 0;
}

void palbut::drawbg() {
       rectfill(screen,SCREEN_W-48,COLORS_TOP,SCREEN_W,COLORS_TOP+36,bgcolor);
       rectfill(screen,SCREEN_W-38,COLORS_TOP+8,SCREEN_W-10,COLORS_TOP+28,fgcolor);
}

