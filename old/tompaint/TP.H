#ifndef __TM7_TP_H_
#define __TM7_TP_H_

// cut down on allegro loading
#define alleg_flic_unused
#define alleg_sound_unused
#define alleg_joystick_unused

// global defines
#define MAX_TOOLS 22
#define TOOLBAR_TOP 0 // menu doesn't overlap any more.
#define COLORS_TOP ((24*(MAX_TOOLS>>1))+TOOLBAR_TOP)
#define VERSION "0.5"
#define MENU_HEIGHT 14

#define YMAXMAC(c) (((c)>MENU_HEIGHT)?(c):MENU_HEIGHT)
#define DRAWMAXX (((toolson)?(SCREEN_W-48):(SCREEN_W))-1)

// headers...
#include "allegro.h"
#include "pmethod.h"
#include "paintbsh.h"
#include "linetool.h"
#include "palette.h"
#include "brush.h"
#include "magnify.h"
#include "glass.h"
#include "slick.h"
#include "menu.h"
#include "hand.h"
// externals

extern palbut pally;
extern brush brushy;
extern glass glassy;
extern int toolson, cpaint, bitx, bity,bitheight,bitwidth ;
extern paintmethod * toolbar[MAX_TOOLS];
extern BITMAP * buffer;

void drawscreen ();
void blit_buffer();

inline void mline(BITMAP * dest, int x1, int y1, int x2, int y2, int c);
inline void mdoline(BITMAP * dest, int x1, int y1, int x2, int y2);

void quit();
void init();
void loadfile();

// inline functions...
inline void mdoline(BITMAP * dest, int x1, int y1, int x2, int y2) {
   if (!glassy.ison) {
      do_line(dest,x1,y1,x2,y2,0,brushdraw);
   } else {
//      if (dest==screen) {
//         do_line(glassy.shadow,x1,y1,x2,y2,0,brushdraw);
//      } else {
         do_line(dest,x1,y1,x2,y2,1,brushdraw);
//      }
//      glassy.update();
   }
}

inline void mline(BITMAP * dest, int x1, int y1, int x2, int y2, int c) {
   if (!glassy.ison) {
      line(dest,x1,y1,x2,y2,c);
   } else {
      if (dest==screen) {
         line(glassy.shadow,x1,y1,x2,y2,c);
      } else {
         line(dest,x1,y1,x2,y2,c);
      }
      glassy.update();
   }
}

#endif
