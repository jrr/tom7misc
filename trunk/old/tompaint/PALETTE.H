#ifndef __TM7_PALETTE_H
#define __TM7_PALETTE_H

class palbut {
public:
       int oldx, oldy, oldb;
       BITMAP * behind;
       int startc, palheight;
       int menuout, fgcolor, bgcolor;
       int clicking;
       long clickedat;
       palbut();
      ~palbut();
void clicked  (int mouseb, int x, int y);
void stillheld(int mouseb, int x, int y);
int doclick  (int b, int x, int y);
void released (void);
void draw     ();
void drawfg   ();
void drawbg   ();
void setfg    (int);
void setbg    (int);
};
#endif
