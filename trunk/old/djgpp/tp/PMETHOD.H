#ifndef __TM7_PMETHOD_H
#define __TM7_PMETHOD_H

#include "allegro.h"

class paintmethod {
public:
        paintmethod();
       ~paintmethod();

        int bgcolor,
            color,
            oldx,
            oldy;

        int maxbeetmaps, currenttool, justclicked, oldtool;

        BITMAP * beetmap[6]; // maximum Beetmaps
        BITMAP * offscreen;
        BITMAP * backsave;
        int menuout, gottrace,lastclick;

void paintmethod::  selectmenuitem(int x,int y, int n);
void paintmethod::deselectmenuitem(int x,int y, int n);
virtual void stillleft   (int,int);
virtual void stillright  (int,int);
virtual void clickleft   (int,int);
virtual void clickright  (int,int);
virtual void releaseleft (int,int);
virtual void releaseright(int,int);
virtual void clickless   (int,int);

virtual void onselect    ();
//virtual void paintmethod::drawbrush    (int x,int y)

virtual void rightmenu();

/*virtual*/ void deselect   (int,int);
/*virtual*/ void select     (int,int);
/*virtual*/ void draw       (int,int);
            void makemenu   (int,int);
            void releasemenu(int,int);
int clickyou     (int x, int y, int bbb, int xxx, int yyy);
int yourmenu     (int,   int,   int    , int    , int    );

};

#endif
