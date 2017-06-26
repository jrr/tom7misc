#ifndef __TM7_MAGNIFY_H_
#define __TM7_MAGNIFY_H_

#include "tp.h"

class magnify : public paintmethod { // : paintmethod {
public:
        magnify();
       ~magnify();

        int oldx;
        int oldy;

        int startx;
        int starty;

        int oldpos, started, level, old_retrace;

virtual void stillleft(int,int);
//virtual void stillright(int,int);
virtual void clickleft(int,int);
//virtual void clickright(int,int);
virtual void releaseleft(int,int);
//virtual void releaseright(int,int);

void clickless(int,int);
void onselect();
/*
virtual void draw(int,int);
virtual void select(int,int);
virtual void deselect(int,int);
*/
};
#endif
