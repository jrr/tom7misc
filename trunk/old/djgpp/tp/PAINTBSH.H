#ifndef __TM7_PAINTBSH_H_
#define __TM7_PAINTBSH_H_

#include "tp.h"

class paintbrush : public paintmethod { // : paintmethod {
public:
        paintbrush();
       ~paintbrush();

        int oldx;
        int oldy;

        int oldpos;

//        BITMAP * mybitmap;

virtual void stillleft(int,int);
virtual void stillright(int,int);
virtual void clickleft(int,int);
virtual void clickright(int,int);
virtual void releaseleft(int,int);
virtual void releaseright(int,int);
/*
virtual void draw(int,int);
virtual void select(int,int);
virtual void deselect(int,int);
*/
};
#endif
