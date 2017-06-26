#ifndef __TM7_GLASS_H
#define __TM7_GLASS_H

class glass {
public:
     glass();
    ~glass();

    int ison, maglevel;
    int topx, topy,DESTX,DESTY;

    BITMAP * shadow;

void off();
void on(int,int);
void draw();
void update();
};

#endif
