#ifndef __WM_GAMEREC_H
#define __WM_GAMEREC_H

#include "epoch.h"

#define GAMETIMEOUT 180 // == 3 minutes

class gamerec {
public:
    short from;
    short to;
    char gametype;
    short overwrite;

    long timeout;
    
    epochgame * game;

    gamerec();
    gamerec(short from, short to, char gametype, short overwrite,
            long timeout, epochgame * game);
   ~gamerec();
};

void sendrequest(short,short,short,char,char,string);

#endif
