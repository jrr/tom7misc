#ifndef __TM7_SERVER_H
#define __TM7_SERVER_H

#include "tsocket.h"

tsocket * makeserversock(int port);
void do_serverstuff(tsocket * );

void ignoresigpipe(int);

#endif
