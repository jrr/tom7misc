#include "tp.h"
#include "allegro.h"
#include "paintbsh.h"

hand::hand() {
   if (beetmap[currenttool=0]) destroy_bitmap(beetmap[currenttool]);
   beetmap[currenttool] = load_bitmap("hand.pcx", NULL);
   maxbeetmaps=1;
}

hand::~hand() {
   for (int n=0;n<maxbeetmaps;n++)
   if (beetmap[n]) destroy_bitmap(beetmap[n]);
}

void hand::clickleft(int x, int y) {

}

void hand::stillleft(int x, int y) {

}

void hand::releaseleft(int x, int y) {

}

void hand::clickright(int x, int y) {

}

void hand::stillright(int x, int y) {

}

void hand::releaseright(int x, int y) {  }

