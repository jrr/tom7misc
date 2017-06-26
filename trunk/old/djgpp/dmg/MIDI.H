#ifndef __TM7_MIDI_H
#define __TM7_MIDI_H

#include <string>
#include <stdlib.h>
#include <stdio.h>

void writevar (register long value, unsigned char * buffer, int & loc);

#define SMF_TYPE 1
#define DELTA_TICKS (0x60)

#define QUARTER (DELTA_TICKS)
#define HALF (DELTA_TICKS << 1)
#define WHOLE (DELTA_TICKS << 2)
#define EIGHTH (DELTA_TICKS >> 1)
#define SIXTEENTH (DELTA_TICKS >> 2)
#define THIRTYSECONDTH (DELTA_TICKS >> 3)

/* enum { E_NOTEON, E_NOTEOFF, } */

struct track;

struct song {
     track * tracks;
     song () { tracks = NULL; }
};

struct event {
/*     int type; */
     int ticks;
     event * next;
     unsigned char data[3]; /* status|data1|data2 */
     unsigned char bytes;   /* how many data bytes to write? */
};

struct track {
     int channel;
     int instrument;
     event * events;
     track * next;
/*     track () { events = NULL;
                channel = 1;
                instrument = 0;
              } */
     void insert(event * e) {
          event ** z = &events;
          while (*z && e->ticks > (*z)->ticks) z = &((*z)->next);
          e->next = *z;
          *z = e;     /* man, I hope this works. */
     }
};

struct note {
     unsigned char key,
                   vel;
     int start, length;
};

void writenote(track & t, note n);
int output_track( track t, string & whence );
void output_song( song s, FILE *& outfile);

#endif
