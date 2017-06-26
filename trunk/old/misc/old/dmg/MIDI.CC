#include "midi.h"

const unsigned
char midi_header[] = { 'M','T','h','d',
                   0 , 0 , 0 , 6,
                   0 , SMF_TYPE , 0 , 4, // 4 = numtrks = midi_header[11]
                   0 , DELTA_TICKS };
#define MIDI_HEAD_SIZE 14
#define MIDI_SIZE_LOC 4
#define MIDI_TRACKSNUM_LOC 11

const unsigned
char track_header[]  = { 'M','T','r','k',
                       0, 0, 0, 0, }; // 0000 = length = track_header[4]
#define TRACK_SIZE_LOC 4
#define TRACK_HEAD_SIZE 8

// #define BYTE(c) do {printf("Write %0X [%d] at %d\n",c,loc,__LINE__); buffer[loc++] = c; } while (0)
#define BYTE(c) buffer[loc++] = c
// #define TRACE(m) printf(#m "\n"); m; printf("-\n")
#define TRACE(m) m

void writevar (long value, unsigned char * buffer, int & loc) {
     unsigned long muffer;
     TRACE(muffer = value & 0x7f);
     while ((value >>= 7) > 0) {
        muffer <<= 8;
        muffer |= 0x80;
        muffer += (value & 0x7f);
     }
     while (1) {
        BYTE(muffer);
        if (muffer & 0x80) muffer >>= 8;
        else break;
     }
}

void output_song( song s, FILE *& outfile) {
     int trackies = 0;
     track * l = s.tracks;
     string data;

     for (int q=0;q<MIDI_HEAD_SIZE;q++)
          data += midi_header[q];

     TRACE(int length = 0);

    // printf (" One. \n");

     while (l) {
          TRACE(length += output_track(*l, data));
       //   printf (" Did a track at %x. \n", l);
          trackies++;
          l = l->next;
     }

     // printf (" Two. \n");
#if 0
     data[MIDI_SIZE_LOC   ] = (length>>24)&255;
     data[MIDI_SIZE_LOC +1] = (length>>16)&255;
     data[MIDI_SIZE_LOC +2] = (length>> 8)&255;
     data[MIDI_SIZE_LOC +3] =  length     &255;
#endif
     data[MIDI_TRACKSNUM_LOC] = trackies;

     // printf (" Three. \n");

     for (unsigned int q=0;q<data.length();q++) putc(data[q],outfile);

     // printf (" Four. \n");

}

int output_track( track t, string & whence ) {

     int last_time=0,size=TRACK_HEAD_SIZE,loc=0;

	unsigned char oldstat = 0x00;

	// printf (" I am in output_track. \n" );

     TRACE(event * s = t.events);
     while (s) { size += 7; s = s->next; }
     TRACE(unsigned char * buffer = (unsigned char*)malloc((size | 0x07)+1) );

     if (!buffer) { printf ( " No memory.\n" ) ; exit ( 0 ) ; }

     /* write header */

     for (int q=0;q<TRACK_HEAD_SIZE;q++)
          buffer[q] = track_header[q];

     TRACE(loc = TRACK_HEAD_SIZE);
     s = t.events;

     /* write 1100xxxx (xxxx = channel) then 1 data byte 0xxxxxxx
               for instrument number */
     writevar(0,buffer,loc); /* delta-time */
     BYTE(0xC0 | t.channel);
     BYTE(t.instrument);


	// printf (" I am in output_track, about to event-loop. \n" );


     while (s) {
          /* for each event, write out delta time */
          TRACE(writevar((s->ticks)-last_time,buffer,loc));
          last_time = s->ticks;

          /* then write the data */

	  if (s->data[0] != oldstat) {
		oldstat = s->data[0];
		BYTE(oldstat);
          }

          for (int q=1;q<s->bytes;q++)
		BYTE(s->data[q]);
	  // printf (" Event at %x. \n", s);

     s = s->next;
     }

     /* writeout meta-event 0x2F = end of track */
     BYTE(0x00); 
     BYTE(0xFF);
     BYTE(0x2F);
     BYTE(0x00);

     /* loc holds the length of the file so far (including header) */

     loc -= TRACK_HEAD_SIZE;
     buffer[TRACK_SIZE_LOC   ] = (loc>>24)&255;
     buffer[TRACK_SIZE_LOC +1] = (loc>>16)&255;
     buffer[TRACK_SIZE_LOC +2] = (loc>> 8)&255;
     buffer[TRACK_SIZE_LOC +3] =  loc     &255;

     /* now write */

     for (int q=0;q<(loc + TRACK_HEAD_SIZE);q++) whence += buffer[q];

     free (buffer);
     return loc + TRACK_HEAD_SIZE;
}

void writenote(track & t, note n) {
     t.insert( new event( (event) { n.start, NULL,
               {0x90 | t.channel, n.key,n.vel} ,3 } ) );
     t.insert( new event( (event) { n.start+n.length - 1, NULL,
               {0x90 | t.channel, n.key,0} ,3 } ) );
}

