#include "bass.h"
#include "midi.h"

void (*bass4[])(int,int,track*,scheme) = {
     bass4_f4,
     bass4_f4,
     bass4_f4,
     bass4_f4,
};

void construct_measure_bass(int root, int measure, track * t, scheme & s) {

     printf ("Measure %d.\n", measure);
     switch(s.length) {
          case 4:
               if (s.method == -1)
                    s.method = (int)((float(random())/float(MAXINT))*float(NUM_BASS4));
               bass4[s.method](root,measure,t,s);
          break;
          case 3:
          /* break; */
          default:
          printf ("I can't handle a scheme of length %d yet. Aborting.\n", s.length);
          exit(-1);
     }
}


void bass4_f1(int root,int measure,track* t,scheme s) {
     for (int n=0;n<16;n++) {
          writenote(*t,
           (note){root+s.chords[n>>2].notes[1],100,(measure*WHOLE) + n*SIXTEENTH,SIXTEENTH});
          if (!(n&1)) {
            writenote(*t,
            (note){root+s.chords[n>>2].notes[0],100,(measure*WHOLE) + n*SIXTEENTH,EIGHTH});
            writenote(*t,
            (note){root+s.chords[n>>2].notes[2],100,(measure*WHOLE) + n*SIXTEENTH,SIXTEENTH});
          }
     }
}
void bass4_f2(int root,int measure,track* t,scheme s) {
     for (int n=0;n<16;n++) {
          writenote(*t,
               (note){root+s.chords[n>>2].notes[((n%4)==3)?0:(n%4)],100,(measure*WHOLE) + n*SIXTEENTH, SIXTEENTH});
     }
}
void bass4_f3(int root,int measure,track* t,scheme s) {
     for (int n=0;n<16;n++) {
          writenote(*t,
               (note){root+s.chords[n>>2].notes[((n%4)==3)?1:(n%4)],100,(measure*WHOLE) + n*SIXTEENTH, SIXTEENTH});
     }
}
void bass4_f4(int root,int measure,track* t,scheme s) {
     static long int x[4] = {random(),random(),random(), random()};
     for (int n=0;n<16;n++) {
         for (int m=0;m<3;m++) {
               switch (n%4) {
               case 0: if (x[0] & 1) {
                         writenote(*t,
                         (note){root+s.chords[n>>2].notes[m],90 + 10*((x[0]&6)>>1),(measure*WHOLE) + n*SIXTEENTH, SIXTEENTH/*((x[0]&24)>>3)*/});
                         } break;
               case 1: if (x[1] & 32) {
                         writenote(*t,
                         (note){root+s.chords[n>>2].notes[m],90 + 10*((x[1]&192)>>6),(measure*WHOLE) + n*SIXTEENTH, SIXTEENTH/*((x[1]&768)>>8)*/});
                         } break;
               case 2: if (x[2] & 512) {
                         writenote(*t,
                         (note){root+s.chords[n>>2].notes[m],90 + 10*((x[2]&3072)>>10),(measure*WHOLE) + n*SIXTEENTH, SIXTEENTH/*((x[2]&12288)>>12)*/});
                         } break;
               case 3: if (x[3] & 32768L) {
                         writenote(*t,
                         (note){root+s.chords[n>>2].notes[m],90 + 10*((x[3]&98304)>>15),(measure*WHOLE) + n*SIXTEENTH, SIXTEENTH/*((x[3]&393216)>>17)*/});
                         } break;
              }
         }
     }
}
