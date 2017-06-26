#include "midi.h"
#include <fstream.h>
#include <time.h>

#include "bass.h"
#include "dmg.h"

scheme * SCHEMES = NULL;

int numschemes = 0;

scheme rndscheme(int);
void read_scheme(string s);

main (int argc, char ** argv) {
     song sing;

    srandom(time(NULL));

    read_scheme("dance.scm");

     int force = -1;
     if (argc == 2) { force = atoi(argv[1]); }

    scheme verse_scheme = rndscheme(force);

     sing.tracks = new track((track){0,0,NULL,NULL});

     int root = 50;

    for (int measure=0; measure<4; measure ++) {
          construct_measure_bass(root, measure, sing.tracks, verse_scheme);
    }
 
     FILE * x;

     if ((x = fopen ("new.mid", "wb+"))) {
          printf (" Zero. \n");
          output_song(sing, x);
          fclose (x);
          exit(0);
     }
}

void read_scheme(string s) {
TRACE(     ifstream inways(s.c_str()));
TRACE(     int n );
     while (inways >> n) {
TRACE(          scheme * news = new scheme );
          news->next = SCHEMES;
          SCHEMES = news;
TRACE(          SCHEMES->length = n  );
          int q=0;
          while (q++<n) {
               for (int z=0;z<3;z++) {
                inways >> SCHEMES->chords[q-1].notes[z];
/*                printf ( "I stuffed %d into SCHEMES->chords[%d].notes[%d]\n",
                          SCHEMES->chords[q-1].notes[z], q-1, z); */

                          }
                }
          numschemes++;
     }
TRACE(     inways.close() );
}

scheme rndscheme(int force) {
     int num;
     scheme * head = SCHEMES;
     if (force == -1) {
          num = (int)((float(random())/float(MAXINT))*float(numschemes));
     } else num = force;
     printf ("Chose scheme #%d of %d", num, numschemes);
     while (num--) head = head->next;
     return * head;
}

