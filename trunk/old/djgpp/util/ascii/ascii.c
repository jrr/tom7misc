// public domain by Tom Murphy 7
// interlaces arg1, arg2 pcx files and writes as arg3. 
// for virtual-io glasses

#include <stdio.h>
#include <allegro.h>

int main (int argc, char**argv) {
     allegro_init();
     if (argc != 2) {
        printf("interl in.pcx > ascii.txt\n");
        exit(-1);
     } else {
        PALETTE pal;
        int y,X,Y;
        BITMAP * one = load_pcx(argv[1],pal);
//        set_palette(pal);
        if (! one) {
          printf("Files error.\n");
          exit(-1);
        }
        X = one->w;
        Y = one->h;
	{
	  // ...

	}
        exit(0);
     }
}







