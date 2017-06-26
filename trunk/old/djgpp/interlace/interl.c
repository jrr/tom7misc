// public domain by Tom Murphy 7
// interlaces arg1, arg2 pcx files and writes as arg3. 
// for virtual-io glasses

#include <stdio.h>
#include <allegro.h>

int main (int argc, char**argv) {
     allegro_init();
     if (argc != 4) {
        printf("interl in1.pcx in2.pcx out.pcx\n");
        exit(-1);
     } else {
        PALETTE pal;
        int y,X,Y;
        BITMAP * one = load_pcx(argv[1],pal);
        BITMAP * two = load_pcx(argv[2],pal);
//        set_palette(pal);
        if (!( one && two )) {
          printf("Files error.\n");
          exit(-1);
        }
        X = one->w;
        Y = one->h;
     {        BITMAP * buffer=create_bitmap(X,Y);
        clear_to_color(buffer,0);
        for (y=0;y<Y;y++) {
          if (y&1) blit(one,buffer,0,y,0,y,X,Y);
          else     blit(two,buffer,0,y,0,y,X,Y);
        }

        save_pcx(argv[3],buffer,pal);
}
        exit(0);
     }
}
