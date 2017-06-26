/* ------=[ LUDUS | dirt.c ]=-------( dirty rectangles )-----------

     Dirty rectangle registration, screen drawing.

  ----------------------------------------------------------------- */

/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */



#include <allegro.h>
#include <dirt.h>

rectangle dirt[MAX_DIRTY];
int dirties=MAX_DIRTY+1;    // (always start with a full redraw.)

#ifdef LUDUS_DEBUG
int num_fullscreen = 0,
    num_coagulated = 0,
    num_separate   = 0,
    num_nochange   = 0,
    num_dirty      = 0;
#endif

BITMAP * buffer=NULL;
PALETTE pal;

void force_fs() {
     dirties = MAX_DIRTY+1;
}

void dirt_init () {
     if (buffer) destroy_bitmap(buffer);

        set_gfx_mode(GFX_AUTODETECT, 640, 480, 0, 0);
        buffer = create_bitmap(640,480);
        clear(buffer);
        dirties = MAX_DIRTY+1;
}

void thisisdirty(int x1, int y1, int w,int h) {

// round this up to the nearest 32-bit word. ( (x+4)-(x%4) ?)
     x1--; y1--; w+=4; h+=4;

/*
     x1&= 0xFFFC;
     y1&= 0xFFFc;

     if (w & 3) { w&=0xFFFc; w+=4; }
     if (h & 4) { h&=0xFFFc; h+=4; }
*/

     if (dirties >= MAX_DIRTY) return;  // will do fullscreen update.
     dirt[dirties++] = ((rectangle){x1,y1,w,h});

     // FIXME: this is a hack. It was leaving droppings around the edges,
     // I don't know why -- might want to investigate it instead of just
     // increasing the blitting area by a pixel all around.
}

void dirtyspritemove(int x1, int y1, int x2, int y2, int xsize, int ysize) {
     // see if they are within half of 'size'. If so, just draw the bounding
     // box.

     if ((abs(x2-x1) < (xsize>>1)) && (abs(y2-y1) < (ysize>>1)) )
          {
               int nx1 = min(x1,x2),
                   ny1 = min(y1,y2),
                   nx2 = max(x2,x1) + xsize,
                   ny2 = max(y2,y1) + ysize;
               thisisdirty(nx1,ny1,nx2-nx1,ny2-ny1);
#ifdef LUDUS_DEBUG
               num_coagulated++; // DEBUG
#endif
               return;
          }

    // It's not going to save us any drawing, so just draw them separately:

    thisisdirty(x1,y1,xsize,ysize);
    thisisdirty(x2,y2,xsize,ysize);
#ifdef LUDUS_DEBUG
    num_separate++; // DEBUG
#endif
}

void dirty_draw() {
     if (!dirties) {
#ifdef LUDUS_DEBUG
     num_nochange++;
#endif
     return;
     }      // woo!

     if (dirties>MAX_DIRTY) {  // Too much activity. Redraw Everything.
         blit(buffer,screen,0,0,0,0,640,480);
#ifdef LUDUS_DEBUG
         num_fullscreen++;
#endif
     } else {                  // woo!
          int x;
          for (x=0;x<dirties;x++)
               blit(buffer,screen,dirt[x].x,dirt[x].y,
                                  dirt[x].x,dirt[x].y,
                                  dirt[x].w, dirt[x].h );
#ifdef LUDUS_DEBUG
          num_dirty++;
#endif
     }
     dirties = 0; // Uh... gotta do that.
}

#ifdef LUDUS_DEBUG
void dirt_profile() {
     printf("Stats:\n"
            "\tfullscreen:\t%d (%.2f)\n"
            "\tdirty:     \t%d (%.2f)\n"
            "\tnochange:  \t%d (%.2f)\n"
            "--\n"
            "\tcoagulated:\t%d (%.2f)\n"
            "\tseparate:  \t%d (%.2f)\n\n",
            num_fullscreen, 100*(num_fullscreen / (float)(num_dirty+num_nochange+num_fullscreen)),
            num_dirty,      100*(num_dirty      / (float)(num_dirty+num_nochange+num_fullscreen)),
            num_nochange,   100*(num_nochange   / (float)(num_dirty+num_nochange+num_fullscreen)),
            num_coagulated, 100*(num_coagulated / (float)(num_coagulated+num_separate) ),
            num_separate,   100*(num_separate   / (float)(num_coagulated+num_separate) )
     );                    
}
#endif
