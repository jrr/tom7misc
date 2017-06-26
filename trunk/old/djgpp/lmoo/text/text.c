
#include <allegro.h>

void _plus_linear_draw_sprite8(struct BITMAP *bmp, struct BITMAP *sprite, int x, int y, int p);
void _linear_draw_sprite8(struct BITMAP *bmp, struct BITMAP *sprite, int x, int y);

void ptextout ( BITMAP * dest, FONT * f, const char * str, int x,
                                                  int y, int p ) ;


int main (int argc, char ** argv) {
     BITMAP * buffer;
     PALETTE pal; 

     DATAFILE *d;
     FONT * bigfont;

     allegro_init();
     install_keyboard();

     d = load_datafile_object("fdata.dat","bigfont");
     bigfont = d->dat;

     set_gfx_mode(GFX_AUTODETECT, 640, 480, 0, 0);

     buffer = load_pcx("testes.pcx",pal);

     set_palette(pal);

     ptextout ( screen, bigfont, "Oshin: ", 10, 10, 12 );

     ptextout ( screen, bigfont, "What's going on?",
          10+text_length(bigfont,"Oshin: "), 10, 5 );

     while (!keypressed()) ;

     exit(0);
}

/* This expects a 256-color bitmap dest and a proportional font src.
   NO EXCEPTIONS, or it won't work. The color p will be added to
   characters (in some range); others will be drawn with their original
   colormap. */

void ptextout ( BITMAP * dest, FONT * f, const char * str, int x,
                                                  int y, int p ) {
   FONT_PROP *fp;
   BITMAP *b;
   int c;
   fp = f->dat.dat_prop;

   while (*str) {
      c = (int)*str - ' ';
      if ((c < 0) || (c >= FONT_SIZE)) c = 0;
      b = fp->dat[c];
      if (b) {
         _plus_linear_draw_sprite8(dest,b,x,y,p); /* ADD THIS:
                                                     only send plus if
                                                     c is in range! */
         x += b->w;
         if (x >= dest->cr) return;
      }
      str++;
   }
}
