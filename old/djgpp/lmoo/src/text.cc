/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */

#include "global.h"

/* ----=[ LUDUS | text.c ]=------( clientside text rendering routines )- */


/* pluscodes for ftextout, etc. */

uchar text_pluspal[256] = {
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,12, 0, 0, 1, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 4, 0, 0, 0, 0, 0, 0,16, 0, 0, 0, 0, 0, 0,
      0, 0,13, 0, 0, 2, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 5, 0, 0, 0, 0, 0, 0,17, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

     FONT * bigfont,
          * smallfont;

void text_init () {
     DATAFILE * d;
     if (
     (d = load_datafile_object(DATAFI,"bigfont")))
     { bigfont = (FONT*)d->dat;
       d = load_datafile_object(DATAFI,"smallfont");
       smallfont = (FONT*)d->dat;
     }
     else { printf("Can't open the datafile.\n"); exit(-1); }
}

int ftextlen ( FONT *f, const char * str) {

   FONT_PROP *fp;
   BITMAP * b;
   int c,x=0;
   fp = f->dat.dat_prop;

   while (*str) {
      if (*str=='^') {
          if (!++str) break;
          if (!++str) break;
          ++str; continue;
      }
      if (*str=='~') {
          if (!++str) break; /* for malformatted strings */
          ++str;
          continue;
      }
      c = (int)*str - ' ';
      if ((c < 0) || (c >= FONT_SIZE)) c = 0;
      if ((b = fp->dat[c])) x += b->w;
      str++;
   }
   return x;
}

/* ftextout() draws a formatted (fancy) text string:

     Hi ~yBob~W, how are you?
*/

#define hexval(c) ( ( (((c)|32)>='a') && (((c)|32)<='b') ) \
     ? ( 10 + (((c)|32)-'a') ):(( ((c)>='0') && ((c)<='9') )?((c)-'0'):0) )



void ftextout ( BITMAP * dest, FONT * f, const char * str, int x,
                                                  int y) {
   FONT_PROP *fp;
   BITMAP *b;
   int c,currentcolor=0; /* (default to +0) */
   fp = f->dat.dat_prop;

   while (*str) {
      if (*str=='~') {
          currentcolor = text_pluspal[(uchar)*++str];
          if (!str) break; /* for malformatted strings */
          ++str;
          continue;
      }
      if (*str=='^') {
          if (!++str) break;
          c = hexval(*str)<<4;
          if (!++str) break;
          c |= hexval(*str);
      } else 
      c = (int)*str;
      c -= ' ';
      if ((c < 0) || (c >= FONT_SIZE)) c = 0;
      if ((      b = fp->dat[c])) {
         plus_linear_draw_sprite8(dest,b,x,y,(c<=TEXT_MAX_PLUS_CHAR)?currentcolor:0);

               /* NOTE: CHANGE THIS TO SELECT BETWEEN THE TWO FUNCTIONS?
                  (instead of just sending +0 which doesn't do anything)
                  */

         x += b->w;
         if (x >= dest->cr) return;
      }
      str++;
   }
}


void ptextout ( BITMAP * dest, FONT * f, const char * str, int x,
                                                  int y, int p ) {
   FONT_PROP *fp;
   BITMAP *b;
   int c;
   fp = f->dat.dat_prop;

   while (*str) {
      c = (int)*str - ' ';
      if ((c < 0) || (c >= FONT_SIZE)) c = 0;
      if ((      b = fp->dat[c])) {
         plus_linear_draw_sprite8(dest,b,x,y,(c<=TEXT_MAX_PLUS_CHAR)?p:0);

               /* NOTE: CHANGE THIS TO SELECT BETWEEN THE TWO FUNCTIONS?
                  (instead of just sending +0 which doesn't do anything)
                  */
         x += b->w;
         if (x >= dest->cr) return;
      }
      str++;
   }
}

/* what is this? */
int fnumchars(const char * go) {
     int tilde=0,esc=0,golen=0;
     for (uint x=0;go[x];x++)
          if (go[x] == '^') {
               esc = 2;
          } else if (esc) {
               esc --;
          } else if (go[x] == '~') {
               tilde = 1;
          } else if (tilde) {
               tilde=0;
          } else golen++;
     return golen;
}
