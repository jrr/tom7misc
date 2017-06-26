
/* -----=[LUDUS | text.h]=----

     Routines for drawing text.

   --------------------------- */

#ifndef LUDUS_TEXT_H
#define LUDUS_TEXT_H

#include <allegro.h>

/* Does this go here? Or in internal_graphics or something? */
//void plus_linear_draw_sprite8(struct BITMAP *bmp, struct BITMAP *sprite, int x, int y, int p);

#define TEXT_MAX_PLUS_CHAR (128)

void ftextout (BITMAP *, FONT *, const char *, int x, int y),
     ptextout (BITMAP *, FONT *, const char *, int x, int y, int p ),
     text_init();

int  ftextlen ( FONT *f, const char * str),
     fnumchars(const char * go);

extern FONT * bigfont,
            * smallfont;

#endif
