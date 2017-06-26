
/* Additions to color.c from the allegro library. */

/* *** TOM 7 added this
 * 
 * This is for the shortened transparency table, which only sets up a table
 * between the first num colors and the entire palette.
 *
 * Ludus will use a constant palette of 32 or 64 colors, and the rest will
 * change with the map -- since the palette can be changing for each new
 * scene, we want this to be as fast as possible.
 *
 *
 * Note that color 0 always remains transparent.
*/

void create_short_trans_table(COLOR_MAP *table, PALLETE pal, int num, void (*callback)(int pos))
{
   int x, y;
   RGB c;

   for (y=0; y<PAL_SIZE; y++)
      table->data[0][y] = y;

   if (callback)(*callback)(0);

   for (x=1; x<num; x++) {
      for (y=1; y<=PAL_SIZE; y++) {

      c.r = (((int)pal[x].r + pal[y].r)>>1);
      c.g = (((int)pal[x].g + pal[y].g)>>1);
      c.b = (((int)pal[x].b + pal[y].b)>>1);

         /* This will be symmetric, ignoring the old r g b values */
/*         table->data[y][x] = */
         table->data[x][y] = bestfit_color(pal, pal[x].r, pal.g, c.b);

      if (callback)(*callback)(x);
   }
}
/* *** TOM 7 added this
 * 
 * This is for the shortened transparency table, which only sets up a table
 * between the first num colors and the entire palette.
 *
 * Ludus will use a constant palette of 32 or 64 colors, and the rest will
 * change with the map -- since the palette can be changing for each new
 * scene, we want this to be as fast as possible.
 *
 *
 * Note that color 0 always remains transparent.
*/

void create_short_trans_table(COLOR_MAP *table, PALLETE pal, int num, void (*callback)(int pos))
{
   int x, y;
   RGB c;

   for (y=0; y<PAL_SIZE; y++)
      table->data[0][y] = y;

   if (callback)(*callback)(0);

   for (x=1; x<num; x++) {
      for (y=1; y<=PAL_SIZE; y++) {

      c.r = (((int)pal[x].r + pal[y].r)>>1);
      c.g = (((int)pal[x].g + pal[y].g)>>1);
      c.b = (((int)pal[x].b + pal[y].b)>>1);

         /* This will be symmetric, ignoring the old r g b values */
/*         table->data[y][x] = */
         table->data[x][y] = bestfit_color(pal, pal[x].r, pal.g, c.b);

      if (callback)(*callback)(x);
   }
}
