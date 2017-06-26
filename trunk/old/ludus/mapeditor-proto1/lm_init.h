#ifndef __ludus_map_init_
#define __ludus_map_init_

// Init for the Ludus Map Editor.
extern PALLETE local_pallete;
void everything_init(void) {
   allegro_init();
   install_keyboard(); 
   install_timer();
   install_mouse();

   if (set_gfx_mode(GFX_AUTODETECT, 640, 480,0,0))
   {printf("Looks like I can't find a compatible SVGA Card - Sorry!\r\n");
   printf("Allegro reports: %s\r\n ",allegro_error);
   exit(1);
   };

   // alloc some m!

   map1 = (int *) malloc (maxx * maxy * sizeof(int));
   map2 = (int *) malloc (maxx * maxy * sizeof(int));
   map3 = (int *) malloc (maxx * maxy * sizeof(int));

   if (!((* map1) && (* map2) && (* map3))) {
      free(map1);
      free(map2);
      free(map3);
      printf("Not enough memory for map. Sowwy!\r\n");
      exit(-1);
   }
   show_mouse(NULL);
   grab_tools();
   grab_toolset();
   get_cells();
   grab_pcx();
   grab_tiles();
   cellframe=1;
   tileset[0]=toolset[4];
   init_map();

   set_mouse_sprite(toolset[43]);
   set_mouse_range(0,0,640,440);
}

void resize_map(int nmaxx,int nmaxy, int xoffset, int yoffset) {
int * nmap1;
int * nmap2;
int * nmap3;
int xs,ys,taxx,taxy;
   nmap1 = (int *) malloc (nmaxx * nmaxy * sizeof(int));
   nmap2 = (int *) malloc (nmaxx * nmaxy * sizeof(int));
   nmap3 = (int *) malloc (nmaxx * nmaxy * sizeof(int));
   for (ys=0;ys<nmaxy;ys++)
   for (xs=0;xs<nmaxx;xs++)
   {
   nmap1[(ys*nmaxx)+xs]=3;
   nmap2[(ys*nmaxx)+xs]=0;
   nmap3[(ys*nmaxx)+xs]=0;
   }
   if (nmaxx<maxx)
   {taxx=nmaxx;}
   else
   {taxx=maxx;}
   if (nmaxy<maxy)
   {taxy=nmaxy;}
   else
   {taxx=maxy;}
   for (ys=0;ys<taxy;ys++)
   for (xs=0;xs<taxx;xs++)
   {
   nmap1[((ys+yoffset)*nmaxx)+xs+xoffset]=map(1,ys,xs);
   nmap2[((ys+yoffset)*nmaxx)+xs+xoffset]=map(2,ys,xs);
   nmap3[((ys+yoffset)*nmaxx)+xs+xoffset]=map(3,ys,xs);
   }

   free(map1);
   free(map2);
   free(map3);
   map1=nmap1;
   map2=nmap2;
   map3=nmap3;
   maxx=nmaxx;
   maxy=nmaxy;
}

void init_map(void) {
	int i,j;
	for (i=0;i<maxy;i++)
      for (j=0;j<maxx;j++)	{
			mapset(1,i,j,3);              // We should change the init tile.
			mapset(2,i,j,0);
         mapset(3,i,j,0);
		}
	}

void grab_tiles(void) {
	int d,j,i,xs,ys;
	for(i=0;i<301;i++) tileset[i] = create_bitmap(32,32);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<15;j++)	{
		for(i=0;i<20;i++)	{
			blit(screen,tileset[d],xs,ys,0,0,32,32);
			xs+=32;
			d++;
			}
		xs=0;
		ys+=32;
		}
	}

void get_cells(void) {
	int d,j,i,xs,ys;
   PALLETE the_pallete;
   BITMAP *the_image;
   the_image = load_pcx("smdanm.pcx", the_pallete);
   set_pallete(the_pallete);
   blit(the_image, screen, 16,16,0,0,640,480);

   destroy_bitmap(the_image);

	for(i=0;i<6;i++) animate[i] = create_bitmap(32,64);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<1;j++) {
		for(i=0;i<2;i++) {
			blit(screen,animate[d],xs,ys,0,0,32,64);
			xs+=32;
			d++;
			}
		xs=0;
		ys+=32;
		}
}

void grab_pcx(void) {
   // PALLETE the_pallete;
   BITMAP *the_image;
   the_image = load_pcx("alltiles.pcx", local_pallete);
   set_pallete(local_pallete);
//   local_pallete = the_pallete;
   blit(the_image, screen, 16,16, (SCREEN_W-the_image->w)/2, 
       (SCREEN_H-the_image->h)/2, the_image->w, the_image->h);
   destroy_bitmap(the_image);
}

void grab_tools(void) {
   PALLETE the_pallete;
   BITMAP *the_image;
   the_image = load_pcx("medtiles.pcx", the_pallete);
   set_pallete(the_pallete);
   blit(the_image, screen, 16,48, (SCREEN_W-the_image->w)/2, 
       (SCREEN_H-the_image->h)/2, the_image->w, the_image->h);
   destroy_bitmap(the_image);
}

void grab_toolset(void)	{
	int d,j,i,xs,ys;
	for(i=0;i<60;i++)	toolset[i] = create_bitmap(32,32);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<3;j++) {
		for(i=0;i<20;i++)	{
			blit(screen,toolset[d],xs,ys,0,0,32,32);
			xs+=32;
			d++;
			}
		xs=0;
		ys+=32;
		}
	}

#endif
