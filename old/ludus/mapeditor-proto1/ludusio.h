#ifndef __ludus_map_io_
#define __ludus_map_io_

// Ludus map IO - screen shots, load/save.

extern int maxx,maxy;

void take_screen_shot(void)
{
BITMAP *bmp;
      PALETTE pal;

      get_palette(pal);
      bmp = create_sub_bitmap(screen, 0, 0, SCREEN_W, SCREEN_H);
      save_bitmap("screenst.pcx", bmp, pal);
      destroy_bitmap(bmp);
}


void save_map(char * mapname)   // This routine looks SO cool now.
{
	FILE *stream;
	int i,j,p;
	stream = fopen("map.dat","w");
for(p=1;p<=3;p++)                   // different map planes. Actually more
	for(i=0;i<maxy;i++) for(j=0;j<maxx;j++) fputc(map(p,i,j),stream);
//	for(i=0;i<maxy;i++) for(j=0;j<maxx;j++) fputc(map(p,i,j),stream);
//	for(i=0;i<maxy;i++) for(j=0;j<maxx;j++) fputc(map(p,i,j),stream);
	fclose(stream);
}

void load_map(char * mapname)
	{
	FILE *stream;
	int i,j,p;
	stream = fopen(mapname,"r");
for(p=1;p<=3;p++)
	for(i=0;i<maxy;i++)
      for(j=0;j<maxx;j++) mapset(p,i,j,fgetc(stream));
//   for(i=0;i<maxy;i++)
 //     for(j=0;j<maxx;j++) mapset(p,i,j,fgetc(stream));
//	for(i=0;i<maxy;i++)
//   	for(j=0;j<maxx;j++) mapset(p,i,j,fgetc(stream));
	fclose(stream);
}

#endif
