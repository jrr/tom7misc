#ifndef __ludus_map_draw_
#define __ludus_map_draw_


void show_tiles(void)
	{
	int i,j,d;
	d=1;
	for (j=0;j<15;j++)
		for (i=0;i<20;i++)
			blit (tileset[d++],screen,0,0,i*32,j*32,32,32);
   }

void prepare_offscreen (void)
{
int oxo,oyo;
for (oyo=0;oyo<16;oyo++)
	for (oxo=0;oxo<22;oxo++)
	{
   if (layer1) blit(tileset[map(1,mapy+oyo,mapx+oxo)],off_screen,0,0,oxo*32,oyo*32,32,32);
	if (map(2,mapy+oyo,mapx+oxo) && layer2) draw_sprite(off_screen,tileset[map(2,mapy+oyo,mapx+oxo)],oxo*32,oyo*32);
   if (map(3,mapy+oyo,mapx+oxo) && layer3) draw_sprite(off_screen,tileset[map(3,mapy+oyo,mapx+oxo)],oxo*32,oyo*32);
   }
//   draw_sprite(off_screen,animate[cellframe],32,32);
if (toolson) drawtools();
}

void blit_offscreen (void)
{
//vsync();
show_mouse(NULL);
blit(off_screen,screen,xc,yc,0,0,640,448);
show_mouse(screen);
}



#endif
