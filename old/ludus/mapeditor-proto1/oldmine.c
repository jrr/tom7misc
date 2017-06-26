/*         Ludus Map Editor
 *   
 *     By Seth Mcgann 1 and Tom Murphy 7
 *
 *   Version 0.[enter random number here]
 *     Revision: //
 */

// For you to do: You can get rid of the guy in the top left, and clean
// up the code a bit. Let's get serious with the map editor.

// - Change the bracket notation to use malloc()'d array space with our
// own dimensions.

// I'd also like to be able to simultaneously work on features in my own
// module. I sort-of know how to do this, but I don't know what variables
// are critical and which are going to be changed when we move from the
// static-size arrays. After that happens, we can put some of this stuff
// in a header file and add another module.

// And don't forget; comment!

#include <stdlib.h>
#include <stdio.h>

#include <allegro.h>

void get_cells(void);
void grab_pcx(void);
void grab_tools(void);
void grab_toolset(void);
void grab_tiles(void);
void draw_map(void);
void show_tiles(void);
void init_map(void);
void save_map(void);
void load_map(void);
void waitrelease(int button);
void parsekeycommand(void);
void parsemousecommand(void);
void flippage(void);
void drawtools(void);
void update_map(void);
void take_screen_shot(void);
void fill_area(int,int,int,int,int,int);

enum { STANDARD, FILLTOP, FILLBOT }; // Drawing modes, constants.

BITMAP *tileset[301];
BITMAP *toolset[61];
BITMAP *animate[5];

#define MAXX 100
#define MAXY 100
int map3[MAXY][MAXX];
int map2[MAXY][MAXX];
int map[MAXY][MAXX];

char msg[80];
int layer1=1;
int layer2=1;
int layer3=1;
int current_page;
int oldmousex,oldmousey,toolson;
int dn,uf,die,xo,yo,xc,yc,mapx,mapy;
int i,sspeed,s,q,xcold,ycold,mxold,myold,t,lx,ly,lxo,lyo;
int goup,godown,goleft,goright;
int cellframe;

int fx1,fy1,fx2,fy2,drawmode;

BITMAP *page1,*page2,*active_page,*view_page;
BITMAP *off_screen;
MIDI *the_music;

void take_screen_shot(void)
{
BITMAP *bmp;
      PALETTE pal;

      get_palette(pal);
      bmp = create_sub_bitmap(screen, 0, 0, SCREEN_W, SCREEN_H);
      save_bitmap("screenst.pcx", bmp, pal);
      destroy_bitmap(bmp);
}


void save_map(void)
{
	FILE *stream;
	int i,j;
	stream = fopen("map.dat","w");
	for(i=0;i<MAXY;i++)
		{
		for(j=0;j<MAXX;j++)
			fputc(map[i][j],stream);
		}
	for(i=0;i<MAXY;i++)
		{
		for(j=0;j<MAXX;j++)
			fputc(map2[i][j],stream);
		}
	for(i=0;i<MAXY;i++)
		{
		for(j=0;j<MAXX;j++)
			fputc(map3[i][j],stream);
		}
	fclose(stream);
}

void waitrelease(int button)
	{
		do{
		} while (mouse_b && button);
	}

void load_map(void)
	{

	FILE *stream;
	int i,j;
	stream = fopen("map.dat","r");
	for(i=0;i<MAXY;i++)
		{
		for(j=0;j<MAXX;j++)
			map[i][j]=fgetc(stream);
		}
	for(i=0;i<MAXY;i++)
		{
		for(j=0;j<MAXX;j++)
			map2[i][j]=fgetc(stream);
		}
	for(i=0;i<MAXY;i++)
		{
		for(j=0;j<MAXX;j++)
			map3[i][j]=fgetc(stream);
		}
	
	fclose(stream);
}


void main()
{
   // init

   allegro_init();
   install_keyboard(); 
   install_timer();
   install_mouse();


   if (set_gfx_mode(GFX_AUTODETECT, 640, 480,0,0))
   {printf("Looks like I can't find a compatible SVGA Card - Sorry!\r\n");
   exit(1);
   };

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
   draw_map();
   readkey();
   remove_keyboard();
   remove_mouse();
   remove_timer();
   exit(0);
}

void init_map(void)
	{
	int i,j;
	for (i=0;i<MAXY;i++)
		{
		for (j=0;j<MAXX;j++)
		{
			map[i][j]=3;
			map2[i][j]=map3[i][j]=0;
		}
		}
	}

void show_tiles(void)
	{
	int i,j,d;
	d=1;
	for (j=0;j<15;j++)
		{
		for (i=0;i<20;i++)
			{
			blit (tileset[d],screen,0,0,i*32,j*32,32,32);
			d++;
			}

		}
	}

void grab_tiles(void)
	{
	int d,j,i,xs,ys;
	for(i=0;i<301;i++)
		tileset[i] = create_bitmap(32,32);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<15;j++)
		{
		for(i=0;i<20;i++)
			{
			blit(screen,tileset[d],xs,ys,0,0,32,32);
			xs+=32;
			d++;
			}
		xs=0;
		ys+=32;
		}
	}

void get_cells(void)
{
	int d,j,i,xs,ys;
PALLETE the_pallete;
   BITMAP *the_image;
   the_image = load_pcx("smdanm.pcx", the_pallete);
   set_pallete(the_pallete);
   blit(the_image, screen, 16,16,0,0,640,480);

   destroy_bitmap(the_image);

	for(i=0;i<6;i++)
		animate[i] = create_bitmap(32,64);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<1;j++)
		{
		for(i=0;i<2;i++)
			{
			blit(screen,animate[d],xs,ys,0,0,32,64);
			xs+=32;
			d++;
			}
		xs=0;
		ys+=32;
		}

}

void grab_pcx(void)
{
   PALLETE the_pallete;
   BITMAP *the_image;
   the_image = load_pcx("alltiles.pcx", the_pallete);
   set_pallete(the_pallete);
   blit(the_image, screen, 16,16, (SCREEN_W-the_image->w)/2, 
       (SCREEN_H-the_image->h)/2, the_image->w, the_image->h);
   destroy_bitmap(the_image);
}

void grab_tools(void)
{
   PALLETE the_pallete;
   BITMAP *the_image;
   the_image = load_pcx("medtiles.pcx", the_pallete);
   set_pallete(the_pallete);
   blit(the_image, screen, 16,48, (SCREEN_W-the_image->w)/2, 
       (SCREEN_H-the_image->h)/2, the_image->w, the_image->h);
   destroy_bitmap(the_image);
}

void grab_toolset(void)
	{
	int d,j,i,xs,ys;
	for(i=0;i<60;i++)
		toolset[i] = create_bitmap(32,32);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<3;j++)
		{
		for(i=0;i<20;i++)
			{
			blit(screen,toolset[d],xs,ys,0,0,32,32);
			xs+=32;
			d++;
			}
		xs=0;
		ys+=32;
		}
	}

void prepare_offscreen (void)
{
int oxo,oyo;
for (oyo=0;oyo<17;oyo++)
	for (oxo=0;oxo<22;oxo++)
	{
   if (layer1) blit(tileset[map[mapy+oyo][mapx+oxo]],off_screen,0,0,oxo*32,oyo*32,32,32);
	if (map2[mapy+oyo][mapx+oxo] && layer2) draw_sprite(off_screen,tileset[map2[mapy+oyo][mapx+oxo]],oxo*32,oyo*32);
   if (map3[mapy+oyo][mapx+oxo] && layer3) draw_sprite(off_screen,tileset[map3[mapy+oyo][mapx+oxo]],oxo*32,oyo*32);
   }
//   draw_sprite(off_screen,animate[cellframe],32,32);
}
void blit_offscreen (void)
{
//vsync();
show_mouse(NULL);
blit(off_screen,screen,xc,yc,0,0,640,480);
show_mouse(screen);
}

void draw_map(void)
{
off_screen=create_bitmap(704,544);
page1=create_sub_bitmap(screen,0,0,SCREEN_W,SCREEN_H);
page2=create_sub_bitmap(screen,0,SCREEN_H,SCREEN_W,SCREEN_H);
view_page=screen;
active_page=screen;
text_mode(-1);
s=1;
mapx=10;
mapy=10;
sspeed=1;
prepare_offscreen();
install_sound(DIGI_NONE,MIDI_AUTODETECT,NULL);
the_music=load_midi("annual.mid");
play_midi(the_music,TRUE);
uf=q=1;
xc=yc=32;
current_page=t=0;
die=lx=ly=lxo=lyo=0;
do {
parsemousecommand();
switch (cellframe) {
case 1:
cellframe = 2;break;
case 2:
cellframe = 1;break;
default:
}
if (uf)
{
update_map();
blit_offscreen();
if (toolson) drawtools();
}
if (goup) for(i=0;i<32;i++){yc--;update_map();blit_offscreen();}
if (godown) for(i=0;i<32;i++){yc++;update_map();blit_offscreen();}
if (goleft) for(i=0;i<32;i++){xc--;update_map();blit_offscreen();}
if (goright) for(i=0;i<32;i++){xc++;update_map();blit_offscreen();}
goup=godown=goleft=goright=0;
uf=1;


//if (cellframe==1) cellframe=2;
//if (cellframe==2)


}
while (!die);

destroy_midi(the_music);
destroy_bitmap(page1);
destroy_bitmap(page2);
exit(0);                           // Another minor Tom adjustment...
			   // Now the program actually exits!!
}

void parsekeycommand(void)
{
int c;
c=(readkey() & 0xFF);
switch (c)
{
case 'w':goup=1;break;
case 'z':godown=1;break;
case 'a':goleft=1;break;
case 's':goright=1;break;
case 'e':s=0;break;
case 'f':drawmode=FILLTOP;break;
case 'p':take_screen_shot();break;
// case '\\':layer1=abs(layer1-1);goto rerun;break;
case '[':layer2=abs(layer2-1);break;
case ']':layer3=abs(layer3-1);break;
case 't':toolson=abs(toolson-1);break;
case '+':                   // Tom additions; Now it:
	sspeed++;                          // - Displays current scroll speed
	if (sspeed>=32) sspeed=32;            // - Clips upper and lower
	sprintf(msg,"+ Speed: %d ",sspeed);            // (0<=sspeed<=32)
	textout(view_page,font, msg, 5,460, 255);
	break;
case '-':
	sspeed--;
	if (sspeed<=0) sspeed=0;
	sprintf(msg,"- Speed: %d ",sspeed);
	textout(view_page,font, msg, 5,460, 255);
	break;
case '1':set_mouse_sprite(toolset[43]);q=1;break;
case '2':set_mouse_sprite(toolset[44]);q=2;break;
case '3':set_mouse_sprite(toolset[45]);q=3;break;
case '5':
	sprintf(msg,"* Saving map...");          
	textout(view_page,font, msg, 5,460, 255);
	save_map();
	sprintf(msg,"* Done.");          
	textout(view_page,font, msg, 5,460, 255);
	break;
case '4':
	sprintf(msg,"* Loading map...");          
	textout(view_page,font, msg, 5,460, 255);
	load_map();
	sprintf(msg,"* Done.");          
	textout(view_page,font, msg, 5,460, 255);
	break;
case '6':init_map();break;
case '7':s=7;break;
case '8':s=8;break;
case '9':s=9;break;
case 'q':die++;break;
case 27:die++;break;            // Tom's addition - now you can leave
				    // with the escape key...
default:
// do nothing
}
}
void parsemousecommand()
{
xcold=xc;
ycold=yc;
oldmousex=mouse_x;
oldmousey=mouse_y;

if (mouse_x>=638) xc+=sspeed;          // Seth: I changed the bounds slightly
if (mouse_x<=1) xc-=sspeed;            // on these, just so the <= would make
if (mouse_y>=438) yc+=sspeed;          // sense. ;) It also reacts much more
if (mouse_y<=1) yc-=sspeed;            // nicely in the corners now.

if (((oldmousex==mouse_x)&&(oldmousey==mouse_y))&&((ycold==yc)&&(xcold==xc)))uf=0;

if (mouse_b & 2)
	{
	uf=1;
	show_mouse(NULL);
	scroll_screen(0,0);
	show_tiles();
	show_mouse(screen);
		waitrelease(2);
      dn=1;
		do
		{
		if (mouse_b & 1)
      {
		s=(mouse_y/32*20)+(mouse_x/32+1);
      dn=0;
		}
		} while (dn); //while (!(mouse_b & 1));
	waitrelease(1);
}

if (mouse_b & 1) 
   {
   switch (drawmode) {
   case FILLTOP:
   waitrelease(1);
   fx1 = (mouse_x+xc)/32 + mapx;
   fy1 = (mouse_y+yc)/32 + mapx;
   drawmode = FILLBOT;
   break;
   case FILLBOT:
   fx2 = (mouse_x+xc)/32 + mapx;
   fy2 = (mouse_y+yc)/32 + mapy;
   uf=1;
   fill_area(fx1,fy1,fx2,fy2,q,s);
   drawmode = STANDARD;
   break;
   case STANDARD:  // standard draw
   default:
   uf=1;
	if ((q==1)) map[(mouse_y+yc)/32+mapy][(mouse_x+xc)/32+mapx]=s;
	if ((q==2)) map2[(mouse_y+yc)/32+mapy][(mouse_x+xc)/32+mapx]=s;
	if ((q==3)) map3[(mouse_y+yc)/32+mapy][(mouse_x+xc)/32+mapx]=s;
   }
	prepare_offscreen();

   }



if (keypressed()) parsekeycommand();


lx=(mouse_x+xc)/32+mapx;
ly=(mouse_y+yc)/32+mapy;
/*
if (! ( (lx==lxo) && (ly==lyo) ) )
{
lxo=lx;
lyo=ly;
}
*/

}


void update_map(void)
{
int p;
p=0;
if (yc>64)
	{
	yc-=32;
	mapy++;
   p=1;
   }
if (yc<0)
	{
	yc+=32;
	mapy--;
   p=1;
	}
if (xc>64)
	{
	xc-=32;
	mapx++;
   p=1;
	}
if (xc<0)
	{
	xc+=32;
	mapx--;
   p=1;
   }
if (mapx<0)
	{
	mapx++;
	xc=0;
   p=1;
	}

if (mapy<0)
	{
	mapy++;
	yc=0;
   p=1;
	}
if (mapx>(MAXX-22))
	{
	mapx--;
	xc=64;
   p=1;
	}
if (mapy>(MAXY-18))
	{
	mapy--;
	yc=64;
   p=1;
	}
if (p) prepare_offscreen();

   // Take constant math OUT of the INNER LOOP please. ;)
}

void drawtools(void)
{
show_mouse(NULL);
if (s>0)
{
blit(tileset[s],active_page,0,0,600,445,32,32);
}
else 
{
blit(toolset[1],active_page,0,0,600,445,32,32);
}

text_mode(13);
sprintf(msg,"XMap: %d YMap: %d %d %d",lx,ly,xc,yc);
textout(active_page,font, msg, 100,460, 40);
blit(toolset[4],active_page,0,0,340,445,32,32);
blit(toolset[4],active_page,0,0,300,445,32,32);
blit(toolset[4],active_page,0,0,380,445,32,32);
blit(tileset[map[ly][lx]],active_page,0,0,300,445,32,32);
if (map2[ly][lx]) blit(tileset[map2[ly][lx]],active_page,0,0,340,445,32,32);
if (map3[ly][lx]) blit(tileset[map3[ly][lx]],active_page,0,0,380,445,32,32);

if (layer1)
{
draw_sprite(active_page,toolset[2],300,445);
}
else
{
draw_sprite(active_page,toolset[3],300,445);
}  // optomize me

if (layer2)
{
draw_sprite(active_page,toolset[2],340,445);
}
else
{
draw_sprite(active_page,toolset[3],340,445);
}  // optomize me

if (layer3)
{
draw_sprite(active_page,toolset[2],380,445);
}
else
{
draw_sprite(active_page,toolset[3],380,445);
}  // optomize me
text_mode(-1);
sprintf(msg,"> Tool %d",s); 
textout(active_page,font, msg, 500,450, 255);
sprintf(msg,"> Layer: %d",q);          
textout(active_page,font, msg, 500,458, 255);
show_mouse(screen);
}

void fill_area(int x1,int y1, int x2, int y2, int layer, int value) {
// Fills an area of map Layer with Value.
int x,y;
for (y=y1;y<=y2;y++) {
for (x=x1;x<=x2;x++) {
switch (layer) {
case 1:
map[y][x] = value;break;
case 2:
map2[y][x] = value;break;
case 3:
map3[y][x] = value;break;
default:
}
    }} // fors

}
