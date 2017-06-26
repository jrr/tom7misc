/* [Our game engine name here] Map Editor
 *   
 *     By Seth Mcgann 1 and Tom Murphy 7
 *
 *          DJGPP C, using the Allegro Library. 
 *
 *   Version 0.[enter random number here]
 *     Revision: /
 */

// Note: Things added in this revision:
//     ù Hit [ and ] to turn off the 2nd and third layers
// Things for Seth to do:
//     ù Use the Eraser Graphic
//     ù Put the eye icons on the layers if they are being seen/invisible
//     ù make draw_map into a CALLABLE PROCEDURE! The entire program runs
//         out of it and it sucks. Get rid of the gotos, please?
//     ù Comment any code you add.

#include <stdlib.h>
#include <stdio.h>

#include "..\allegro.h"

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

/* Note to Seth:
 *     Obviously we are going to need different-sized maps. I recommend
 *     you change these to user definable arrays (C can do dynamically
 *     declared arrays, right?). For the temporary map file, also add
 *     a few bytes at the beginning to reflect this information.
 */

// By the way.... uh, comment your code? For the most part I can understand
// it, but I haven't done C in ages and it just makes it more difficult to
// understand when it's not commented. *ahem*

BITMAP *tileset[301];
BITMAP *toolset[50];
int map3[50][50];
int map2[50][50];
int map[50][50];
char msg[80];
int layer1=1;
int layer2=1;
int layer3=1;


// Look how cool I am... I indented everything all nicely...

void save_map(void)
{
	FILE *stream;
	int i,j;
	stream = fopen("map.dat","w");
	for(i=0;i<40;i++)
		{
		for(j=0;j<40;j++)
			fputc(map[i][j],stream);
		}
	for(i=0;i<40;i++)
		{
		for(j=0;j<40;j++)
			fputc(map2[i][j],stream);
		}
	for(i=0;i<40;i++)
		{
		for(j=0;j<40;j++)
			fputc(map3[i][j],stream);
		}
	fclose(stream);
}

void waitrelease(int button)
	{
		do{
		} while (mouse_b & button);
	}

void load_map(void)
	{

	FILE *stream;
	int i,j;
	stream = fopen("map.dat","r");
	for(i=0;i<40;i++)
		{
		for(j=0;j<40;j++)
			map[i][j]=fgetc(stream);
		}
	for(i=0;i<40;i++)
		{
		for(j=0;j<40;j++)
			map2[i][j]=fgetc(stream);
		}
	for(i=0;i<40;i++)
		{
		for(j=0;j<40;j++)
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
   set_gfx_mode(GFX_AUTODETECT, 640, 480, 0, 940);
   grab_tools();
   grab_toolset();
   
   grab_pcx();
   grab_tiles();
   tileset[0]=toolset[4];
   init_map();
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
	for (i=0;i<50;i++)
		{
		for (j=0;j<50;j++)
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
	for(i=0;i<50;i++)
		toolset[i] = create_bitmap(32,32);
	xs=0;
	ys=0;
	d=1;
	for(j=0;j<1;j++)
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


void draw_map(void)
{
int xo,yo,xc,yc,mapx,mapy;
int sspeed,c,s,xcold,ycold,mxold,myold,t,q,oq,lx,ly,lxo,lyo;
BITMAP *page1,*page2,*active_page,*view_page;
MIDI *the_music;
page1=create_sub_bitmap(screen,0,0,SCREEN_W,SCREEN_H);
page2=create_sub_bitmap(screen,0,SCREEN_H,SCREEN_W,SCREEN_H);
view_page=page1;
active_page=page2;
text_mode(-1);
s=1;
q=1;
mapx=5;
mapy=5;
sspeed=8;
install_sound(DIGI_NONE,MIDI_AUTODETECT,NULL);
the_music=load_midi("urgency.mid");
play_midi(the_music,TRUE);
xc=yc=t=0;
lx=ly=lxo=lyo=0;
rerun:

if (yc>32)
	{
	yc-=32;
	mapy++;
	}
if (yc<(-32))
	{
	yc+=32;
	mapy--;
	}
if (xc>32)
	{
	xc-=32;
	mapx++;
	}
if (xc<(-32))
	{
	xc+=32;
	mapx--;
	}
if (mapx<0)
	{
	mapx=0;
	xc=(-32);
	}
if (mapy<0)
	{
	mapy=0;
	yc=(-32);
	}
if (mapx>18)
	{
	mapx=18;
	xc=32;
	}
if (mapy>23)
	{
	mapy=23;
	yc=32;
	}
show_mouse(NULL);

 // Take constant math OUT of the INNER LOOP please. ;)

for (yo=0;yo<17;yo++)
	for (xo=0;xo<22;xo++)
	{
	if (layer1==1)
	blit(tileset[map[mapy+yo][mapx+xo]],active_page,0,0,(xo-1)*32-xc,(yo-1)*32-yc,32,32);
	if (layer2==1) {
	if (map2[mapy+yo][mapx+xo])
		draw_sprite(active_page,tileset[map2[mapy+yo][mapx+xo]],(xo-1)*32-xc,(yo-1)*32-yc);
			}
	if (layer3==1) {
	if (map3[mapy+yo][mapx+xo])
		draw_sprite(active_page,tileset[map3[mapy+yo][mapx+xo]],(xo-1)*32-xc,(yo-1)*32-yc);
			}
	}
if (active_page==page1)
	{
	scroll_screen(0,0);
	active_page = page2;
	view_page = page1;
	show_mouse(screen);
	}
else
	{
	scroll_screen(0,SCREEN_H);
	active_page = page1;
	show_mouse(page2);
	view_page = page2;
	}
xcold=xc;
ycold=yc;

rectfill(view_page,0,440,639,479,3);
if (s>0) {
blit(tileset[s],view_page,0,0,600,445,32,32);
} else {
blit(toolset[1],view_page,0,0,600,445,32,32);
}
sprintf(msg,"> Tool %d",s); 
textout(view_page,font, msg, 500,450, 255);
sprintf(msg,"> Layer: %d",q);          
textout(view_page,font, msg, 500,458, 255);


do{
/*
if (abs(mxold-mouse_x)>16 || abs(myold-mouse_y)>16)
{
show_mouse(NULL);
rect(view_page,(mouse_x+xc)/32*32-xc,(mouse_y+yc)/32*32-yc,
               (mouse_x+xc)/32*32+32-xc,(mouse_y+yc)/32*32+32-yc,15);
show_mouse(view_page);
mxold=mouse_x;
myold=mouse_y;
}
*/
if (mouse_x>=638) xc+=sspeed;          // Seth: I changed the bounds slightly
if (mouse_x<=1) xc-=sspeed;            // on these, just so the <= would make
if (mouse_y>=478) yc+=sspeed;          // sense. ;) It also reacts much more
if (mouse_y<=1) yc-=sspeed;            // nicely in the corners now.

if (mouse_b & 1) 
	{
	if ((q==1)) map[(mouse_y+yc+32)/32+mapy][(mouse_x+xc+32)/32+mapx]=s;
	if ((q==2)) map2[(mouse_y+yc+32)/32+mapy][(mouse_x+xc+32)/32+mapx]=s;
	if ((q==3)) map3[(mouse_y+yc+32)/32+mapy][(mouse_x+xc+32)/32+mapx]=s;
	//t--;
	goto rerun;         // WHAT!!? A *GOTO*???
}

if (mouse_b & 2)
	{
	show_mouse(NULL);
	scroll_screen(0,0);
	show_tiles();
	show_mouse(screen);
		waitrelease(2);
		do
		{
		if (mouse_b & 1) 
			{
			do {
			s=(mouse_y/32*20)+mouse_x/32+1;
//			t=5;
			} while (mouse_b & 1);
			goto rerun;
			}
/*		if (mouse_b & 2)
			waitrelease(2);
			goto rerun;
*/
		if (keypressed())
		c=(readkey() & 0xFF);
		switch (c) {
			case 27:goto rerun;break;
			default:
			}
		} while (1);
	}

if (keypressed())
{
c=(readkey() & 0xFF);        // I take it back.  I thought you were making
switch (c)                  // it lowercase.
{
case 'w':yc+=sspeed;break;
case 'z':yc-=sspeed;break;
case 'a':xc+=sspeed;break;
case 's':xc-=sspeed;break;
case 'e':s=0;break;
//case '\\':layer1=abs(layer1-1);goto rerun;break;
case '[':layer2=abs(layer2-1);goto rerun;break;
case ']':layer3=abs(layer3-1);goto rerun;break;
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
case '1':q=1;break;
case '2':q=2;break;
case '3':q=3;break;
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
case 'q':goto geto;break;
case 27:goto geto;break;            // Tom's addition - now you can leave
				    // with the escape key...
default:
// do nothing
}

if (q!=oq) {
	sprintf(msg,"> Layer: %d ",q);
	textout(view_page,font, msg, 5,460, 255);
	oq=q;
}

}
lx=(mouse_x+xc+32)/32+mapx;
ly=(mouse_y+yc+32)/32+mapy;
if (! ( (lx==lxo) && (ly==lyo) ) )
{
show_mouse(NULL);
text_mode(13);
sprintf(msg,"XMap: %d YMap: %d ",lx,ly);          
textout(view_page,font, msg, 100,460, 255);
sprintf(msg,"Blnk",lx,ly);          
rectfill(view_page,340,445,371,476,255);
rectfill(view_page,300,445,331,476,255);
rectfill(view_page,380,445,411,476,255);
textout(view_page,font, msg, 340,450, 255);
textout(view_page,font, msg, 380,450 , 255);

blit(tileset[map[ly][lx]],view_page,0,0,300,445,32,32);
if (map2[ly][lx]) blit(tileset[map2[ly][lx]],view_page,0,0,340,445,32,32);
if (map3[ly][lx]) blit(tileset[map3[ly][lx]],view_page,0,0,380,445,32,32);
text_mode(-1);
lxo=lx;
lyo=ly;
show_mouse(view_page);
}

oq=q;


}while ((xcold==xc) && (ycold==yc));
goto rerun;
geto:
destroy_midi(the_music);
destroy_bitmap(page1);
destroy_bitmap(page2);
exit(0);                           // Another minor Tom adjustment...
				   // Now the program actually exits!!
}
