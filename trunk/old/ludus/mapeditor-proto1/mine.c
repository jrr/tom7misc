/*         Ludus Map Editor
 *   
 *     By Seth McGann 1 and Tom Murphy 7
 *
 *   Version 0.[enter random number here]
 *     Revision: ////
 */

// Your job now is to make the map size redimensionable. I gave you a nice
// console to add these features. You should also make the map structure
// thingie work to reflect this.

// Clean up that Animation Guy Code (if any remains)
// Go to triple-buffering for the toolbar

// Important change: Save and Load are now 's' and 'l' keys. Scroll changed
// to ijkm, but it really should be arrow keys.

// And don't forget; comment!

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <allegro.h>

#define toolson 1

void get_cells(void);
void grab_pcx(void);
void grab_tools(void);
void grab_toolset(void);
void grab_tiles(void);
void draw_map(void);
void show_tiles(void);
void init_map(void);
void save_map(char * mapname);
void load_map(char * mapname);
void waitrelease(int button);
void parsekeycommand(void);
void parsemousecommand(void);
void flippage(void);
void drawtools(void);
void update_map(void);
void take_screen_shot(void);
void everything_init(void);
void fill_area(int,int,int,int,int,int);        // TM
int map(int which, int y, int x);              /// TM
int mapset(int,int,int,int valu);             //// TM
void console_on(void);
void cons_gets(char * dest);

enum { STANDARD, FILLTOP, FILLBOT }; // Drawing modes, constants.

BITMAP *tileset[301];
BITMAP *toolset[61];
BITMAP *animate[5];


PALLETE local_pallete;

char msg[80];
int layer1=1;
int layer2=1;
int layer3=1;
int current_page;
int oldmousex,oldmousey; //,toolson;
int dn,uf,die,xo,yo,xc,yc,mapx,mapy;
int i,sspeed,s,q,xcold,ycold,mxold,myold,t,lx,ly,lxo,lyo;
int goup,godown,goleft,goright;
int cellframe;
char * mpname;

int fx1,fy1,fx2,fy2,drawmode;

BITMAP *page1,*page2,*active_page,*view_page;
BITMAP *off_screen,*tool_bar;
MIDI *the_music;

int * map1;           /// TM
int * map2;          ///
int * map3;         /// TM
int maxx = 30, maxy = 30;          //  TM

// All of the module includes go here:

 #include "ludusio.h"
 #include "lm_init.h"
 #include "lm_draw.h"
 #include "lm_consl.h"

void waitrelease(int button)
	{
		do{} while (mouse_b && button);
	}

void main()
{
   // init
   everything_init(); // Moved out of sight! - TM7

   draw_map();
   readkey();
   remove_keyboard();
   remove_mouse();
   remove_timer();
   free(map1);
   free(map2);
   free(map3);
   exit(0);
}



void draw_map(void)
{
off_screen=create_bitmap(704,544);
tool_bar=create_bitmap(641,31);
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
the_music=load_midi("renewal.mid");
play_midi(the_music,TRUE);
uf=q=1;
xc=yc=32;
current_page=t=0;
die=lx=ly=lxo=lyo=0;

do {
parsemousecommand();
if (uf)
{
update_map();
blit_offscreen();
} // else {
if (goup) for(i=0;i<32;i++){yc--;update_map();blit_offscreen();}
if (godown) for(i=0;i<32;i++){yc++;update_map();blit_offscreen();}
if (goleft) for(i=0;i<32;i++){xc--;update_map();blit_offscreen();}
if (goright) for(i=0;i<32;i++){xc++;update_map();blit_offscreen();}
goup=godown=goleft=goright=0;
uf=1;
//}
}
while (!die);

destroy_midi(the_music);
destroy_bitmap(page1);
destroy_bitmap(page2);
destroy_bitmap(off_screen);
destroy_bitmap(tool_bar);
free(map1);
free(map2);
free(map3);
exit(0);
}

void parsekeycommand(void)
{
int c;
c=(readkey() & 0xFF);
switch (c)
{
case 'i':goup=1;break;
case 'm':godown=1;break;
case 'j':goleft=1;break;
case 'k':goright=1;break;
case 'e':s=0;break;
case 'f':drawmode=FILLTOP;break;
case 'p':take_screen_shot();break;
// case '\\':layer1=abs(layer1-1);goto rerun;break;
case '[':layer2=abs(layer2-1);break;
case ']':layer3=abs(layer3-1);break;
// case 't':toolson=abs(toolson-1);break;
case '+':
	sspeed++;
	if (sspeed>=32) sspeed=32;
	sprintf(msg,"+ Speed: %d ",sspeed);
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
case 's':
	sprintf(msg,"* Saving map...");
	textout(view_page,font, msg, 5,460, 255);
	save_map("map.dat");
	sprintf(msg,"* Done.");          
	textout(view_page,font, msg, 5,460, 255);
	break;
case 'l':
	sprintf(msg,"* Loading map...");          
	textout(view_page,font, msg, 5,460, 255);
	load_map("map.dat");
	sprintf(msg,"* Done.");          
	textout(view_page,font, msg, 5,460, 255);
	break;
case '6':init_map();break;
case '7':s=7;break;
case '8':s=8;break;
case '9':s=9;break;
case '`':
case '~':
// Pop down the console.
   console_on();
break;
case '!':
resize_map(40,40,0,0);
break;
case 'q':die++;break;
case 27:die++;break;
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

if (((oldmousex==mouse_x)&&(oldmousey==mouse_y))&&((ycold==yc)&&(xcold==xc)))
  {
    uf=0;   // No need to redraw (*** is it set uf=1 by default?)
  } else {  // Check Mouse Movement
//    sleep(1);
    if (toolson) {
        drawtools();
//        blittools();
    }

  }

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
	if (q==1) mapset(1,(mouse_y+yc)/32+mapy,(mouse_x+xc)/32+mapx,s);
	if (q==2) mapset(2,(mouse_y+yc)/32+mapy,(mouse_x+xc)/32+mapx,s);
	if (q==3) mapset(3,(mouse_y+yc)/32+mapy,(mouse_x+xc)/32+mapx,s);
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
if (yc>64) {
	yc-=32;
	mapy++;
   p=1;
   }
if (yc<0) {
	yc+=32;
	mapy--;
   p=1;
	}
if (xc>64) {
	xc-=32;
	mapx++;
   p=1;
	}
if (xc<0) {
	xc+=32;
	mapx--;
   p=1;
   }
if (mapx<0)	{
	mapx++;
	xc=0;
   p=1;
	}

if (mapy<0)	{
	mapy++;
	yc=0;
   p=1;
	}
if (mapx>(maxx-22)) {
	mapx--;
	xc=64;
   p=1;
	}
if (mapy>(maxy-17)) {
	mapy--;
	yc=64;
   p=1;
	}
if (p) prepare_offscreen();

}

void drawtools(void) {
//show_mouse(NULL);
clear(tool_bar);
if (s)
  blit(tileset[s],tool_bar,0,0,20,0,32,32);
else
  blit(toolset[1],tool_bar,0,0,20,0,32,32);

text_mode(13);
sprintf(msg,"XMap: %d YMap: %d %d %d",lx,ly,xc,yc);
textout(tool_bar,font, msg, 100, 5, 255);
blit(toolset[4],tool_bar,0,0,300,0,32,32);
blit(toolset[4],tool_bar,0,0,340,0,32,32);
blit(toolset[4],tool_bar,0,0,380,0,32,32);
blit(tileset[map(1,ly,lx)],tool_bar,0,0,300,0,32,32);
if (map(2,ly,lx)) blit(tileset[map(2,ly,lx)],tool_bar,0,0,340,0,32,32);
if (map(3,ly,lx)) blit(tileset[map(3,ly,lx)],tool_bar,0,0,380,0,32,32);

// Eyes

if (layer1)
    draw_sprite(tool_bar,toolset[2],300,0);
 else
    draw_sprite(tool_bar,toolset[3],300,0);

if (layer2)
    draw_sprite(tool_bar,toolset[2],340,0);
 else
    draw_sprite(tool_bar,toolset[3],340,0);

if (layer3)
    draw_sprite(tool_bar,toolset[2],380,0);
 else
    draw_sprite(tool_bar,toolset[3],380,0);

text_mode(-1);
sprintf(msg,"> Tool %d",s); 
textout(tool_bar,font, msg, 500,0, 255);
sprintf(msg,"> Layer: %d",q);          
textout(tool_bar,font, msg, 500,16, 255);
//show_mouse(screen);
blit(tool_bar,screen,0,0,0,450,640,30); // blow it all over at once

}

void fill_area(int x1,int y1, int x2, int y2, int layer, int value) {
// Fills an area of map Layer with Value.
int x,y;
for (y=y1;y<=y2;y++) {
for (x=x1;x<=x2;x++) {
switch (layer) {
case 1:
mapset(1,y,x, value);break;
case 2:
mapset(2,y,x, value);break;
case 3:
mapset(3,y,x, value);break;
default:
}
    }} // fors

}

// This does not do any bounds checking, and is not terribly fast! Should
// NOT be used for any routines that require any sort of speed.

int map(int which, int y, int x) {
switch (which) {
case 1:
return (map1[(y*maxx)+x]);
break;case 2:
return (map2[(y*maxx)+x]);
break;case 3:
return (map3[(y*maxx)+x]);
default: }
}
int mapset(int which, int y, int x,int valu) {
switch (which) {
case 1:
map1[(y*maxx)+x] = valu;
break;case 2:
map2[(y*maxx)+x] = valu;
break;case 3:
map3[(y*maxx)+x] = valu;
default: }
return (valu);
}


