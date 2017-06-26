Date: Sat, 15 Feb 1997 22:59:25 -0500
From: Seth McGann <tashego@sprynet.com>
Reply-To: tashego@sprynet.com
Organization: Eli Whitney Museum
To: Tom 7 <propello@m6.sprynet.com>
Subject: Re: DD & alltiles

Tom 7 wrote:
> 
> At 03:26 PM 2/15/97 -0500, you wrote:
> >Try the Sci Tech 5.3 (not 5.3a).. It looks older.  Also get the S3 VBE TSR.
> > Seth 1
> 
> I got it. It seems exactly the same except that it is registered now. A
> nice util! Speeds up my video with the linear framebuffer about 150%.
> 
> The map editor works Really Nicely with Vesa 2.0. I made some nice maps and
> screenshots -- map saving and loading is still pretty "quirky".
> 
> Attached is the newest "alltiles" -- there are a few new tiles (toilet,
> beds, some stolen from Escape, bird poop, etc.), a couple of adjustments
> (the outlines [top right corner] have been changed to transparent. I think
> I added a fully black tile to go behind these.)
> 
> When you have finished with your try at the code, send it to me; there are
> a number of changes I want to make.
> 
> All in all, though, everything is looking nice! I will continue working on
> graphics, music (I have a lot more that you don't have, and some of the
> previously very-short songs have come to be more full-fledged pieces now --
> I almost have enough tunes to cover the whole game!), animation, the web
> page, etc. I will also do some thinking about the scripting language and
> perhaps some prototypes in QB. Good luck with the video problems if you
> haven't solved them yet. =)
> 
> Note: Eudora is pretty cool! I think I may use it instead of NutScrape in
> the future.
> 
> I don't remember -- is it possible to make zip files with pkzip or winzip
> which are compatible with biomed's VMS? I will be mailing the home page
> there soon and I would hate to do it one-file-at-a-time.
> 
> Cheers,
> 
> Tom 7
> 

Here's the code so far.  I set it up to require VBE 2.0 for now.  I cant
think of a way the hardware scrolling would help us.  Page flipping
works ok.  I also have it check to make sure video is working (ya!).  As
for dynamically allocated 2d arrays.  They are really hard to do, so I
am looking for a better way.  As for VMS, just ftp to my account.  mput
those bitches.  You will have to run webspace to reset the protections, 
(make sure its from the home directory).

Seth 1
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

#include <allegro.h>

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


BITMAP *page1,*page2,*active_page,*view_page;
MIDI *the_music;


// Look how cool I am... I indented everything all nicely...
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
		} while (mouse_b & button);
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

   if (set_gfx_mode(GFX_VESA2L, 640, 480,0,940))
   {printf("Oh...well Looks like YOU dont have VBE 2.0");
   exit(1);
   };
   //sprintf(msg,"%d,%d",VIRTUAL_H,VIRTUAL_W);
   //textout(screen,font, msg, 500,450, 255);
   //sleep(5);
   //exit(0);

   show_mouse(NULL);
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
page1=create_sub_bitmap(screen,0,0,SCREEN_W,SCREEN_H);
page2=create_sub_bitmap(screen,0,SCREEN_H,SCREEN_W,SCREEN_H);
view_page=page1;
active_page=page2;
text_mode(-1);
s=1;
mapx=5;
mapy=5;
sspeed=8;
install_sound(DIGI_NONE,MIDI_AUTODETECT,NULL);
the_music=load_midi("urgency.mid");
play_midi(the_music,TRUE);
uf=q=1;
current_page=xc=yc=t=0;
die=lx=ly=lxo=lyo=0;
do {
parsemousecommand();
if (uf)
{
update_map();
if (toolson) drawtools();
flippage();
}
uf=1;
}while (!die);

destroy_midi(the_music);
destroy_bitmap(page1);
destroy_bitmap(page2);
exit(0);                           // Another minor Tom adjustment...
			   // Now the program actually exits!!
}

void parsekeycommand(void)
{
int c;
c=(readkey() & 0xFF);        // I take it back.  I thought you were making
switch (c)                  // it lowercase.
{
case 'w':yc+=sspeed;break;
case 'z':yc-=sspeed;break;
case 'a':xc+=sspeed;break;
case 's':xc-=sspeed;break;
case 'e':s=0;break;
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
if (mouse_y>=478) yc+=sspeed;          // sense. ;) It also reacts much more
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
	uf=1;
	if ((q==1)) map[(mouse_y+yc+32)/32+mapy][(mouse_x+xc+32)/32+mapx]=s;
	if ((q==2)) map2[(mouse_y+yc+32)/32+mapy][(mouse_x+xc+32)/32+mapx]=s;
	if ((q==3)) map3[(mouse_y+yc+32)/32+mapy][(mouse_x+xc+32)/32+mapx]=s;
	}



if (keypressed()) parsekeycommand();


lx=(mouse_x+xc+32)/32+mapx;
ly=(mouse_y+yc+32)/32+mapy;
/*
if (! ( (lx==lxo) && (ly==lyo) ) )
{
lxo=lx;
lyo=ly;
}
*/

}

void flippage()
{
show_mouse(NULL);
if (current_page==0)
	{
	scroll_screen(0,0);
	active_page = page2;
   current_page=1;
	view_page = page1;
	//show_mouse(page1);
	}
else
	{
	scroll_screen(0,SCREEN_H);
   active_page = page1;
	//show_mouse(page2);
	view_page = page2;
   current_page=0;
	}
 	//scroll_screen(0, (current_page==0) ? 0: SCREEN_H);
   //vsync();
 	show_mouse(view_page);


}

void update_map(void)
{
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
if (mapx>(MAXX-22))
	{
	mapx=MAXX-22;
	xc=32;
	}
if (mapy>(MAXY-18))
	{
	mapy=MAXY-18;
	yc=32;
	}
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
}

void drawtools(void)
{
rectfill(active_page,0,440,639,479,3);
if (s>0) 
{
blit(tileset[s],active_page,0,0,600,445,32,32);
}
else 
{
blit(toolset[1],active_page,0,0,600,445,32,32);
}

text_mode(13);
sprintf(msg,"XMap: %d YMap: %d ",lx,ly);          
textout(active_page,font, msg, 100,460, 255);
blit(toolset[4],active_page,0,0,340,445,32,32);
blit(toolset[4],active_page,0,0,300,445,32,32);
blit(toolset[4],active_page,0,0,380,445,32,32);
blit(tileset[map[ly][lx]],active_page,0,0,300,445,32,32);
if (map2[ly][lx]) blit(tileset[map2[ly][lx]],active_page,0,0,340,445,32,32);
if (map3[ly][lx]) blit(tileset[map3[ly][lx]],active_page,0,0,380,445,32,32);
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
}