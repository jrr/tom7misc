// --=[TP]=-----------------------------------------------------------------
//              TomPaint II Enchanted
// --------------------------------------------------------------=[tp.cc]=--

#include <stdlib.h>
#include <stdio.h>
//#include <unistd.h>

#define ONBUTTON (oldleft || oldright)

/*
 *    Work on the magnifying glass... this is gonna change how everything
 *    works... =/
*/
int DRAWX1, DRAWX2, DRAWY1, DRAWY2;

#include "tp.h"
//#include "allegro.h"

   DECLARE_GFX_DRIVER_LIST(

   GFX_DRIVER_VGA
   GFX_DRIVER_MODEX
   GFX_DRIVER_VBEAF
   GFX_DRIVER_VESA2L
   GFX_DRIVER_VESA2B
   GFX_DRIVER_XTENDED
   GFX_DRIVER_ATI
   GFX_DRIVER_MACH64
   GFX_DRIVER_CIRRUS64
   GFX_DRIVER_CIRRUS54
   GFX_DRIVER_PARADISE
   GFX_DRIVER_S3
   GFX_DRIVER_TRIDENT
   GFX_DRIVER_ET3000
   GFX_DRIVER_ET4000
   GFX_DRIVER_VIDEO7
   GFX_DRIVER_VESA1

   )

DECLARE_COLOR_DEPTH_LIST(
   COLOR_DEPTH_8
//   COLOR_DEPTH_15
//   COLOR_DEPTH_16
//   COLOR_DEPTH_24
//   COLOR_DEPTH_32
)
//DECLARE_DIGI_DRIVER_LIST()
//DECLARE_MIDI_DRIVER_LIST()

int  toolsclick (paintmethod ** tb, int b, int x, int y);
void drawtoolbar(paintmethod ** toolbar);
void drawcolors (int fg, int bg);
void denit(BITMAP *&, PALETTE *&, paintmethod ** toolbar);

void print_progress(int pos) {
   if ((pos & 3) == 3) {
      printf("\xFE");
      fflush(stdout);
   }
}

void init();

   palbut pally;        // globular pallit
   brush brushy;        // globular bwush
   glass glassy;        // globular gwassi
   int toolson, cpaint;

   paintmethod * toolbar[MAX_TOOLS] = {NULL};
   PALLETE * pal;
   BITMAP *buffer;

int main() {
   init ();

   /* make a bitmap for use as a texture map
   texture = create_bitmap(32, 32);
   clear_to_color(texture, 64);
   line(texture, 0, 0, 31, 31, 1);
   line(texture, 0, 31, 31, 0, 1);
   rect(texture, 0, 0, 31, 31, 1);
   text_mode(-1);
   textout(texture, font, "dead", 0, 0, 2);
   textout(texture, font, "pigs", 0, 8, 2);
   textout(texture, font, "cant", 0, 16, 2);
   textout(texture, font, "fly.", 0, 24, 2);
*/
   clear_keybuf();

   show_mouse(screen);

   // line(bitmap,x,y,x,y,color)
   int key;
   int oldleft=0, oldright=0;
      // fgcolor=1, bgcolor=42;

// initialize toolbar

toolbar[0] = new paintbrush;
toolbar[1] = new linetool;
toolbar[3] = new magnify;
toolbar[5] = new hand;
//toolbar[0]->draw(100,100);
for (int n=0;n<MAX_TOOLS;n++) {
    if (!toolbar[n]) toolbar[n] = new paintmethod;
    toolbar[n]->offscreen = buffer;
    }
cpaint=0; toolson = 1;
pally.fgcolor = 0;
pally.bgcolor = 180;
toolbar[cpaint]->select((SCREEN_W - 48) +(24*(cpaint%2)), TOOLBAR_TOP + ((cpaint>>1)*24));

toolbar[cpaint]->color = pally.fgcolor;
toolbar[cpaint]->bgcolor = pally.bgcolor;

//drawtoolbar(toolbar);
//drawcolors(fgcolor,bgcolor);
startmenus();
drawscreen();

   while (1) {
 if (!ONBUTTON) domenus();
// handle mouse ------------------------------------------------------------
// lock variables...

int ppp = mouse_pos;
int xxx = ppp >> 16;
int yyy = ppp & 0xFFFF;
int bbb = mouse_b;
// menu ...
if (ONBUTTON  || !pally.doclick(bbb,xxx,yyy)) {
if (ONBUTTON || !toolsclick(toolbar,bbb,xxx,yyy)) {
// drawering ...
//if (!(toolson && (xxx > (SCREEN_W-48)))) {
if (bbb & 1){
   if (oldleft) toolbar[cpaint]->stillleft((xxx>DRAWMAXX)?DRAWMAXX:xxx,YMAXMAC(yyy));
   else if (xxx<=DRAWMAXX) {
        oldleft=1;
        toolbar[cpaint]->clickleft(xxx,yyy);
        }
} else {
   if (oldleft) { oldleft=0; toolbar[cpaint]->releaseleft((xxx>DRAWMAXX)?DRAWMAXX:xxx,YMAXMAC(yyy)); }
   else ; // assume they don't want to be notified that we're just
          // dragging the mouse around... maybe this will change later.

// only track right mousebutton if left is unpressed.
if (bbb & 2){
   if (oldright) toolbar[cpaint]->stillright((xxx>DRAWMAXX)?DRAWMAXX:xxx,YMAXMAC(yyy));
   else if (xxx<=DRAWMAXX)
        { oldright=1; toolbar[cpaint]->clickright(xxx,yyy); }
} else {
   if (oldright) { oldright=0; toolbar[cpaint]->releaseright((xxx>DRAWMAXX)?DRAWMAXX:xxx,YMAXMAC(yyy)); }
   else toolbar[cpaint]->clickless(xxx,yyy); // ditto
}

}
} // mouse handled by toolbar
} // mouse handled by pally.

// handle keys -------------------------------------------------------------
      if (keypressed()) {
      key = readkey() & 0xFF;
      if (key == 0x0009) {
      toolson ^= 1;
      drawscreen();
      }
	 if ((key & 0xFF) == key) {
            if ((unsigned char)key >= '0' && (unsigned char)key <='9') {
            // pressed number
            toolbar[cpaint]->deselect((SCREEN_W - 48) +(24*(cpaint%2)), TOOLBAR_TOP + ((cpaint>>1)*24));
            cpaint = (unsigned char)key - '0';
            oldleft = oldright = 0;
            toolbar[cpaint]->select((SCREEN_W - 48) +(24*(cpaint%2)), TOOLBAR_TOP + ((cpaint>>1)*24));
            //toolbar[cpaint]->color = pally.fgcolor;
            //toolbar[cpaint]->bgcolor = pally.bgcolor;

            } else switch (key) {
            case 'l': case 'L':
               loadfile();
            break;
            case 27: case 'Q': case 'q':
                 quit(); break;
            case 13: // enter == redraw.
                drawscreen();
                break;
            default:
            break;
            }
	 } else { // key > 255
;//            break;
         }
      }
   }

quit();
}

void quit () {
  show_mouse(NULL);
  destroy_bitmap(buffer);
   free(color_map);
   free(pal);
  for (int m=MAX_TOOLS;m--;) delete toolbar[m];
   exit(0);

}
int bitx=0, bity=0,bitheight,bitwidth;
void init () {
int c, w, h;

   allegro_init();
   install_keyboard();
   install_mouse();
   install_timer();
   printf("Tompaint II Enchanted " VERSION " Loading...\n\n"
           "Using Allegro version %s\n",allegro_id);
   slick_info();
//   sleep(1);

   pal = (PALETTE*)malloc(sizeof(PALETTE));

   /* color 0 = black */
   (*pal)[0].r = (*pal)[0].g = (*pal)[0].b =    // transparent
   (*pal)[1].r = (*pal)[1].g = (*pal)[1].b = 0; // opaque
   (*pal)[255].r = (*pal)[255].g = (*pal)[255].b = 255;
   /* copy the desktop pallete */
   for (c=48; c<255; c++) {
      (*pal)[c].r = (unsigned char) (desktop_pallete[255-c].r * (c/255.));
      (*pal)[c].g = (unsigned char) (desktop_pallete[255-c].g * (c/255.));
      (*pal)[c].b = (unsigned char) (desktop_pallete[255-c].b * (c/255.));
      }

   /* build a lighting table */
//   printf("Generating lighting table:\n");
//   printf("<................................................................>\r<");
//   color_map = (COLOR_MAP*)malloc(sizeof(COLOR_MAP));
//   create_light_table(color_map, *pal, 0, 0, 0, print_progress);

   /* set the graphics mode */
   set_gfx_mode(GFX_VGA, 320, 200, 0, 0);
   set_pallete(desktop_pallete);

   if (!gfx_mode_select(&c, &w, &h)) {
      allegro_exit();
      exit(1);
   }

   if (set_gfx_mode(c, w, h, 0, 0)) {
      allegro_exit();
      printf("Error setting graphics mode\n%s\n\n", allegro_error);
      exit(1);
   }

   bitheight = SCREEN_H;
   bitwidth = SCREEN_W;


   buffer = create_bitmap(bitwidth, bitheight);
   clear_to_color(buffer,255);
   clear_to_color(screen,255);

   set_mouse_sprite(load_bitmap("mouse.pcx",NULL));
   set_mouse_sprite_focus(7,7);
//   _mouse_xor = 1; //buggy.
   set_pallete(*pal);
   gui_bg_color = 255;
   gui_fg_color = 0;
}
/*
void drawcolors(int fg, int bg) {
    show_mouse(NULL);
    show_mouse(screen);
}
*/
void drawtoolbar(paintmethod ** toolbar) {
//show_mouse(NULL);
for (int m = 0; m<MAX_TOOLS;m++)
   toolbar[m]->draw(   //m*24,0);
   (SCREEN_W - 48) +(24*(m%2)), TOOLBAR_TOP + ((m>>1)*24));

line(screen,SCREEN_W-48,TOOLBAR_TOP+(MAX_TOOLS>>1)*24,SCREEN_W,
                               TOOLBAR_TOP+(MAX_TOOLS>>1)*24,0);
//show_mouse(screen);
}
void drawscreen() {
    /* drawscreen(toolbar,toolson, buffer,&pally);
//}
void drawscreen(paintmethod ** toolbar,int toolson,
                                              BITMAP * buffer,
                                              palbut * pally) {
                                              */
show_mouse(NULL);
blit_buffer();
if (toolson) {
   drawtoolbar(toolbar);
   pally.draw();
   }
glassy.update();
redrawmenus();
show_mouse(screen);
}

void blit_buffer() {
     blit(buffer, screen, bitx, bity, 0, 12,
                  SCREEN_W - (toolson*48),
                  SCREEN_H-12);    /* this should become safe_blit,
                                   which makes a grey nothingness if
                                   you blit beyond the bounds of the
                                   bitmap. */
}

int toolsclick (paintmethod ** tb, int b, int x, int y) {
int fa, na=0;
if (!toolson) return 0;
for (int n=0;n<MAX_TOOLS;n++)
    if (tb[n]->yourmenu(
    (SCREEN_W - 48) +(24*(n%2)),
    TOOLBAR_TOP + ((n>>1)*24),
    b, x, y)) { return 1;
            na++;
    }
if (!na) {

for (int n=0;n<MAX_TOOLS;n++)
    if (2 == (fa = tb[n]->clickyou(
    (SCREEN_W - 48) +(24*(n%2)),
    TOOLBAR_TOP + ((n>>1)*24),
    b, x, y))) {
            tb[cpaint]->deselect((SCREEN_W - 48) +(24*(cpaint%2)), TOOLBAR_TOP + ((cpaint>>1)*24));
            cpaint = n;
            tb[cpaint]->select((SCREEN_W - 48) +(24*(cpaint%2)), TOOLBAR_TOP + ((cpaint>>1)*24));
            return 1;
    } else if (fa) na++;
return (na!=0);
 } else return 1;
}

void loadfile(void) {
 char nooper[100] = {".\\"};
 if(file_select("Load file",nooper,"PCX")) {
     BITMAP * xyz = load_pcx(nooper,*pal);
     blit (xyz,buffer,0,0,0,0,SCREEN_W,SCREEN_H);
     set_pallete(*pal);
     destroy_bitmap (xyz);
 }
 drawscreen();
}

// file size reduction
/*
char**__ctr0_glob_function(char *) {return 0;}
void __crt0_load_environment_file(char*) {}
void __crt0_setup_arguments(void) {}
*/
