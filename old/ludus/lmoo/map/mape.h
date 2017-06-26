#ifndef _TM7_LUDUS_MAPE_H
#define _TM7_LUDUS_MAPE_H

#include <stdio.h>
#include <time.h>
#include <global.h>
#include <allegro.h>

#include <string>
#include "kconst.h"

#define uchar unsigned char
#define uint unsigned int
/*
#define YTILES 16
#define XTILES 21
*/
#define LAST_TOOLICON 3

#define MAX_LOOPS 10

#define W_TOOLBAR 7
#define W_STATUS 2

#define keybinding string

#define BL(c) bootlog(c ,__FILE__,__LINE__)
#define BLO   bootlog("",__FILE__,__LINE__)

extern int l_waitrelease, r_waitrelease, drawtile, draw_clip,
           redraw_buffer_flag, paintlayer, draw_only_root;
extern keybinding bindkeys[NUM_BINDKEYS];

struct tile_loop {
     tile_loop * link;
     int next,
         tileset,
         interval;
     uchar  loop[16];
     uchar  length;
};

extern string post_evaluate;

void map_draw(BITMAP *, lmap ,int,int);
void onlyroot_off(),
     onlyroot_on();

int limphost_interrupt();

int  drawonmap(lwin*,int,int,int);
int  tilepick(lwin*self,int mx, int my,int mb);
void rootpaint(lwin*);
void map_draw_windows();

void cprint(string s);
int map_keynum(string);
int maptobindspace(int);

string console_input(lwin*,string);
string upper(string i);

void make_tiles(),
     default_mapvariables();

extern BITMAP * toolico[LAST_TOOLICON];
/*
lmap load_map(string & mapname,
              string & tileset,
              string & defmidi,
              string & palfile,
              string & mapinfo,
              string & objfile,
              string & shortname,
              uchar & flags1,
              uchar & flags2,
              uchar & flags3,
              string filename);
*/

void puteightnul(FILE *& outfile, string & ooo);
int geteightsnul(FILE * in, string & out);
int fgetword(FILE * in, int & w);
int fgetdword(FILE * in, int & w);

int  msgclear(lwin*,int);
int tilekey(lwin*,int);
int console_key(lwin * self, int k),
    rootkey    (lwin * self, int k);

void make_icons();

void limpeprint(string),
     limpdprint(string);

void tilespaint(lwin*);

void console_paint(lwin *self);

void do_animation();
#define W_CONSOLE 4

void putword (FILE *& outfile, uint zip),
     putdword(FILE*&,uint);

int add_keybind(string key, string exp);
int fgetstringat(FILE * inways, string & super, int loc);

string map_numtokey(int s);

ext_limp_token map_limp_help(stringqueue & oh),
               map_limp_test(stringqueue & oh),
               map_limp_quit(stringqueue & oh),
               map_limp_use (stringqueue & oh),
               map_limp_bind(stringqueue & oh),
               map_limp_status(stringqueue & oh),
               map_limp_new_map(stringqueue & oh),
               map_limp_toolbar(stringqueue & oh),
               map_limp_set_tile(stringqueue & oh),
               map_limp_save_map(stringqueue & oh),
               map_limp_load_map(stringqueue & oh),
               map_limp_get_file(stringqueue & oh),
               map_limp_add_loop(stringqueue & oh),
               map_limp_fill_layer(stringqueue & oh),
               map_limp_resize_map(stringqueue & oh),
               map_limp_randomlist(stringqueue & oh),
               map_limp_mouse_xtile(stringqueue & oh),
               map_limp_mouse_ytile(stringqueue & oh),
               map_limp_show_console(stringqueue & oh),
               map_limp_toggle_layer(stringqueue & oh),
               map_limp_save_bindings(stringqueue & oh),
               map_limp_set_paintlayer(stringqueue & oh),
               map_limp_refresh_screen(stringqueue & oh),
               map_limp_screen_capture(stringqueue & oh),
               map_limp_toggle_onlyroot(stringqueue & oh),
               map_limp_toggle_bluestuff(stringqueue & oh),
               map_limp_debug_allbindings(stringqueue & oh);


string getfilebase(string in);
void toolbarproc(int idx);

void status(string msg, int ticks = 144);

void map_config (const char *) ;

extern int DRAWLAYER0,DRAWLAYER1,DRAWLAYER2;
extern tile_loop * loophead;

#endif
