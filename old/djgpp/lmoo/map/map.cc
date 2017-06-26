//*--------------=[ map.cc ]=--------------------------------------------//
//                      map editor for ludus.
//*--------------------------------------=[ ludus ]=---------------------//

/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */

#define VERSION "0.927"

#include "mape.h"
#include "kconst.h" // key constants

/* configurable "constants" */
string DATAF;
int XTSIZE=32, YTSIZE=32, XTMASK = 31, YTMASK = 31, XTBITS = 5, YTBITS = 5;
int XTILES, YTILES;
int DEFX, DEFY;
int SELW, SELH;
int NUMTILES = 247, TILESW;

/* flags */

int DRAWLAYER0=1,DRAWLAYER1=1,DRAWLAYER2=1;

int l_waitrelease=0,  /* something is waiting for the mouse button to be */
    r_waitrelease=0;  /*    released.   */

keybinding bindkeys[NUM_BINDKEYS];
// also keybinding JOY_BUTTON1, etc.

int pos=(100<<16)|100,oldpos=0,oc=0,drawtile=1;

int draw_clip = 0;

int redraw_buffer_flag;

int paintlayer = 0;

/* tiles[tileset][tile] */

BITMAP * tiles[1][256];
BITMAP * toolico[LAST_TOOLICON];

/*
tile_loop loops[MAX_LOOPS] = {
     { 1, 0, 15, {7,8,9,0},3 },
     { 1, 0,  30, {212,213},2},
     { 0, 0, 0,  {0}, 0 },
};
*/

tile_loop * loophead = 0;

string post_evaluate = "";  // sorry
/* FIXME: with no setjmp, this kind of nonsense isn't needed any more. */

int draw_only_root = 0;

     BITMAP * testtiles;

limp_funcstuff limpfunctions[] = {
     {"randomlist",map_limp_randomlist},
     {"all-keybindings",map_limp_debug_allbindings},
     {"toggle-bluestuff",map_limp_toggle_bluestuff},
     {"toggle-onlyroot",map_limp_toggle_onlyroot},
     {"screen-capture",map_limp_screen_capture},
     {"set-paintlayer",map_limp_set_paintlayer},
     {"screen-refresh",map_limp_refresh_screen},
     {"save-bindings", map_limp_save_bindings},
     {"show-console",map_limp_show_console},
     {"toggle-layer",map_limp_toggle_layer},
     {"mouse-xtile",map_limp_mouse_xtile},
     {"mouse-ytile",map_limp_mouse_ytile},
     {"resize-map",map_limp_resize_map},
     {"fill-layer",map_limp_fill_layer},
     {"get-file",map_limp_get_file},
     {"set-tile",map_limp_set_tile},
     {"save-map",map_limp_save_map},
     {"add-loop",map_limp_add_loop},
     {"load-map",map_limp_load_map},
     {"new-map",map_limp_new_map},
     {"toolbar",map_limp_toolbar},
     {"status",map_limp_status},
     {"help",map_limp_help},
     {"test",map_limp_test},
     {"bind",map_limp_bind},
     {"quit",map_limp_quit},
     {"use",map_limp_use},
     {NULL,NULL},
};
void tempproc(int);
void tempproc(int) {
     limpdprint("I like smack.");
}

int main (int argc, char ** argv) {
     DATAFILE* dub;

     int x=0,mx,mb,my;

     BL("Start");

     allegro_init();
     install_keyboard();
     install_timer();
     install_mouse();

     srand(time(NULL)); /* move to some other init routine */

     BL("Init:");
     BL("   readconf:");
     if (argc > 1)
     map_config (argv[1]) ;
     else map_config ("config") ;
     BL("   text_init:");
     text_init();
     BL("   dirt_init:");
     dirt_init();
     BL("   lwin_init:");
     lwin_init();
     BL("   limp_init:");
     limp_init();
     BL("Init done.");

     print_limp_error = limpeprint;
     print_limp_debug = limpdprint;
     limp_interrupt = limphost_interrupt;

     BL("Funcreg:");

     limp_regfuncstuffs(limpfunctions);

     BL("Color_map:");

     color_map = new COLOR_MAP;
     create_trans_table(color_map,_current_pallete,128,128,128,0);

     if (!
       (dub = load_datafile_object(DATAFI,"tiles"))
     )
      { printf ("Couldn't open the datafile.\n"); exit(-1); }
     testtiles = (BITMAP *)dub->dat;
     if ((dub = load_datafile_object(DATAFI,"pal")))
     {
        set_palette((PALETTE)dub->dat);
     } else { printf ("Couldn't open the datafile.\n"); exit(-1); }



     BL("Maketiles, windows:");

     make_tiles();
     make_icons();
#if 0
     lw_label * s1 = new lw_label;
     s1->text = "~0hello!";
     lw_label * s2 = new lw_label;
     s2->text = "~GL~gook ~RA~rt ~YA~yll ~BT~beh ~0C~eol~Eou~g~rr~bs~y!";
     lw_button *s3 = new lw_button;

     s3->text = "~RPoop";
     s3->flags = BUTTON_DRAW_PIC | BUTTON_DRAW_TEXT;
     s3-> pic = toolico[0];
     s3->id = 1;
     s3->proc = tempproc;

     lw_description pox;
     pox.title = "~0Test of ~rGe~yner~Galiz~yed~B!";
     pox.color = 14;
     windows[6] = create_generalized_lwin(3,3,400,220,pox);
     add_widget(&windows[6],s1,LW_LABEL,280,12,0,0);
     add_widget(&windows[6],s2,LW_LABEL,27,100,0,0);
     add_widget(&windows[6],s3,LW_BUTTON,100,30,40,40);
#endif

//   windows[W_TOOLBAR] = 0;

     windows[3] = create_lame_lwin(XTSIZE*SELW + 3 + 8,YTSIZE*SELH + 12 + 3,
                                   100,300,
                                   14,"~G[~0Tile Selector~G]");

//     windows[3].style = LWINSTYLE_TRANSLAME;
     windows[3].scrollbar = 1;
     windows[3].scrollmax = (NUMTILES/SELW) - (SELH-1);
     windows[3].scrollpos = 0;
     windows[3].clicked   = tilepick;
     windows[3].paint     = tilespaint;
     windows[3].keypressed= tilekey;

     windows[0] = create_null_lwin(0,0);
     windows[0].w = 640; windows[0].h = 480;
     windows[0].style = LWINSTYLE_ROOT;
     windows[0].canvas = create_bitmap(640,480);
     windows[0].clicked = drawonmap;
     windows[0].paint = rootpaint;
     windows[0].keypressed = rootkey;

     windows[W_STATUS] = create_transtext_lwin ( 16,460,0,
                         ""); /* status window */


     status("~g[~0[~g[ ~BLudus Map Editor ~0" VERSION " ~g]~0]~g]");

     windows[W_CONSOLE] = create_console_lwin(30,100,580,280,C_WHITE,
     "~B[~0*~BConsole~0*~B]",
     "~0<~bConsole~0> ~eWelcome to the ludus map editor "
     "version ~B" VERSION "~e. This is a menuless interface; "
     "you enter commands at the console (type ~Bhelp~e for "
     "commands, or [~Besc~e] to hide the console).");
     windows[W_CONSOLE].paint = console_paint;
     windows[W_CONSOLE].keypressed = console_key;

     windows[0].map.h = DEFY;
     windows[0].map.w = DEFX;

     windows[0].map.dat[0] = (ushort*) malloc((sizeof (ushort))*DEFX*DEFY);
     windows[0].map.dat[1] = (ushort*) malloc((sizeof (ushort))*DEFX*DEFY);
     windows[0].map.dat[2] = (ushort*) malloc((sizeof (ushort))*DEFX*DEFY);
     windows[0].map.clip   = (uchar *) malloc((DEFX*DEFY));

     mx = 1;
     for (x=0;x<(windows[0].map.h * windows[0].map.w);x++)
        { windows[0].map.dat[0][x] = 60;
          windows[0].map.dat[1][x] =   0;
          windows[0].map.dat[2][x] = 0; //35;
//          windows[0].map.clip[x>>1] |= (x&1)?((x&3)&7):((x&7)<<4);
          windows[0].map.clip[x] = 0;
        }

//         (enc_block((wint){time(0),5},(wint){x,~x}).h % 64);

     windows[0].data[4] = windows[0].data[5] =

     windows[0].data[0] = windows[0].data[1] =
     windows[0].data[2] = windows[0].data[3] = 0;

//           rootpaint(&windows[0]);


     BL("Paints:");

/* call paint methods if they exist... */

for (x=0;x<MAX_LWIN;x++) if (windows[x].style && windows[x].paint)
                              windows[x].paint(&windows[x]);

           map_draw_windows();

           force_fs();

     default_mapvariables();

     BL("Start Loop:");

               /* ugh hack */
               eval_limp("(use rc.lim)");
               while (post_evaluate != "") {
                  string temp = post_evaluate;
                  post_evaluate = "";
                  eval_limp(temp);
               }
               /* ugh hack */

     while (!windows[0].killme) {
     redraw_buffer_flag=0;
          show_mouse(NULL);

          windows[0].data[0] += windows[0].data[4];
          windows[0].data[1] += windows[0].data[5];

          if (windows[0].data[0] > ((windows[0].map.w -XTILES)<<5))
              windows[0].data[0] = (windows[0].map.w-XTILES)<<5;
          if (windows[0].data[1] > ((windows[0].map.h -YTILES)<<5))
              windows[0].data[1] = (windows[0].map.h-YTILES)<<5;
          if (windows[0].data[0] < 0) windows[0].data[0] = 0;
          if (windows[0].data[1] < 0) windows[0].data[1] = 0;

          pos = mouse_pos;
          mx = pos>>16;
          my = pos&65535;
          mb = mouse_b;

          if (! (mouse_b & 1)) l_waitrelease=0;
          if (! (mouse_b & 2)) r_waitrelease=0;
          freeze_mouse_flag = 1;

     lwin_do_windows(mx,my,mb);
     do_animation();

          windows[0].data[4] = windows[0].data[5] = 0;

     if (windows[0].data[0] != windows[0].data[2]
       ||windows[0].data[1] != windows[0].data[3]) {
          windows[0].paint(&windows[0]);
          redraw_buffer_flag++;
          force_fs();
          windows[0].data[2] = windows[0].data[0];
          windows[0].data[3] = windows[0].data[1];
       }

      /******/

        if (pos != oldpos) {
           dirtyspritemove(oldpos>>16,oldpos&65535,mx,my,16,16);
           oldpos = pos;
        }

           if (mx  > 637)   windows[0].data[4]= 8;
           if (mx <  3)     windows[0].data[4]=-8;
           if (my  > 477)   windows[0].data[5]= 8;
           if (my <  3)     windows[0].data[5]=-8;
#if 0
/*(KB_SHIFT_FLAG & key_shifts)*//* || key[KEY_RCONTROL]*/
#endif
     
          if (!(key_shifts & KB_SHIFT_FLAG)) {
             if (key[KEY_LEFT])  windows[0].data[4]-=4;
             if (key[KEY_RIGHT]) windows[0].data[4]+=4;
             if (key[KEY_UP])    windows[0].data[5]-=4;
             if (key[KEY_DOWN])  windows[0].data[5]+=4;
          }

     if (redraw_buffer_flag) { map_draw_windows(); }

          freeze_mouse_flag = 0;
          show_mouse(buffer);
          dirty_draw();
     }

     set_gfx_mode(GFX_TEXT,0,0,0,0);
return 0;
}

string console_input(lwin*,string in) {
     /* this is where to handle all the inputs to the console.
     */
//     lconsole_addstring(self,string("(") +in + string(")"));
     // add parentheses?
     for (uint x=0;x<in.length();x++) {
          if (!whitespc(in[x])) {
               string evalume;
               if (in[x] != '(' && in[x] != '`')
                  evalume = string("(") + in + string(")");
               else evalume = in;
               post_evaluate = "";
//               BL("About to eval_limp(evalume)");
               string uaa = eval_limp(evalume);
//               BL("(done)");               
               while (post_evaluate != "") {
                  string temp = post_evaluate;
                  post_evaluate = "";
                  uaa =eval_limp(temp);
               }
               return uaa;
          break;
          }
     }
     return "";
}

int console_key(lwin * self, int k) {

/*
     char temp[256];
     sprintf(temp,"0x%04X", k);
     lconsole_addstring(self,temp);
*/
     k &= 0xff;
     if (k == 1) k = ' ';
/*
     char sh[123];
     sprintf(sh,"Key: ~g%d",k);
     lconsole_addstring(self,sh);
*/
     switch(k) {
     case 13: {
          lconsole_addstring(self,(string)"~0<~bYou~0> ~e"+self->inputline);
          string sum = console_input(self,self->inputline);
          if (sum != "") {
            lconsole_addstring(self,(string)"~0<~Glimp~0> ~e"+sum);
          }
          }
          self->inputline = "";
          break;
     case 27:
          /* 'minimize' ... */
          self->flags &= ~(LWIN_FL_ACTIVE);
          break;
     case 8:
          self->inputline = self->inputline.substr(0,self->inputline.length()-1);
          break;
     case 9:
          /* do tab completion */
          // First seek out the last token here.
          {
               string s;
               uint x = self->inputline.length();
#define isseparator(c) ((c)==' '||(c)=='('||(c)=='`')
               while (x--) if (isseparator(self->inputline[x])) break;
#undef isseparator(c)               
               x++;

               (s =self->inputline.substr(x,self->inputline.length()-x));

               if (s == "") return 1;
               /*********************************/
               stringqueue snut;
               possiblefuncs(s,limp_FUNC_ROOT,snut);

               if (snut.length() > 1) {
                    string stuff;
                    for (uint i=0;i<snut.length();i++) {
//                    BL(stuff.c_str());
                    stuff += snut.stuff[i];
                    if (i != snut.length()) stuff += ' ';
                    }
                    limpdprint(stuff);
                    uint shortest = 0, shortlen=65535;
                    for (uint i=0;i<snut.length();i++) {
                      if (snut.stuff[i].length() < shortlen) {
                         shortlen = snut.stuff[i].length();
                         shortest=i;
                      }
                    }
                    string completion="";
                    for (uint y=0;y<shortlen;y++) {
                         for (uint i=0;i<snut.length();i++) {
                              if (snut.stuff[shortest][y] !=
                                  snut.stuff[i][y]) {
                                   completion =snut.stuff[shortest].substr(0,y);
                                   goto sorry_nope;
                                   }
                         }
                    }
                    completion = snut.stuff[shortest];
                    sorry_nope:;
                    self->inputline =
                         self->inputline.substr(0,x) +
                         completion;
               } else if (snut.length() == 1) {
                    /* exactly one match. Replace. */
                    self->inputline =
                         self->inputline.substr(0,x) +
                         snut.stuff[0] + (string)" ";
               } else {
                    limpdprint("[no match]");
               }

          }
          break;
     default:
          if (k >= ' ' && k <= '~') {
               self->inputline += k;
          } else {
               cprint(itos(k));
          return 0;
          }
          break;
     }
     if(self->paint)(self->paint)(self);

     map_draw_windows(); /* for any keypress! */
     thisisdirty(self->x,self->y,self->w,self->h);
     return 1; /* basically accept any key. */
}

void console_paint(lwin *self) {
     /* draw text strings to self->canvas */
     int th = self->data[2]; //text_height(smallfont);

     clear_to_color(self->canvas,30); /* text is transparent so we've got to
                                         clear the canvas first... */

     int ystart = self->h -
          (self->tb+self->bb+(th<<1)+5);

     int y = self->data[0];

     int startby = self->data[1];

     startby -= (self->scrollmax-self->scrollpos);
     if (startby < 0) startby += LWIN_CON_SCROLLBACK;

     y -= (self->scrollmax - self->scrollpos);

     for (uint x=startby;x--;) {
          y--;
          if (y < 0) y = LWIN_CON_SCROLLBACK-1;

          if ( ((string*)self->pointera)[y] != "")
             ftextout(self->canvas,smallfont,
                  ((string*)self->pointera)[y].c_str(),0,ystart);
          ystart -= th;
          if (ystart < (-th)) break;
                         // out of window now.
     }
     hline(self->canvas,2,self->h-(self->tb+self->bb+th+4),self->w - (self->lb + self->rb + 2),31);
     hline(self->canvas,2,1+self->h-(self->tb+self->bb+th+4),self->w - (self->lb + self->rb + 2),29);

     uint numchars = ( (self->w-(self->lb+self->rb)) /th ) - 2;

     if (self->inputline.length() < numchars)
       ptextout(self->canvas,smallfont,(self->inputline + (string)("_")).c_str(),0,self->h - (self->tb+self->bb+th+2),0);
     else
       ptextout(self->canvas,smallfont,(self->inputline + (string)("_")).c_str()+(self->inputline.length()-numchars),0,self->h - (self->tb+self->bb+2+th),0);

     /* draw cursor. */
}

void map_draw(BITMAP*b,lmap m,int mx,int my) {
     int x,y,t,u,g=0,xoff=mx>>XTBITS,yoff=my>>YTBITS,xplus=mx&XTMASK,yplus=my&YTMASK;
     char msg[512];
     
     for(y=yoff;y<(yoff+YTILES);y++)
       for(x=xoff;x<(xoff+XTILES);x++) {
          if (x >= 0 && x < m.w && y >= 0 && y < m.h) {
             t = m.dat[0][(y*m.w) + x];
             u = m.dat[1][(y*m.w) + x];
             g = m.dat[2][(y*m.w) + x];
             }
          else t = u = g = 0;
          if (!DRAWLAYER0) t=0;
          blit(tiles[0][t],b,0,0,((x-xoff)<<XTBITS)-xplus,((y-yoff)<<YTBITS)-yplus,XTSIZE,YTSIZE);
          if (u && DRAWLAYER1)
              draw_sprite(b,tiles[0][u],((x-xoff)<<XTBITS)-xplus,((y-yoff)<<YTBITS)-yplus);
          if (g && DRAWLAYER2) 
              draw_sprite(b,tiles[0][g],((x-xoff)<<XTBITS)-xplus,((y-yoff)<<YTBITS)-yplus);
       }

     /* PLEASE OPTIMIZE THIS to draw bigger rectangles where possible. */
     /************************* FIXME ! **************/
     if (draw_clip || (paintlayer==3)) {
        drawing_mode(DRAW_MODE_TRANS,0,0,0);
     for(y=yoff;y<(yoff+YTILES);y++)
        for (x=xoff;x<(xoff+XTILES);x++) {
          if (x >= 0 && x < m.w && y >= 0 && y < m.h) {
               t = m.clip[((y*m.w)+x)]&7;
//               if (x&1) t  &= 7;
//                   else t >>= 4;
          } else t = 0;
          if (t) rectfill(b,
               ((x-xoff)<<XTBITS)-xplus,
               ((y-yoff)<<YTBITS)-yplus,
               XTMASK+(((x-xoff)<<XTBITS)-xplus),
               YTMASK+(((y-yoff)<<YTBITS)-yplus),
               16);
        }
        solid_mode();
     }
     force_fs();
/*
     sprintf(msg,"~Wxoff: ~B%d ~Wyoff: ~B%d ~Wxplus: ~B%d ~Wyplus: ~B%d ~Wt: ~B%d",
               xoff,yoff,xplus,yplus,drawtile);
*/
     sprintf(msg,"~Wx: ~B%d ~Wy: ~B%d ~Wtile: ~B%d ~Wlayer: ~B%d",
               xoff,yoff,drawtile,paintlayer);

     ftextout(b,smallfont,msg,10,10);
}

int tilepick(lwin*self,int mx, int my,int) {
     if (key_shifts & KB_CTRL_FLAG) {
               /* if console is open ************************/
          if ((windows[W_CONSOLE].flags & LWIN_FL_ACTIVE)&& (! l_waitrelease)) {
          if (windows[W_CONSOLE].inputline.length() &&
              windows[W_CONSOLE].inputline[
                 windows[W_CONSOLE].inputline.length()-1] != ' ')
                 windows[W_CONSOLE].inputline += ' ';
          windows[W_CONSOLE].inputline +=itos(
               ((self->scrollpos+(my>>YTBITS))*SELW) +(mx>>XTBITS));// + string(" ");
          if(windows[W_CONSOLE].paint)(windows[W_CONSOLE].paint)(&windows[W_CONSOLE]);
          map_draw_windows();
          force_fs();
          l_waitrelease=1;
          }
     } else drawtile = ((self->scrollpos+(my>>YTBITS))*SELW) +(mx>>XTBITS);
     return 0;
}

int drawonmap(lwin*self,int mx,int my,int mb) {
      int cx= self->data[0]+mx,
          cy= self->data[1]+my,
          offs=(cy>>YTBITS)*self->map.w + (cx>>XTBITS);
//      if (!mb) return;
     if ((cx>>XTBITS) >= self->map.w) return 0; //cx = self->map.w-1;
     if ((cy>>YTBITS) >= self->map.h) return 0; //cy = self->map.h-1; 


      if (paintlayer == 3) {
          if (mb&1) {

          if (!(self->map.clip[offs]&7)) {
            self->map.clip[offs] |= 1;
            thisisdirty((mx&self->data[0]) -XTSIZE,(my&self->data[1]) -YTSIZE,XTSIZE,YTSIZE);
            rootpaint(self);    // This could be made faster, by only drawing
            map_draw_windows();       // the tile that changed. Big deal.
          }
          } else {
          if ((self->map.clip[offs]&7)) {
            self->map.clip[offs] &= 0xF0;
            thisisdirty((mx&self->data[0]) -XTSIZE,(my&self->data[1]) -YTSIZE,XTSIZE,YTSIZE);
            rootpaint(self);    // This could be made faster, by only drawing
            map_draw_windows();       // the tile that changed. Big deal.
          }

/*
          if (!(self->map.clip[offs>>1] & ((!(offs&1))?(0xF0):(0x0F))) ){
            self->map.clip[offs>>1] |= ((!(offs&1))?(0xF0):(0x0F));
            thisisdirty((mx&self->data[0]) -XTSIZE,(my&self->data[1]) -YTSIZE,XTSIZE,YTSIZE);
            rootpaint(self);    // This could be made faster, by only drawing
            map_draw_windows();       // the tile that changed. Big deal.
          }
          } else {
          if (self->map.clip[offs>>1] & ((!(offs&1))?(0xF0):(0x0F))) {
            self->map.clip[offs>>1] &= ((!(offs&1))?(0x0F):(0xF0));
            thisisdirty((mx&self->data[0]) -XTSIZE,(my&self->data[1]) -YTSIZE,XTSIZE,YTSIZE);
            rootpaint(self);    // This could be made faster, by only drawing
            map_draw_windows();       // the tile that changed. Big deal.
          }
*/
          }
      } else {
         if (self->map.dat[paintlayer][offs] != drawtile)
            { self->map.dat[paintlayer][offs] = drawtile;
          thisisdirty((mx&self->data[0]) -XTSIZE,(my&self->data[1]) -YTSIZE,XTSIZE,YTSIZE);
          rootpaint(self);    // This could be made faster, by only drawing
          map_draw_windows();       // the tile that changed. Big deal.
            }
      }
return 0;
}

void rootpaint(lwin*self) {
     map_draw(self->canvas, self->map,self->data[0],self->data[1]);
     self->data[2] = self->data[0];
     self->data[3] = self->data[1];
}

int rootkey(lwin*,int k) {

     /* these are now all handled by bindings in rc.lim */

     k = maptobindspace(k);
     if (k >= 'A' && k <= 'Z') k |= 32;
     if (k < NUM_BINDKEYS && bindkeys[k] != "") {
          string retty =
            eval_limp(string("(")+bindkeys[k]+string(")"));
          if (retty != "" && LIMP_VERBOSITY > 0)
               lconsole_addstring(&windows[W_CONSOLE],retty);
          return 1;
     } else return 0;
}

void tilespaint(lwin*self) {
     int x,y,t;
     for (y=0;y<SELH;y++) {
       for (x=0;x<SELW;x++) { /* HERE */
           t = (self->scrollpos + y)*SELW + x;
           if (t>=NUMTILES) t=0;
           blit(tiles[0][t],self->canvas,0,0,x<<XTBITS,y<<YTBITS,XTSIZE,YTSIZE);
       }
     }
}

int tilekey(lwin*self,int k) {
     switch (k>>8) {
       case KEY_PGUP:
            self->scrollpos-=SELH;
            if (self->scrollpos < 0) self->scrollpos=0;
            if (self->paint)(self->paint)(self);
            map_draw_windows();
            thisisdirty(self->x,self->y,self->w,self->h);
       break;
       case KEY_UP:
       if (key_shifts & KB_SHIFT_FLAG) {
            self->scrollpos--; if (self->scrollpos < 0) self->scrollpos=0;
            if (self->paint)(self->paint)(self);
            map_draw_windows();
            thisisdirty(self->x,self->y,self->w,self->h);
       }
       break;
       case KEY_PGDN:
            self->scrollpos+=SELH;
            if (self->scrollpos > self->scrollmax)
                 self->scrollpos=self->scrollmax;
            if (self->paint)(self->paint)(self);
            map_draw_windows();
            thisisdirty(self->x,self->y,self->w,self->h);
       break;
       case KEY_DOWN:
       if (key_shifts & KB_SHIFT_FLAG) {
            self->scrollpos++;
            if (self->scrollpos > self->scrollmax)
                 self->scrollpos=self->scrollmax;
            if (self->paint)(self->paint)(self);
            map_draw_windows();
            thisisdirty(self->x,self->y,self->w,self->h);
       }
       break;
       case KEY_HOME:
            self->scrollpos=0;
            if (self->paint)(self->paint)(self);
            map_draw_windows();
            thisisdirty(self->x,self->y,self->w,self->h);
       break;
       case KEY_END:
            self->scrollpos=self->scrollmax;
            if (self->paint)(self->paint)(self);
            map_draw_windows();
            thisisdirty(self->x,self->y,self->w,self->h);
       break;
       default:
       return 0;
     }
     return 1;
}

int msgclear(lwin* self, int) {
     self->text = "";
     map_draw_windows();
     force_fs(); /* sorry ! */
     return 0;
}

void make_tiles() {
     int t;
     for (t=0;t<NUMTILES;t++) {
       tiles[0][t] = create_bitmap(XTSIZE,YTSIZE);
       blit(testtiles,tiles[0][t],(t%TILESW)<<XTBITS,(t/TILESW)<<YTBITS,0,0,XTSIZE,YTSIZE);
     }
}

void make_icons() {
     DATAFILE * dub;
     BITMAP * wack;
     if ((dub = load_datafile_object(DATAFI,"mapico"))) {
          wack = (BITMAP*)dub->dat;
          for(int x=0;x<LAST_TOOLICON;x++) {
               toolico[x] = create_bitmap(13,13);
               blit(wack,toolico[x],x*13,0,0,0,13,13);
          }
     } else {
          /* can't open datafile, tool thingie, something? */
     }
}

void do_animation() {
     int /*x=0, */y=0,rc = retrace_count,dopaint=0;
     BITMAP * temp;
     tile_loop * ttt = loophead;
     for (;ttt;ttt = ttt -> link) {
          if (ttt->next && ttt->next < rc) {
//               cprint((string)"Dude: rc:" + itos(rc) + (string)" len:" + itos(ttt->length) + (string)" ts:" + itos(ttt->tileset));
               temp = tiles[ttt -> tileset][ ttt -> loop[0]];
               for (y=0;y<(ttt -> length-1);y++) {
                    tiles[ttt->tileset][ttt -> loop[y]] =
                    tiles[ttt -> tileset][ttt -> loop[y+1]];
               }
               tiles[ttt -> tileset][ttt -> loop[y]] = temp;
               ttt -> next = rc + ttt -> interval;
               dopaint = redraw_buffer_flag = 1;
          }
     }
if (dopaint) {
      rootpaint(&windows[0]);
      tilespaint(&windows[3]); /*************** FIXME always 3? */
          force_fs();
     }
}

void limpeprint(string s) {
     cprint((string)"~0[~rERROR~0] ~e"+s);
}

void limpdprint(string s) {
     cprint((string)"~0(~Bdebug~0) ~e"+s);
}


void default_mapvariables() {
   limp_setval("PALFILE", "testpal");
   limp_setval("TILESET", "testset");
   limp_setval("DEFMIDI", "trunk");
   limp_setval("MAPNAME", "Default mapname.");
   limp_setval("MAPINFO", "Default info. Created with Ludus map editor " VERSION ".");
   limp_setval("FLAGS1",  "0");
   limp_setval("FLAGS2",  "0");
   limp_setval("FLAGS3",  "0");
}

void putdword (FILE *& outfile, uint zip) {
     fputc(zip>>24,outfile);
     fputc(255&(zip>>16),outfile);
     fputc(255&(zip>>8),outfile);
     fputc(255&zip,outfile);
}

void putword (FILE *& outfile, uint zip) {
     fputc(255&(zip>>8),outfile);
     fputc(zip&255,outfile);
}

void puteightnul(FILE *& outfile, string & ooo) {
     for (uint x=0;x<8;x++) {
          if (x < ooo.length())
               fputc(ooo[x],outfile);
          else fputc(0,outfile);
     }
     fputc(0,outfile);
}

void cprint(string s) {
     lconsole_addstring(&windows[W_CONSOLE],s);
     if (windows[W_CONSOLE].flags & LWIN_FL_ACTIVE) {
          windows[W_CONSOLE].paint(&windows[W_CONSOLE]);
          map_draw_windows();
          thisisdirty(windows[W_CONSOLE].x,
                      windows[W_CONSOLE].y,
                      windows[W_CONSOLE].w,
                      windows[W_CONSOLE].h);
     }
}

int add_keybind(string keyz, string exp) {
     int knum = map_keynum(keyz);
     if (knum < 0) return -1;
     bindkeys[knum] = exp;
     VERB     cprint((string)"~g"+keyz + (string)"~0 -> ~B" + exp);
     return 1;
}

int map_keynum(string s) {
     if (s.length() == 1) {
          uchar kk = s[0];
          if (kk     >= 'A' && kk     <= 'Z') kk     |= 32;
          if (kk     > 127  || kk     == '`') { return -1; }
          return kk;
     } else {
          s=upper(s);
          if (s == "ESC") {
               return 27;
          } else if (s == "ENTER") {
               return 13;
          } else if (s == "BACKSPACE") {
               return 8;
          } else if (s == "TAB") 
               return 9;
            else if (s == "TILDE") return '~';
            else if (s == "APOSTROPHE" || s == "QUOTE") return '\'';
            else if (s == "BACKQUOTE") return '`';
            else if (s == "BACKSLASH") return '\\';
            else if (s == "F1") return KB_F1;
            else if (s == "F2") return KB_F2;
            else if (s == "F3") return KB_F3;
            else if (s == "F4") return KB_F4;
            else if (s == "F5") return KB_F5;
            else if (s == "F6") return KB_F6;
            else if (s == "F7") return KB_F7;
            else if (s == "F8") return KB_F8;
            else if (s == "F9") return KB_F9;
            else if (s == "F10") return KB_F10;
            else if (s == "F11") return KB_F11;
            else if (s == "F12") return KB_F12;
            else if (s == "C-S") return KB_C_S;
            else return -1;
     }
}

string map_numtokey(int s) {
     switch(s) {
          case 27: return "ESC";
          case 13: return "ENTER";
          case 8: return "ENTER";
          case 9: return "TAB";
          case '~': return "TILDE";
          case '\'': return "QUOTE";
          case '`': return "BACKQUOTE";
          case '\\': return "BACKSLASH";
          case KB_F1: return "F1";
          case KB_F2: return "F2";
          case KB_F3: return "F3";
          case KB_F4: return "F4";
          case KB_F5: return "F5";
          case KB_F6: return "F6";
          case KB_F7: return "F7";
          case KB_F8: return "F8";
          case KB_F9: return "F9";
          case KB_F10:return "F10";
          case KB_F11:return "F11";
          case KB_F12:return "F12";
          case KB_C_S:return "C-S";
     default:
     if ((s >= 'a' && s<= 'z') || (s<=127 && s >' ')) return string("")+(char)s;
     return "(ERROR)";
     }
     return "(ERROR)";
}


string upper(string i) {
     for (uint x=0;x<i.length();x++) i[x]=toupper(i[x]);
     return i;
}

void bootlog(string n,char*file,int line) {
     fprintf(stdout,"[%s:%d] %s\n", file,line,n.c_str());
}

void map_draw_windows() {
     int w;
     if (draw_only_root) {
       draw_lwin(buffer,windows[0]);
     } else {
       for (w=0;w<MAX_LWIN;w++)
          if (windows[w].style && (windows[w].flags & LWIN_FL_ACTIVE))
             draw_lwin(buffer,windows[w]);
     }
}

void onlyroot_off() {
     draw_only_root=0;
     for(int w=1;w<MAX_LWIN;w++)
          if ((windows[w].flags & LWIN_FL_WASACTIVE))
               {if (windows[w].style) windows[w].flags |= LWIN_FL_ACTIVE;
                windows[w].flags &= ~LWIN_FL_WASACTIVE;
               }
     map_draw_windows();
     force_fs();
}

void onlyroot_on() {
     draw_only_root=1;
     for(int w=1;w<MAX_LWIN;w++)
          if (windows[w].flags & LWIN_FL_ACTIVE)
               {if (windows[w].style) windows[w].flags |= LWIN_FL_WASACTIVE;
                windows[w].flags &= ~LWIN_FL_ACTIVE;
               }
     map_draw_windows();
     force_fs();
}

int limphost_interrupt() {
     if (keypressed()) return 1; else return 0;
}

int maptobindspace(int k) {
     switch (k) {
          case K_F1: return KB_F1;
          case K_F2: return KB_F2;
          case K_F3: return KB_F3;
          case K_F4: return KB_F4;
          case K_F5: return KB_F5;
          case K_F6: return KB_F6;
          case K_F7: return KB_F7;
          case K_F8: return KB_F8;
          case K_F9: return KB_F9;
          case K_F10: return KB_F10;
          case K_F11: return KB_F11;
          case K_F12: return KB_F12;
     default:  return k&127;
     }
}

void status(string msg, int ticks=144) {
     windows[W_STATUS].text = msg;
     windows[W_STATUS].alarm = msgclear;
     windows[W_STATUS].nextalarm = retrace_count + ticks;
}

void map_config(const char * cfgfile ) {
     dictionary conf; /* read file called config */
     conf.readfile(cfgfile);
     DATAF = conf.lookup("DATAFILE");
     NUMTILES = atoi(conf.lookup("NUMTILES").c_str());
     XTBITS = atoi(conf.lookup("XTBITS").c_str());
     YTBITS = atoi(conf.lookup("YTBITS").c_str());

     XTILES = atoi(conf.lookup("XTILES").c_str());
     YTILES = atoi(conf.lookup("YTILES").c_str());

     DEFX   = atoi(conf.lookup("DEFX").c_str());
     DEFY   = atoi(conf.lookup("DEFY").c_str());

     SELH   = atoi(conf.lookup("SELH").c_str());
     SELW   = atoi(conf.lookup("SELW").c_str());

     TILESW = atoi(conf.lookup("TILESW").c_str());

//     printf("xtbits: %d ytbits: %d\n", XTBITS, YTBITS);


     XTMASK = (1<<XTBITS)-1;
     YTMASK = (1<<YTBITS)-1;
     XTSIZE = (1<<XTBITS);
     YTSIZE = (1<<YTBITS);

}

