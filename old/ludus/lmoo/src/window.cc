/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */

#include <time.h>
#include <global.h>
#include <allegro.h>

/* */
//     void limpdprint(string);
/* */

lwin windows[MAX_LWIN];

int has_capture=-1; /* -1: No window has mouse capture
                        0...MAX_LWIN-1: this window receives
                        mousemoved events until the window
                        releases capture or the mouse buttons
                        go up. */

int hwd=0,hws=0,hx=0,hy=0,hwr=0;
static int drawflag;
int lwin_do_windows(int mx, int my,int mb) {
     int w,rc=retrace_count;
     drawflag = 0;
      /* Process voluntary window deaths. */

     for (w=0;w<MAX_LWIN;w++)
          if (windows[w].style && windows[w].killme) {
               /* this window wants out. */
               windows[w].style = 0;   /* enough for now. */
               force_fs(); /* thisisdirty doesn't work cuz of
                              transtext with width/height 0 */
               ++drawflag;
          }

     
     for (w=0;w<MAX_LWIN;w++)
          if (windows[w].style
           && windows[w].nextalarm
           && (windows[w].nextalarm < rc)) {
               /* alarm time! */

/*               set_gfx_mode(GFX_TEXT,0,0,0,0);
               printf("On window %d, nextalarm is %d and rc is %d\n",
                      w,windows[w].nextalarm,rc);
               exit(0);
*/
               windows[w].nextalarm = 0;
               if (windows[w].alarm)
                    windows[w].alarm(&windows[w],windows[w].alarmnum);
          }

     /* process keypresses */

     if (keypressed()) {
          int k = readkey();

/*          if ((k&0xFF) == 27) windows[0].killme++; */

          for(w=MAX_LWIN-1;w>=0;w--) {
               if (windows[w].style
               &&  (windows[w].flags & LWIN_FL_ACTIVE)
               &&  windows[w].keypressed) {
                    if(windows[w].keypressed(&windows[w],k)) break;
               }
          }

     }

     if (has_capture >= 0) {
       /* a window has captured the mouse pointer; send it mouse_moved
	  for the current position, unless the mouse button has gone
	  up! */

       if (mb & 3) {
//         limpdprint("Calling mousemove");
         if (windows[has_capture].mousemove) /* actually calls it whether */
            (windows[has_capture].mousemove) /* it's moved or not! */
               /* with args relative to the canvas! */
           (&windows[has_capture],
           mx - (windows[has_capture].x + windows[has_capture].lb),
           my - (windows[has_capture].y + windows[has_capture].tb));
       if (drawflag) { win_draw_windows(); drawflag=0; }
       return 1;
       } else {
//         limpdprint("Calling mouseup");
         if (windows[has_capture].mouseup)
            (windows[has_capture].mouseup)
           (&windows[has_capture]);
         has_capture = -1;
//          limpdprint("removed capture");
       if (drawflag) { win_draw_windows(); drawflag=0; }
       return 1;
       }

     }

     if (drawflag) { win_draw_windows(); drawflag=0; }

     if (hwr) {
          if (mb & 1) {
               int iw = windows[hwr].w,
                   ih = windows[hwr].h;

               windows[hwr].w = (mx+(windows[hwr].rb-hx)) - (windows[hwr].x);
               windows[hwr].h = (my+(windows[hwr].bb-hy)) - (windows[hwr].y);

               if (windows[hwr].w < 125) windows[hwr].w = 125;
               if (windows[hwr].h < 75) windows[hwr].h = 75;
               /* some arbitrary resize limits; later maybe user-set? */

               /* this is dirty now: */

               destroy_bitmap(windows[hwr].canvas);
               windows[hwr].canvas =
                    create_bitmap(windows[hwr].w
                                   -(windows[hwr].rb+windows[hwr].lb),
                                  windows[hwr].h
                                   -(windows[hwr].tb+windows[hwr].bb));
               /* ack. */
               thisisdirty(windows[hwr].x,windows[hwr].y,
                           max(windows[hwr].w,iw),
                           max(windows[hwr].h,ih)); /* whole window. */
               if (windows[hwr].paint)
                    windows[hwr].paint(&windows[hwr]);
               win_draw_windows();
               return 1;
          } else {
               hwr = hy = hx = 0;
          }
     }
     if (hws) {
          if (mb & 1) {
     
          int sh = windows[hws].h-(windows[hws].bb+windows[hws].tb),
              sy = windows[hws].y+windows[hws].tb,
              os = windows[hws].scrollpos;

          if (my < sy) windows[hws].scrollpos = 0;
          else if (my > sy+sh) windows[hws].scrollpos = windows[hws].scrollmax;
          else {
               windows[hws].scrollpos = (int) (windows[hws].scrollmax*((my-sy)/(float)sh));
          }
          
          if (os != windows[hws].scrollpos) {
               thisisdirty(windows[hws].x,windows[hws].y,
                           windows[hws].w,windows[hws].h); /* whole window. */
               if (windows[hws].paint)
                    windows[hws].paint(&windows[hws]);
               win_draw_windows();
          }

          } else {
               hws = hx = hy = 0;
          }

     } else if (hwd) {
       if (mb & 1) {
                       dirtyspritemove(windows[hwd].x,
                                       windows[hwd].y,
                                       mx-hx, my-hy,windows[hwd].w,
                                                    windows[hwd].h);
                windows[hwd].x=mx-hx;
                windows[hwd].y=my-hy;
                         
                win_draw_windows();
                return 1;

       } else {
                       dirtyspritemove(windows[hwd].x,
                                       windows[hwd].y,
                                       mx-hx, my-hy,windows[hwd].w,
                                                    windows[hwd].h);
                windows[hwd].x=mx-hx;
                windows[hwd].y=my-hy;

               win_draw_windows();
                hx = hwd = hy = 0;
       }

     } else {

     if (mb) {
        for (w=MAX_LWIN-1;w>=0;w--) {
            if (windows[w].style && (windows[w].flags & LWIN_FL_ACTIVE)) {

             if ((mb &1) && mx > windows[w].x && mx < windows[w].x+windows[w].w
              && my > windows[w].y && my < windows[w].y+windows[w].tb) {
             /* engage dragging */
                hx = mx - windows[w].x;
                hy = my - windows[w].y;
                hwd = w;
                return 1;
              } else if ((mb &1) && (windows[w].flags & LWIN_FL_RESIZABLE)
                     &&  mx  > (windows[w].x +windows[w].w-windows[w].rb)
                     &&  mx <  (windows[w].x +windows[w].w)
                     &&  my  > (windows[w].y +windows[w].h-windows[w].bb)
                     &&  my <  (windows[w].y +windows[w].h)
                        ) {
                hwr = w;
                hx = mx  - (windows[w].x + windows[w].w-windows[w].rb);
                hy = my  - (windows[w].y + windows[w].h-windows[w].bb);
                return 1;
              } else if ((mb &1) && windows[w].scrollbar
                     &&  mx  > (windows[w].x +windows[w].w -windows[w].rb)
                     &&  mx <  (windows[w].x +windows[w].w)
                     &&  my  > (windows[w].y +windows[w].tb)
                     &&  my <  (windows[w].y +windows[w].h -windows[w].bb)
                        ) {
                hws = w;
                /* waits until next time through to change? sux. */
                return 1;
              } else if (mx > windows[w].x && mx < (windows[w].x+windows[w].w)
                     &&  my > windows[w].y && my < (windows[w].y+windows[w].h)) {
                /* call clicked */


                if (windows[w].clicked)
                  windows[w].clicked(&windows[w],
                                     mx - (windows[w].x+windows[w].lb),
                                     my - (windows[w].y+windows[w].tb),
                                     mb
                                    );

                         return 1; /* don't grab through windows. */
              }
           }
        }
     } 
    }
return 0;
}

void win_draw_windows () {
     int w;
     for (w=0;w<MAX_LWIN;w++)
        if (windows[w].style && (windows[w].flags & LWIN_FL_ACTIVE))
           draw_lwin(buffer,windows[w]);
}

void lwin_init() {
     int x;
     for (x=0;x<MAX_LWIN;x++) { windows[x].canvas = NULL;
                                windows[x].paint = NULL;
                                windows[x].clicked = NULL;
                                windows[x].keypressed = NULL;
                                windows[x].born = NULL;
                                windows[x].die = NULL;
                                windows[x].style=0;
                              }
}
/* why does this take a w copied? How about lwin & w? */
void draw_lwin(BITMAP * b, lwin w) {
     int /*d = text_height(bigfont), */
         h = ftextlen(bigfont,w.title.c_str()),tx;

     if (!(w.flags & LWIN_FL_ACTIVE)) return;

     switch(w.style) {
       case LWINSTYLE_TRANSTEXT:
       case LWINSTYLE_TRANSLAME:
       case LWINSTYLE_ROOT:
     break;

     default:
        hline(b,w.x,w.y,w.x+w.w-1,w.color);
        vline(b,w.x,w.y,w.y+w.h-1,w.color);

        hline(b,w.x+w.lb,w.y+w.h-w.bb,w.x+w.w-w.rb,w.color);
        vline(b,w.x+w.w-w.rb,w.y+w.tb,w.y+w.h-w.bb-1,w.color);

        /* rectfill for rb, lb, bb */
        /* left */
        rectfill(b,w.x+1,w.y+1,w.x+w.lb-1,w.y+w.h-1,w.color+1);
        /* top */
        rectfill(b,w.x+1,w.y+1,w.x+w.w -1,w.y+w.tb-1,w.color+1);

        /* right */
        rectfill(b,w.x+1+w.w-w.rb,w.y+1,w.x+w.w-1,w.y+w.h-1,w.color+1);
        /* bottom */
        rectfill(b,w.x+1,w.y+1+w.h-w.bb,w.x+w.w-1,w.y+w.h-1,w.color+1);

        vline(b,w.x+w.lb-1,w.y+w.tb,w.y+w.h-w.bb-1,w.color+3);

        hline(b,w.x+w.lb-1,w.y+w.tb-1,w.x+w.w-w.rb,w.color+3);

        hline(b,w.x+1,w.y+w.h-1,w.x+w.w-1,w.color+3);

        vline(b,w.x+w.w-1,w.y+1,w.y+w.h-1,w.color+3);

     /* scrollbot: */
     if (w.scrollbar) {
          int sh = w.h-(w.bb+w.tb+3),
              sx = w.x+w.w-w.rb,
              sy = w.y+w.tb;

          int sby=
          (w.scrollmax)?
          (1+sy + (int)((sh-(w.rb-2))*(w.scrollpos/(float)w.scrollmax)))
          :(1+sy);

          rect    (b,sx+1,sby,sx+(w.rb-2),sby+(w.rb-2),w.color+3);
          rectfill(b,sx+2,sby+1,sx+(w.rb-3),sby+(w.rb-3),w.color);

      }

     break;
     }
   switch (w.style)  {
   case LWINSTYLE_TRANSTEXT:
         if (w.text != "") ftextout(b,bigfont,w.text.c_str(),w.x,w.y);
   break;
   case LWINSTYLE_GENERAL:

     { 
          // nuthin'
//        hline

        blit (w.canvas,b,0,0,w.x+w.lb,w.y+w.tb,w.w-(w.lb+w.rb),w.h-(w.tb+w.bb));

      }
   break;
   case LWINSTYLE_LAME:

        // Lame linedraw window.
/*
        rectfill(b,w.x,w.y,w.x+w.w,w.y+w.h,0);
*/
   case LWINSTYLE_TRANSLAME:
/*
        rect    (b,w.x+3,w.y+9,w.x+w.w-3,w.y+w.h-3,w.color);
        rect    (b,w.x+5,w.y+11,w.x+w.w-5,w.y+w.h-5,w.color);
*/
#if 0
        if (w.scrollbar) {
          int sw = w.rb,
              sh = w.h-(w.bb+w.tb),
              sx = w.x+w.w-w.rb,
              sy = w.y+w.tb;
          
          int sby=
          (w.scrollmax)?
          (1+sy + (int)((sh-(w.rb-2))*(w.scrollpos/(float)w.scrollmax)))
          :(1+sy);
               
          /* draw scrollbar. */
//          rectfill(b,sx,sy,sx+sw,sy+sh,0);

          /* draw scrollbot */

          rectfill(b,sx+1,sby,sx+(w.rb-2),sby+(w.rb-2),w.color);

        }
#endif
        blit (w.canvas,b,0,0,w.x+w.lb,w.y+w.tb,w.w-(w.lb+w.rb),w.h-(w.tb+w.bb));

     if (w.title != "") {
          /* FIXME: calculate this every draw? == slow! */
          tx = (w.x+w.w/2)-((h=ftextlen(smallfont,w.title.c_str()))>>1);

//          rectfill(b,tx,w.y,tx+h,w.y+d,0);

          ftextout(b,smallfont,w.title.c_str(),(w.x+w.w/2)-(h>>1),w.y);
     }


   break;
   case LWINSTYLE_CONSOLE:
        tx = (w.x+w.w/2)-(h>>1);

     if (w.title != "") {
          /* waste of time: */
          tx = (w.x+w.w/2)-((h=ftextlen(smallfont,w.title.c_str()))>>1);
/*
          hline (b,w.x+5,w.y+5,tx-2,w.color);
          hline (b,tx + h + 2,w.y+5,w.x+w.w-5,w.color);
*/
          ftextout(b,smallfont,w.title.c_str(),(w.x+w.w/2)-(h>>1),w.y+1);
     } else {
//          hline(b,w.x+5,w.y+5,w.y+w.w-5,w.color);
     }
/*
          vline (b,w.x+5,w.y+5,w.y+w.h-5,w.color);
          vline (b,w.x+w.w-5,w.y+5,w.y+w.h-5,w.color);

          hline (b,w.x+5,w.y+w.h-5,w.x+w.w-5,w.color);


          rect (b,w.x+w.rb-1,w.y+w.tb-1,w.x+w.w-w.rb,w.y+w.h-w.bb,1);
*/
          /* draw scrollbot */

        blit(w.canvas,b,0,0,w.x+w.lb,w.y+w.tb,
                    w.w-(w.rb+w.lb),w.h-(w.tb+w.bb));

   break;
   case LWINSTYLE_ROOT:
        // root window.

        blit (w.canvas,b,0,0,0,0,640,480);
   break;
   default:
   break;
   }
}

lwin create_null_lwin (int x, int y) {

     lwin yo;

     yo.x = x;
     yo.y = y;

     yo.tb =
     yo.bb =
     yo.rb = yo.lb = 

     yo.w = yo.h =

     yo.scrollbar = 
     yo.scrollmax = 
     yo.scrollpos = 


     yo.nextalarm =
     yo.alarmnum =
     yo.killme = 0;

     yo.flags = LWIN_FL_ACTIVE;

     yo.paint = NULL;
     yo.keypressed = NULL;
     yo.clicked = NULL;
     yo.born = NULL;
     yo.die = NULL;

     yo.pointera = yo.pointerb = NULL;

     yo.alarm = NULL;

     yo.text = "";
     yo.title = "";

     yo.canvas = NULL;

     return yo;

}

lwin create_lame_lwin (int w, int h,
                       int x, int y,
                       int color,
                       char* title) {

     lwin yo = create_null_lwin(x,y);
     yo.w = w;
     yo.h = h;

     yo.style = LWINSTYLE_LAME;

     yo.tb = 12;
     yo.bb =
     yo.lb = 3; yo.rb = 8;

     yo.color = color;
     yo.title = title;

     if ((yo.canvas = create_bitmap (w-(yo.lb+yo.rb), h-(yo.tb+yo.bb))))
         clear_to_color(yo.canvas,0);
     else { printf("Couldn't allocate memory for window bitmap!\n");
            exit(-1);
          }

     return yo;

}

lwin create_console_lwin (int x, int y,
                          int w, int h,
                          int color,
                          char * title,
                          char * firstline) {

     lwin yo = create_null_lwin(x,y);
     yo.style = LWINSTYLE_CONSOLE;
     yo.title = title;

     yo.color = color;

     yo.x = x; yo.y = y; yo.w = w; yo.h = h;

     yo.tb = 12;
     yo.bb = 3;
     yo.rb = 8;
     yo.lb = 3;

     yo.scrollbar = 1;
     yo.scrollmax = 0;
     yo.scrollpos = 0;

     yo.flags |= LWIN_FL_RESIZABLE;

     if ((yo.canvas = create_bitmap (w-(yo.lb+yo.rb), h-(yo.tb+yo.bb))))
         clear_to_color(yo.canvas,30);
     else { printf("Couldn't allocate memory for window bitmap!\n");
            exit(-1);
          }

     yo.pointera = (void *)
               new string[LWIN_CON_SCROLLBACK];

     yo.data[0] = 0; /* position of next inserted line */
     yo.data[1] = 0; /* number of entries */
     yo.data[2] = smallfont->dat.dat_prop->dat['w'-' ']->w; 
                     /* Character width */

     if (firstline) lconsole_addstring (&yo,(string)firstline);

     return yo;
}

lwin create_transtext_lwin (int x, int y,
                               int retraces,
                               char* text) {

     lwin yo = create_null_lwin(x,y);

     yo.style = LWINSTYLE_TRANSTEXT;

     yo.text = text;

     if (retraces) {
        yo.alarm = lwin_killme;
        yo.nextalarm = retrace_count + retraces;
        yo.alarmnum = 1; /* doesn't matter */
     }

     return yo;
}

int lwin_killme(lwin* self, int) {
     self->killme = 1;
     return 0;
}

void lconsole_addline (lwin * sob, string addme) {
     ((string*)sob->pointera)[sob->data[0]++] = addme;
     sob->data[0] %= LWIN_CON_SCROLLBACK;
     sob->data[1]++;
     if (sob->data[1] > LWIN_CON_SCROLLBACK) sob->data[1] = LWIN_CON_SCROLLBACK;
     /* change painting function? */

     sob->scrollpos =
     sob->scrollmax = sob->data[1];
}

void lconsole_addstring (lwin * yo, string addme) {
     /* in contrast to the function above, this one splits the incoming
        string into lines (at words where possible) and calls the above
        function to insert them. */

     /* in addition, this function has to pay attention to color codes,
        ignoring them when making length calculations, yet propagating
        them across lines. */

     uint numchars = ( (yo->w-(yo->lb+yo->rb)) /yo->data[2] ) - 1;

     string go;
     uint golen=0,tilde=0;
     uchar lastcode='0';
     for (uint x=0;x<addme.length();x++) {
          go += addme[x];
          if (addme[x] == '~') {
               tilde = 1;
          } else if (tilde) {
               lastcode = (uchar)addme[x];
               tilde=0;
          } else golen++;

          if (golen > numchars) {
               for (uint y=go.length()-1;y--;) {
                    if (go[y] == ' ') {
                         // found a place to break it.
                         lconsole_addline(yo,go.substr(0,y));
                         go = (string)"~"+(char)lastcode
                            + (string)(char*)(go.c_str()+y);
                         golen = fnumchars(go.c_str());
                         goto lconsole_addstring_out;
                    }
               }
               // got all the way to the bottom. Must be one long word?
               // (maybe go back and break at punctuation?)
               lconsole_addline(yo,go);
               go=(string)"~" + (char)lastcode;
               golen = 0;
               lconsole_addstring_out:;
          }
     }
     if (go != "") lconsole_addline(yo,go); /* add any remaining... */
}

lwin create_generalized_lwin (int x, int y,
                       int w, int h,
                       lw_description desc) {

     lwin yo = create_null_lwin(x,y);
     yo.w = w;
     yo.h = h;

     yo.style = LWINSTYLE_GENERAL;

     yo.tb = 8;
     yo.bb = 
     yo.rb = yo.lb = 3;

     yo.color = desc.color;
     yo.title = desc.title;

     yo.clicked = generalized_click;
     yo.paint   = generalized_paint;
     yo.mouseup = generalized_mouseup;
     yo.mousemove = generalized_mousemove;

     if ((yo.canvas = create_bitmap (w-(yo.lb+yo.rb), h-(yo.tb+yo.bb))))
         clear_to_color(yo.canvas,0);
     else { printf("Couldn't allocate memory for window bitmap!\n");
            exit(-1);
          }

     return yo;

}

void lwin_set_capture(lwin* w) {
     /* This is nasty: */
     int i = (int)w - (int)&windows[0];
     i /= sizeof (lwin);
     if (i >= 0 && i < MAX_LWIN) has_capture = i;
     /* Can't set capture on a window not in the array... */
}

void generalized_mousemove (lwin * me, int mx, int my) {
     lwidget * now = (lwidget*) me->pointerb;
     if (!now) {has_capture = -1; return; } // release capture? 
     switch (now->type) {
       case LW_BUTTON:{
          lw_button * bb = (lw_button *) now->widg;
          if (bb->flags & BUTTON_PRESSED) {
               if (! (mx > now->x && mx < (now->x+now->w)
                   && my > now->y && my < (now->y+now->h))) {
               /* outside the button area: */
               bb->flags &=~ BUTTON_PRESSED;
               lwin_set_dirty(me);
               widget_buttonpaint(me->canvas,bb,
                                   now->x,now->y,
                                   now->w,now->h);
               drawflag=1;
               }
          } else {
               if ((mx > now->x && mx < (now->x+now->w)
                   && my > now->y && my < (now->y+now->h))) {
               /* back inside the button area: */
               bb->flags |= BUTTON_PRESSED;
               lwin_set_dirty(me);
               widget_buttonpaint(me->canvas,bb,
                                   now->x,now->y,
                                   now->w,now->h);

               drawflag=1;
               }
          }
       break;}
       default:;
     }
}

void generalized_mouseup (lwin * me) {
     lwidget * now = (lwidget*) me->pointerb;
     if (!now) return;

     switch (now->type) {
          case LW_BUTTON:{
               lw_button * bb = (lw_button *) now->widg;
               if (bb->flags & BUTTON_PRESSED) {
               /* because the button can be up */
                    if (bb->flags & BUTTON_OK) {
                         /* do that crap */
                    } else if (bb->flags & BUTTON_CANCEL) {
                         /* or this crap */
                    } else if (bb->proc) {
                         bb->proc(bb->id);
                    }
               }
               bb->flags &=~ BUTTON_PRESSED;
               lwin_set_dirty(me); /* maybe just the button? */
               widget_buttonpaint(me->canvas,bb,
                                   now->x,now->y,
                                   now->w,now->h);

               drawflag=1;
          break;}
          default:;
     }
}

int generalized_click (lwin* me, int x, int y, int /*mb */) {
     lwidget * now = (lwidget *) me->pointera;

//     limpdprint("got generalized_click");

     while (now) {
          if (x >= now->x && y >= now->y
           && x < (now->x + now->w) && y < (now->y + now->h)) {
           switch (now->type) {
           case LW_BUTTON: {
//               limpdprint("Clicked on the button");
               /* get capture: */
               lwin_set_capture(me);
               /* set active widget */
               me->pointerb = (void*)now;
               lw_button * bb = (lw_button *) now->widg;
               bb->flags |= BUTTON_PRESSED;
               lwin_set_dirty(me);

               widget_buttonpaint(me->canvas,bb,
                                   now->x,now->y,
                                   now->w,now->h);
               /* guarantee feedback: */

               /* Nasty. FIXME */
               win_draw_windows();
               dirty_draw();

               drawflag=1;
               break;}
           case LW_NOTHING: /* ? */
           default:;
           /* return? */
           }
          }
          now = now -> next;
     }
     return 0;
}    

void add_widget (lwin * who, void * what, LWIDGTYPE type,
                 int x, int y, int w, int h) {

     lwidget * n = new lwidget;
     n->x = x; n->y = y;
     n->w = w; n->h = h;
     n->widg = what;
     n->type = type;
     n->next = (lwidget*) who->pointera;
     who->pointera = n;
}

void generalized_paint (lwin* me) {
     lwidget * now = (lwidget *) me->pointera;

//     printf("Generalized paint called with me->pointera = %p.\n", now);

     while (now) {
           switch (now->type) {
           case LW_EDITBOX:
               break;
           case LW_LABEL:
               widget_labelpaint(me->canvas,(lw_label*)now->widg,now->x,now->y);
               break;
           case LW_BUTTON:
               widget_buttonpaint(me->canvas,(lw_button*)now->widg,
                                   now->x,now->y,
                                   now->w,now->h);
               break;
           case LW_NOTHING: /* ? */
           default:;
           }
          now = now -> next;
     }
}

void widget_buttonpaint(BITMAP * cvs, lw_button * wig, int x, int y, int w, int h) {

     if (wig->flags & BUTTON_PRESSED) {

//     limpdprint("Drawing with button down.");

     rectfill(cvs,x,y,x+w,y+h,C_GREY1);

     hline(cvs,x,y,x+w,C_GREY0);
     hline(cvs,x,y+h,x+w,C_GREY2);
     vline(cvs,x,y,y+h,C_GREY0);
     vline(cvs,x+w,y,y+h,C_GREY2);

     if (wig->flags & BUTTON_DRAW_PIC)
       blit(wig->pic,cvs,0,0,x+2,y+2,w-2,h-2);

     if (wig->flags & BUTTON_DRAW_TEXT)
       ftextout(cvs, smallfont, wig->text.c_str(),x+wig->xtext+1,y+wig->ytext+1 );

     } else {

     rectfill(cvs,x,y,x+w,y+h,C_GREY2);

     hline(cvs,x,y,x+w,C_WHITE);
     hline(cvs,x,y+h,x+w,C_GREY1);
     vline(cvs,x,y,y+h,C_WHITE);
     vline(cvs,x+w,y,y+h,C_GREY1);

     if (wig->flags & BUTTON_DRAW_PIC)
       blit(wig->pic,cvs,0,0,x+1,y+1,w-1,h-1);

     if (wig->flags & BUTTON_DRAW_TEXT)
       ftextout(cvs, smallfont, wig->text.c_str(),x+wig->xtext,y + wig->ytext);

     }
}

void widget_labelpaint(BITMAP * cvs, lw_label * wig, int x, int y) {
//     printf("Tried to print a label at %p.\n", wig);

     if (wig)
          ftextout(cvs, smallfont, wig->text.c_str(), x,y);
}
