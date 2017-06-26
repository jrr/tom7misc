
/* Header file for ludus window bits. */

#ifndef LUDUS_WINDOW_H
#define LUDUS_WINDOW_H
#include <map.h>
#include <global.h>
#include <string>

#define LWINSTYLE_ROOT 1
#define LWINSTYLE_LAME 2
#define LWINSTYLE_TRANSLAME 3
#define LWINSTYLE_TRANSTEXT 4
#define LWINSTYLE_CONSOLE 5
#define LWINSTYLE_GENERAL 6 /* generalized window */

#define MAX_LWIN 25

#define LWIN_CON_SCROLLBACK 128

#define LWIN_FL_RESIZABLE 1
#define LWIN_FL_ACTIVE 2 // That is, being drawn and accepting keypresses...
#define LWIN_FL_WASACTIVE 4 // when hiding all, set this if we turn it back
                            // on with a TAB.
// macros:
#define lwin_set_dirty(wzz) thisisdirty((wzz)->x,(wzz)->y,(wzz)->w,(wzz)->h)

enum LWIDGTYPE { LW_NOTHING, LW_BUTTON, LW_EDITBOX, LW_LABEL, };

typedef struct lwin lwin; /* there's a puzzler. */


struct lwidget {
     void * widg;
     int x,y,w,h;
     lwidget * next;
     LWIDGTYPE type;
};

struct lw_description {
     int color;
     string title;
};

struct lw_label {
     string text;
};

#define BUTTON_PRESSED 1
#define BUTTON_OK 2
#define BUTTON_CANCEL 4
#define BUTTON_DRAW_TEXT 8
#define BUTTON_DRAW_PIC 16

struct lw_button {
     int flags;
     int id;
     int xtext, ytext;
     BITMAP * pic;
     void (*proc)(int);
     string text;
};

struct lw_editbox {
     /* height, width? */
     int cursor;
     int first;
     string contents;
};

struct lwin {
     int x,y,h,w;
     int lb,rb,tb,bb;   /* left, right, top and bottom border widths. */
     int style,color;
     int killme;
     int nextalarm, alarmnum;

     /* FIXME: Make a destructor which deletes this crap. */

     int flags;

     int scrollbar, scrollmax, scrollpos;

     string title;                      /* fancy string title */
     BITMAP * canvas;

     void (*paint)  (lwin*),          /* Draw contents to 'canvas' */
          (*born)   (lwin*),          /* Called when window is born. */
          (*die)    (lwin*),          /* Called before destroying window */
          (*mouseup)(lwin*),          /* following 2 just for capture */
          (*mousemove)(lwin*, int,int);
     int  (*keypressed   )(lwin*,int),
          (*alarm        )(lwin*,int),
          (*clicked      )(lwin*,int,int,int); /* relative to canvas */
  
     int data[8];   /* persistant undefined data for handlers */
     string inputline; /* inputline for console window */
     void * pointera,  /* pointer to widgets list or general purpose */
          * pointerb;  /* generalized: pointer to active widget */
     string text;
     lmap map;      /* more convenient w/o pointers,
                       and it's not too big of a struct. */
};


lwin create_null_lwin (int x, int y);

lwin create_generalized_lwin (int x, int y,
                              int w, int h,
                              lw_description desc);

lwin create_lame_lwin (int w, int h, /* FIXME: order of args */
                       int x, int y,
                       int color,
                       char* title);

lwin create_console_lwin (int x, int y,
                          int w, int h,
                          int color,
                          char * title,
                          char * firstline);

lwin create_transtext_lwin (int x, int y,
                               int retraces,
                               char* text);

int lwin_do_windows(int,int,int);

extern lwin windows[MAX_LWIN];

void draw_lwin(BITMAP*,lwin),
     win_draw_windows(),
     lwin_init();

int lwin_killme(lwin* self, int s);

void lconsole_addline   (lwin * sob, string addme),
     lconsole_addstring (lwin * sob, string addme);

int generalized_click (lwin* me, int x, int y, int mb);
void generalized_paint (lwin*me);
void generalized_mouseup (lwin * me);
void generalized_mousemove (lwin * me, int mx, int my);

void lwin_set_capture(lwin* w);

void add_widget (lwin * who, void * what, LWIDGTYPE type,
                 int x, int y, int w, int h);

void widget_labelpaint(BITMAP * cvs, lw_label * wg, int x, int y);
void widget_buttonpaint(BITMAP * cvs, lw_button * wig, int x, int y, int w, int h);
void widget_editboxpaint(BITMAP * cvs, lw_editbox * wg, int x, int y, int w, int h);



#endif
