// --=[TP]=-----------------------------------------------------------------
//              TomPaint II Enchanted - Menu Stuffs
// ------------------------------------------------------------=[menu.cc]=--

#include "tp.h"
int about_proc();
int quit_proc();
int load_proc();

MENU file_menu[] =
{
   {"&Load",                     load_proc,        NULL },
   {"",                          NULL,             NULL },
   {"&Quit",                     quit_proc,        NULL },
   {NULL,                        NULL,             NULL }
};

MENU about_menu[] =
{
   {"&About TPIIe",              about_proc,       NULL },
   {NULL,                        NULL,             NULL }
};

MENU stuff_menu[] =
{
   {"&Refresh",                  (int(*)())drawscreen,       NULL },
   {NULL,                        NULL,             NULL }
};


MENU brush_menu[] =
{
   {"&Quit",                     quit_proc,        NULL },
   {NULL,                        NULL,             NULL }
};

MENU main_menu[] =
{
   { "=",                        NULL,             about_menu },
   { "&File",                    NULL,             file_menu },
   { "&Brush",                   NULL,             brush_menu },
   { "",                         NULL,             NULL },
   { "&Stuff",                   NULL,             stuff_menu },
   { NULL,                       NULL,             NULL }
};

DIALOG dialogues[] =
{
   { d_menu_proc,       0,    0,    0,    0,    255,  0,    0,    0,       0,    0,    main_menu },
   { NULL,              0,0,0,0,                  0,  0,0,0,0,0,0}
};

//   do_dialog(dialogues, -1);
DIALOG_PLAYER * mymenu;
void startmenus() {
   mymenu  = init_dialog(dialogues, -1);
}
void domenus() { update_dialog(mymenu); }
void redrawmenus() {
//     SEND_MESSAGE(main_menu,MSG_DRAW,0);
       breakmenus();
       startmenus();
}
void breakmenus() { shutdown_dialog(mymenu); }

int about_proc() {
    alert("   TomPaint II Enchanted v" VERSION,
          "      Copyleft 1998 Tom 7",
          "Uses Allegro by S. Hargreaves",
          "Got it",0,13,0);
}

int load_proc() {
    loadfile();
}

int quit_proc() {
   if (1==alert("Sure you want to Quit?",0,0,"Yep","Nope",'y','n'))
       quit();
}
