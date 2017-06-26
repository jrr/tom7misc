
#pragma pack(2)

#include "learn.h"
#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>
#include <System/Unix/unix_stdio.h>

int form;

#define SCREENW 32

/* could be 10 lines */
#define SCREENH 9

#define TOPX 2
#define TOPY 12

#define TEXTHEIGHT 12
#define MAXCHARW 8

#define CLEARW (MAXCHARW * 18)
#define CLEARH (SCREENH * TEXTHEIGHT)

void drawscreen (), scrollup(), addstring(char*);
int stringlen(char*), Tmin(int,int);

static char * screen = 0; /* [SCREENW * SCREENH] = {0}; */
static char screenchanged = 0;

static unsigned char steps = 0;

static Word blankness[4]={0,0,0,0}; /* white pattern */

static RectangleType winclearme;

char slime[128];

DWord PilotMain(Word cmd, Ptr cmdPBP, Word launchFlags) {
  EventType e;
  FormType *pfrm;
  VoidHand vh;
  short x;

  if (!cmd) {
    vh = MemHandleNew(SCREENW*SCREENH+1);
    screen = (char*) MemHandleLock (vh);
    for (x=0; x<=SCREENH*SCREENW; x++) screen[x]=0;

    winclearme.topLeft.x = TOPX;
    winclearme.topLeft.y = TOPY;

    winclearme.extent.x  = CLEARW;
    winclearme.extent.y  = CLEARH;

    form = learnform;
    FrmGotoForm(form);
      
    while(1) 
      {
	EvtGetEvent(&e, 100);

	sprintf(slime,"[%d]", e.eType);
	addstring(slime);
	drawscreen();
#if 0
	if (SysHandleEvent(&e)) 
	  continue;
	if (MenuHandleEvent((void *)0, &e, &x)) 
	  continue;
#endif 	
	switch (e.eType) 
	  {
	  case keyDownEvent:
	    sprintf(slime,"(%d)(%d)(%d)", e.data.keyDown.chr,
		    e.data.keyDown.keyCode,
		    e.data.keyDown.modifiers);
	    addstring(slime);
	    drawscreen();
	  case ctlSelectEvent:
	    switch (e.data.ctlSelect.controlID) {
	    case okbutton:
	      /* exit */
	      goto get_out;
	    case clickme:
	      switch ((steps++)&3) {
	      case 0:
		addstring("AAAAAA BBBBBBB CCCCCC DDDDD EEEE FFFFFF GGGGGG HH");
		// 	  addstring("The wheels on the bus go round and round.");
		break;
	      case 1:
		addstring("iiiiiiiiiiiiiiiiiiiiiiiii iiiiiijj");
		//		addstring("iiiiiiiii jjjjjjjjjj iiiiiiiii jjjjjjjj ii jj ii jj");
	      
		//		  addstring("A dragon is just a frog that has eaten a fire scroll.");
		break;
	      case 2:
		addstring("Morning stars do not work in the evening.");
		break;
	      case 3:
		addstring("Caution: Trap Door.");
		break;
	      }
	      break;
	    default:
	      break;
	    }
	    drawscreen();
	    goto Dft;
	    break;
		
	  case frmLoadEvent:
	    FrmSetActiveForm(FrmInitForm(e.data.frmLoad.formID));
	    break;
	
	  case frmOpenEvent:
	    pfrm = FrmGetActiveForm();
	    FrmDrawForm(pfrm);
	    break;

	  case menuEvent:
	    break;

	  case appStopEvent:
	    goto get_out;

	  default:
	  Dft:
	    FrmHandleEvent(FrmGetActiveForm(), &e);
	  }
      }
  get_out:
    MemHandleUnlock(vh);
    MemHandleFree(vh);
    return 0;
  }
  
  return 0;
}

void drawscreen() {
  int x,y;

  if (!screenchanged) return;

  WinSetPattern(blankness);
  WinFillRectangle(&winclearme,0);

  for (y=0;y<SCREENH;y++) {
    x = Tmin(stringlen(&screen[y*SCREENW]),SCREENW);
    WinDrawChars(&screen[y*SCREENW],x,      TOPX,TOPY+(TEXTHEIGHT*y));
  }
}

void scrollup() {
  int x,y;
  for (y=0;y<(SCREENH-1);y++)
    for (x=0;x<SCREENW;x++)
      screen[(y*SCREENW)+x] = screen[((1+y)*SCREENW)+x];

  /* clear bottom line */
  for (x=0;x<SCREENW;x++) screen[((SCREENH-1)*SCREENW)+x] = 0;
}

void addstring(char * s) {
  SWord y,z;
  int x=0,l=stringlen(s),pls=0, orgy;
  Boolean leave = 0;
  while ((!leave) && (x < l)) {
    scrollup();
    pls = 0;
    y = l-x;
    z = CLEARW;
    FntCharsInWidth ( s + x, &z, &y, &leave );
    if (y > SCREENW) {y = SCREENW; leave =0;}
    orgy = y;
    if (!leave)
      for (;y>0;y--) {
	if (s[x+y] == ' ') { pls = 1;break; }
      }
    if (!y) y = orgy;
    for (z=0;z<y && s[x+z];z++)
      screen[(SCREENH-1)*SCREENW + z] = s[x+z];
    x += y + pls;
  }

#if 0
  for (x=0;x<l;x+=SCREENW) {   
    scrollup();
    for (y=x;y<l && (y-x)<SCREENW;y++)
      screen[(SCREENH-1)*SCREENW + (y-x)] = s[y];
   
  }
#endif
  screenchanged = 1;
}

int stringlen(char * s) {
  int x;
  for (x=0;s[x];x++); return x;
}

int Tmin(int a, int b) {return (a>b)?b:a;}

