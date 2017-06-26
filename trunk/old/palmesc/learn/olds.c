
#pragma pack(2)

#include "learn.h"
#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>

int kidForm;

#define SCREENW 25
#define SCREENH 5

#define TOPX 2
#define TOPY 20

#define TEXTHEIGHT 16

void drawscreen (), scrollup(), addstring(char*);
int stringlen(char*), Tmin(int,int);

static char * screen = 0; /* [SCREENW * SCREENH] = {0}; */
static char screenchanged = 0;

static unsigned char steps = 0;

DWord PilotMain(Word cmd, Ptr cmdPBP, Word launchFlags) {
  EventType e;
  FormType *pfrm;
  VoidHand vh;
  short x;

  if (!cmd) {
    vh = MemHandleNew(SCREENW*SCREENH);
    screen = (char*) MemHandleLock (vh);
    for (x=0;x<SCREENH*SCREENW;x++) screen[x]=0;

      kidForm = kidForm1;
      FrmGotoForm(kidForm);

      while(1) 
	{
	  EvtGetEvent(&e, 100);
	  if (SysHandleEvent(&e)) 
	    continue;
	  if (MenuHandleEvent((void *)0, &e, &x)) 
	    continue;
	
	  switch (e.eType) 
	    {
	    case ctlSelectEvent:
	      switch (e.data.ctlSelect.controlID) {
	      case kidOk:
		/* exit */
		goto get_out;
	      case bitch:
		switch ((steps=steps*7-11)&7) {
		case 0:
		  addstring("fuck bitch!"); break;
		case 1:
		  addstring("long mutherfucken string!! FUCK THAT BITCH BITCH!"); break;
		case 2:
		  addstring("--- check it out ---");
		  break;
		case 3:
		  addstring("break virgins, then bones.");
		  break;
		case 4:
		  addstring("Fucken snazless bitch-ass shit.");
		  break;
		case 5:
		  addstring("Fungal nail infections."); break;
		case 6:
		  addstring("Bitch ass niggas on my african scrotum!");
		  break;
		case 7:
		  addstring("Placenta!");
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
    }
  
 get_out:
  MemHandleUnlock(vh);
  MemHandleFree(vh);

  return 0;
}

void drawscreen() {
  int x,y;

  if (!screenchanged) return;

  // WinSetPattern
  //  WinFillRectangle

  for (y=0;y<SCREENH;y++)
    WinDrawChars(&screen[y*SCREENW],Tmin(stringlen(&screen[y*SCREENW]),SCREENW),TOPX,TOPY+(TEXTHEIGHT*y));
}

void scrollup() {
  int x,y;
  for (y=0;y<(SCREENH-1);y++)
      for (x=0;x<SCREENW;x++)
	screen[(y*SCREENW) + x] = screen[((1+y)*SCREENW) +x];

  /* clear bottom line */
  for (x=0;x<SCREENW;x++) screen[((SCREENH-1)*y) + x] = 0;
}

void addstring(char * s) {
  int x,y,l=stringlen(s);
  for (x=0;x<l;x+=SCREENW) {
    scrollup();
    for (y=x;y<l && (y-x)<SCREENW;y++)
      screen[(SCREENH-1)*SCREENW + (y-x)] = s[y];
  }
  screenchanged = 1;
}

int stringlen(char * s) {
  int x;
  for (x=0;s[x];x++); return x;
}

int Tmin(int a, int b) {(a>b)?b:a;}
