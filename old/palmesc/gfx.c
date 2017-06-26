
#pragma pack(2)

#include "gfx.h"
#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>
#include "esc.h"

#define brd(xxx,yyy) (board[((yyy)*MAXX)+(xxx)])

int form;

typedef struct {
  unsigned char t,x,y,z,o;
} tiletype;

static tiletype * board = 0;

static unsigned char guyx, guyy;

static unsigned char game[] = {
#include "esc2pesc/out.psc"
};

#define MAXX 18
#define MAXY 10
 
#define TOPLEFTX 6
#define TOPLEFTY 28

#define GUYIDX 37

static RectangleType rect;

#if 0
static Word whiteness[4]={0,0,0,0}; /* white pattern */
static Word blackness[4]={65535,65535,65535,65535}; /* black pattern */
#endif 

static WinHandle bitmaps;

#define BMPX (42<<3)
#define BMPY 8

void * tmalloc(int);
void tfree(void *);

void init();
void drawboard(), drawguy() /* (or girl) */, drawborder(),
  moveguy(char,char);

void releasepanel(char,char), paneldown(char,char), blockon(char,char,char),
  blockoff(char,char), swapall(char,char), become(char,char);

void printpair(int t, int q);

static char changed = 0;

DWord PilotMain(Word cmd, Ptr cmdPBP, Word launchFlags) {
  EventType e;
  FormType *pfrm;
  short x;
 
  if (!cmd) {

    init () ;
    while(1) 
      {
	EvtGetEvent(&e, 100);
	
	switch (e.eType) 
	  {
#if 1
	  case keyDownEvent:
	    switch (e.data.keyDown.chr | (e.data.keyDown.modifiers<<8)) {
	    case 30: /* up arrow */
	    case 11|(8<<8):
	    case 11|(136<<8):
	      moveguy(0,-1);
	      changed = 1;

	      break;
	    case 28: /* left */
	    case 517|(8<<8):
	    case 517|(136<<8):
	      moveguy(-1,0);
	      changed = 1;

	      break;
	    case 31: /* down */
	    case 12|(8<<8):
	    case 12|(136<<8):
	      moveguy(0,1);
	      changed = 1;

	      break;
	    case 29: /* right */
	    case 518|(8<<8):
	    case 518|(136<<8):
	      moveguy(1,0);
	      changed = 1;

	      break;
	    default: goto Dft;

	    }
	    if (changed) {

	      drawboard();
	      drawguy();
	      
	      drawborder();
	    }
	    break;
#endif
	  case ctlSelectEvent:
	    switch (e.data.ctlSelect.controlID) {
	    case okbutton:
	      /* exit */
	      goto get_out;
	    case otherboot:
	     
	      drawboard();
	      drawguy();
	      
	      drawborder();
		      
	      break;
	    default:
	      break;
	    }
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

	    if (SysHandleEvent(&e)) 
	      continue;
	    if (MenuHandleEvent((void *)0, &e, &x)) 
	      continue;

	
	    FrmHandleEvent(FrmGetActiveForm(), &e);
	  }
      }
  get_out:
    tfree(board);

    WinDeleteWindow(bitmaps, false);
    return 0;
  }
  
  return 0;
}

void drawguy() {
  rect.topLeft.y = 0;
  rect.extent.x = rect.extent.y = 8;
  rect.topLeft.x = GUYIDX << 3;
  WinCopyRectangle(bitmaps,WinGetActiveWindow(),&rect,TOPLEFTX+(guyx<<3),
		   TOPLEFTY+(guyy<<3),scrCopy);
}

void drawborder() {
  rect.topLeft.x = TOPLEFTX-1;
  rect.topLeft.y = TOPLEFTY-1;
  rect.extent.x = (MAXX << 3) + 2;
  rect.extent.y = (MAXY << 3) + 2;
  WinDrawRectangleFrame ( 1, &rect );
}

void drawboard () {
  int x,y;
  rect.topLeft.y = 0;
  rect.extent.x = rect.extent.y = 8;
  for (y=0;y<MAXY;y++)
    for (x=0;x<MAXX;x++) if (guyx != x || guyy != y) {      
      rect.topLeft.x = brd(x,y).t << 3;
      WinCopyRectangle(bitmaps,WinGetActiveWindow(),&rect,TOPLEFTX+x*8,
		       TOPLEFTY+y*8,scrCopy);
    }
}

/* works like standard malloc, asking for a new handle, locking it,
   and packing the handle in the allocation header. Later call tfree
   to free it as usual. */
void * tmalloc (int amount) {
  VoidHand vh = MemHandleNew(amount + sizeof (VoidHand));
  VoidHand * pointer = MemHandleLock(vh);
  if (!pointer) return 0;
  *pointer = vh;
  return &pointer[1];
}

/* call on NULL pointers or tmalloc'd pointers only */
void   tfree (void * p) {
  if (p) {
    VoidHand v = ((VoidHand *)p)[-1];
    MemHandleUnlock(v);
    MemHandleFree(v);
  }
}


void init () {
  short x,y;
  WindowType * wt;
  WinHandle tmphandle;
  VoidHand bitmaphandle;
  BitmapPtr bitmop;
  
  board = tmalloc(MAXX*MAXY*sizeof (tiletype));
  y=0;
  for(x=0;x<(MAXY*MAXX);x++)
    board[x].t = game[y++];
  for(x=0;x<(MAXY*MAXX);x++)
    board[x].x = game[y++];
  for(x=0;x<(MAXY*MAXX);x++)
    board[x].y = game[y++];

  guyx = game[y++];
  guyy = game[y++];
  
  for(x=0;x<MAXY;x++)
    for(y=0;y<MAXX;y++)
      brd(y,x).z =
	brd(y,x).o = T_NULL;

  form = mainf;
  FrmGotoForm(form);

  wt = WinGetWindowPointer (WinGetActiveWindow());

  bitmaps = WinCreateOffscreenWindow(BMPX, BMPY, screenFormat, &y);
  tmphandle = WinSetDrawWindow(bitmaps);
    
  bitmaphandle = DmGetResource('Tbmp', TILES_ID);
  bitmop = MemHandleLock(bitmaphandle);
  WinDrawBitmap(bitmop, 0, 0);
  MemHandleUnlock(bitmaphandle);
  DmReleaseResource(bitmaphandle);
  
  WinSetDrawWindow(tmphandle);

}

void moveguy (char dx, char dy) {
  unsigned char tile, twosafe;

  /* can't move off the screen: */
  if ((!guyx && dx < 0)
      || (!guyy && dy < 0)
      || (guyx + dx == MAXX)
      || (guyy + dy == MAXY)) return;
  
  twosafe =1;
  if ((dx == -1 && (guyx == 1))
      || (dy == -1 && (guyy == 1))
      || (dx == 1 && (guyx == (MAXX-2)))
      || (dy == 1 && (guyy == (MAXY-2)))) twosafe=0;
      
  switch (tile = brd(guyx+dx,guyy+dy).t) {
  case T_YELLOW:
    /* yellow block. If we can move it at all: */
    if (!twosafe) return;

#if 0
    switch (brd(guyx+dx+dx,guyy+dy+dy).t) {
    case T_FLOOR: case T_PANEL:
    default: return;
    }
#endif
    /* now move the yellow brick as far as it will go. */

    {
      int x = guyx+dx;
      int y = guyy+dy;
      
      /* magnificent all-purpose for loop! */
      for (;(dy || x>=1) && (dx || y>=1) 
	     && (dy || x<(MAXX-1)) && (dx || y< (MAXY-1)) 
	     && (brd(x+dx,y+dy).t == T_FLOOR
		 || brd(x+dx,y+dy).t == T_PANEL)
	     ;(x+=dx),(y+=dy)) {
	/* nil */
      }
      if (x != (guyx+dx) || y != (guyy+dy)) {
	blockoff(guyx+dx,guyy+dy);
	blockon(x,y,T_YELLOW);
      }
    }

    break;
  case T_ON:
   
    become(T_ELECTRIC,T_FLOOR);
    become(T_ON, T_OFF);

    break;
  case T_TRANSPORTER:
    /* warp to destination. */

    if (brd(guyx,guyy).t == T_PANEL)
      releasepanel(guyx,guyy);
    {
      int newx = brd(guyx+dx,guyy+dy).x,
        newy = brd(guyx+dx,guyy+dy).y;
    
      guyx = newx;
      guyy = newy;
    }

    break;
  case T_PANEL:

    paneldown(guyx+dx,guyy+dy);
    
    goto defaultmove;
    break;
  case T_HOLE:
  case T_OFF:
  case T_ELECTRIC:
  case T_LASER:
  case T_GBUT:
  case T_RBUT:
  case T_BBUT:
  case T_WALL:
  case T_BUP:
  case T_GUP:
  case T_RUP:
    /* block the player */
    return;

    break;
  case T_BREAKABLE:
    brd(guyx+dx,guyy+dy).t = T_FLOOR;
    break;
  case T_ONE:
  case T_ZERO:
    /* switch all HORIZ/VERTs */
    swapall(T_VERT,T_HORIZ);
    brd(guyx+dx,guyy+dy).t = (tile==T_ONE)?T_ZERO:T_ONE;
    break;
  case T_GREY:
  case T_RED:
  case T_UPDOWN:
  case T_LEFTRIGHT:
  case T_TOPLEFT:
  case T_TOPRIGHT:
  case T_BOTLEFT:
  case T_BOTRIGHT:
  case T_VERT:
  case T_HORIZ:
    /* Pushable Blocks */
    /* check for push off screen: */
    if (!twosafe) return;

    switch(brd(guyx+dx+dx,guyy+dy+dy).t) {
    case T_PANEL:
      if (tile != T_GREY && tile != T_RED) return;
    case T_FLOOR:

      if (tile == T_VERT && dx) return;
      if (tile == T_HORIZ && dy) return;
      
      /* ok, move it. */

      blockoff(guyx+dx,guyy+dy);

      blockon(guyx+dx+dx,guyy+dy+dy,tile);

      break;
    case T_HOLE:
      if (tile != T_GREY) return;
      
      brd(guyx+dx+dx, guyy+dy+dy).t = T_FLOOR;
      blockoff(guyx+dx,guyy+dy);
      
      break;
    default:
      /* no five. */
      return;
    }

    /* FALLTHRU to move the dude */
  default:
  defaultmove:
    /* move the player FIXME */

    guyx += dx;
    guyy += dy;
    
    if (brd(guyx-dx,guyy-dy).t == T_PANEL)
      releasepanel(guyx-dx,guyy-dy);

  }


}

void blockon(char x, char y, char t) {

  switch (brd(x,y).t) {
  case T_PANEL:
    /* zap the dest: */
    paneldown(x,y);
  case T_FLOOR:
  default:
    brd(x,y).z = brd(x,y).t;
    brd(x,y).t = t;
  }

}

void blockoff(char x, char y) {
  switch ((brd(x,y).t = brd(x,y).z)) {
  case T_NULL:
    brd(x,y).t = T_FLOOR;
  case T_PANEL:
    releasepanel(x,y);
  }
}

void paneldown( char x, char y) {

  char xx = brd(x,y).x,
       yy = brd(x,y).y;

    brd(xx,yy).o = brd(xx,yy).t;
    brd(xx,yy).t = T_FLOOR;

}

void releasepanel(char x, char y) {
  /* panel was pressed, release it: */
  
  char xx = brd(x,y).x,
       yy = brd(x,y).y;

  if (xx == -1 || yy == -1) return; /* error! */

  brd(xx,yy).t = brd(xx,yy).o;

}

void swapall(char a , char b ) {
  char x,y;
  for (y=0;y<MAXY;y++)
    for (x=0;x<MAXX;x++)
      if (brd(x,y).t == a) brd(x,y).t = b;
      else if (brd(x,y).t == b) brd(x,y).t = a;
}

void become(char a, char b) {
  char x,y;
  for (y=0;y<MAXY;y++)
    for (x=0;x<MAXX;x++)
      if (brd(x,y).t == a) brd(x,y).t = b;
}

void printpair(int t, int q) { 
  int z;

  for (z=0;z<8;z++) {
    brd(z,0).t = ((t>>z)&1) ? T_WALL : T_FLOOR;
    brd(z,1).t = ((q>>z)&1) ? T_WALL : T_FLOOR;
  }

}


