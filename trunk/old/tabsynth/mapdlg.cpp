// mapdlg.cpp : implementation file
//

#include "stdafx.h"
#include "Tabsynth.h"
#include "mapdlg.h"
#include "mapper.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// mapdlg dialog


mapdlg::mapdlg(CWnd* pParent /*=NULL*/)
  : CDialog(mapdlg::IDD, pParent) {
  //{{AFX_DATA_INIT(mapdlg)
  // NOTE: the ClassWizard will add member initialization here
  //}}AFX_DATA_INIT
  dmap = 0;
  picture = 0;
  nodeheld = 0;
}



void mapdlg::DoDataExchange(CDataExchange* pDX) {
  CDialog::DoDataExchange(pDX);
  //{{AFX_DATA_MAP(mapdlg)
  DDX_Control(pDX, TOM7_MAPDISPLAY, display);
  //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(mapdlg, CDialog)
	//{{AFX_MSG_MAP(mapdlg)
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONUP()
	ON_WM_PAINT()
	ON_WM_RBUTTONDOWN()
	ON_BN_CLICKED(IDOK, mapdlg_ok)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// mapdlg message handlers

void mapdlg::OnLButtonDown(UINT nFlags, CPoint point) {
  // TODO: Add your message handler code here and/or call default
	
  //MsgBox(itos(point.x) + (string)" - " + itos(point.y), "OOH");
#if 1
  int xx = (point.x-9)<<8,
    yy = (255-(point.y-9))<<8;


  if (xx < 0) xx = 0;
  if (yy < 0) yy = 0;
  if (xx > 65535) xx = 65535;
  if (yy > 65535) yy = 65535;


  mapnode * usa;
  if ( (usa = dmap -> getnode(xx,5)) ) {
    nodeheld = usa;
//    dmap -> remove (nodeheld);
//    nodeheld = 0;
  } else { // make new node: 
    nodeheld = dmap -> insert(xx,yy);
  }
#endif	
  OnPaint();

  CDialog::OnLButtonDown(nFlags, point);
}

void mapdlg::OnMouseMove(UINT nFlags, CPoint point) {
  // TODO: Add your message handler code here and/or call default
#if 1	
  if (nodeheld) {
    /* dragging a node */

    int xx = (point.x-9)<<8,
      yy = (255-(point.y-9))<<8;

    if (xx < 0) xx = 0;
    if (yy < 0) yy = 0;
    if (xx > 65535) xx = 65535;
    if (yy > 65535) yy = 65535;

    /* FIXME */
    if (xx != nodeheld->x || yy != nodeheld->height) {
      nodeheld = dmap -> move (nodeheld, xx, yy);
      OnPaint() ;
    }
  }
#endif
  OnPaint();
  CDialog::OnMouseMove(nFlags, point);
}

void mapdlg::OnLButtonUp(UINT nFlags, CPoint point) {
  // TODO: Add your message handler code here and/or call default
	#if 1
  nodeheld = 0;
#endif
  OnPaint();

  CDialog::OnLButtonUp(nFlags, point);
}

void mapdlg::init(CClientDC & dude, mapper * m) {
  /* make a bitmap with this device context */
  dmap = m;
  ((CDialog*)0)->MessageBox("init","init",MB_OK);
  picture = new CBitmap;
  picture -> CreateCompatibleBitmap(&dude,256,256);
  blahhead = 0;
}

#define DLGBGCOLOR (RGB(0,64,0))
#define DLGGRIDCOLOR (RGB(0,100,0))
#define DLGCROSSCOLOR (RGB(0,150,0))
#define DLGLINECOLOR (RGB(200,200,200))
#define DLGDRAGDOTCOLOR (RGB(255,255,255))

#define DLGTREECOLORL (RGB(128,128,255))
#define DLGTREECOLORR (RGB(255,128,128))

#define DLGTESTCOLOR   (RGB(255,0,0))

/* how about arrows diagramming the tree? */

  CPen treepenl(PS_SOLID,1,DLGTREECOLORL);
  CPen treepenr(PS_SOLID,1,DLGTREECOLORR);

  CBrush bgbrush(DLGBGCOLOR);
  CBrush dragdotbrush(DLGDRAGDOTCOLOR);
  CPen gridpen(PS_SOLID,1,DLGGRIDCOLOR);
  CPen crosspen(PS_SOLID,1,DLGCROSSCOLOR);


void mapdlg::OnPaint() {

  if (!picture) return;

  CPaintDC dc(this); // device context for painting
	
  CClientDC clientDC(this);
	
  CDC memdc;
  memdc.CreateCompatibleDC(&clientDC);
  memdc.SelectObject(picture);


  memdc.FillRect(CRect(0,0,256,256), &bgbrush);

  memdc.SelectObject(&gridpen);
  for (int x=0;x<16;x++) {
    memdc.MoveTo(0,x<<4);
    memdc.LineTo(256,x<<4);
    memdc.MoveTo(x<<4,0);
    memdc.LineTo(x<<4,256);
  }

  /* cross */
  memdc.SelectObject(&crosspen);

  memdc.MoveTo(128,0);
  memdc.LineTo(128,256);
  memdc.MoveTo(0,128);
  memdc.LineTo(256,128);


  CPen linepen(PS_SOLID,1,DLGLINECOLOR);
  memdc.SelectObject(&linepen);

#if 1
  drawmaps(memdc, dmap->maphead);
#endif
  //  drawtree(memdc, dmap->maphead);

#if 1
  memdc.SelectObject(&dragdotbrush);
  drawdots(memdc, dmap->maphead);
#endif

#if 1
  CBrush tempbrush(DLGTESTCOLOR);
  memdc.SelectObject(&tempbrush);
	
  for (blah * s = blahhead; s ; s = s -> next) {
    RECT dott;
	
    dott.left   = (s->x>>8) - 3;
    dott.right  = (s->x>>8) + 3;
    dott.top    = 255-(dmap->map(s->x)>>8) - 3;
    dott.bottom = 255-(dmap->map(s->x)>>8) + 3;

    memdc.Rectangle(&dott);
  }
#endif
  display.SetBitmap(HBITMAP(*picture));
}

void drawtree(CDC & memdc, mapnode *h) {
  if (!h) return;
  if (h->l) {
    drawtree(memdc,h->l);
    memdc.SelectObject(&treepenl);
    memdc.MoveTo(h->l->x>>8, 255-(h->l->height>>8));
    memdc.LineTo(h->x>>8,255-(h->height>>8));
    memdc.MoveTo((h->l->x>>8), (255-(h->l->height>>8)-4));
    memdc.LineTo(h->x>>8,255-(h->height>>8));
  }
  if (h->r) {
    drawtree(memdc,h->r);
    memdc.SelectObject(&treepenr);

    memdc.MoveTo(h->r->x>>8, 255-(h->r->height>>8));
    memdc.LineTo(h->x>>8,255-(h->height>>8));
    memdc.MoveTo((h->r->x>>8), (255-(h->r->height>>8)-4));
    memdc.LineTo(h->x>>8,255-(h->height>>8));
  }

}

void drawmaps(CDC & memdc, mapnode * h) {
  if (!h) return;
  if (h->l) drawmaps(memdc,h->l);

  /* draw me: */
  if (!h->x) {
    /* rightmost node. Do MoveTo, not LineTo: */
    memdc.MoveTo(h->x>>8,255-(h->height>>8));

  } else memdc.LineTo(h->x>>8,255-(h->height>>8));

  if (h->r) drawmaps(memdc,h->r);
}

CBrush dotbrush(RGB(255,255,255));

void mapdlg::drawdots(CDC & memdc, mapnode * h) {
  if (!h) return;
  if (h->l) drawdots(memdc,h->l);

  RECT dott;
	
  dott.left   = (h->x>>8)-2;
  dott.right  = (h->x>>8)+3;
  dott.top    = (255-(h->height>>8))-2;
  dott.bottom = (255-(h->height>>8))+3;
  if (h != nodeheld) {
    memdc.FrameRect(&dott,&dotbrush);
  } else {
    memdc.Rectangle(&dott);
  }

  if (h->r) drawdots(memdc,h->r);
}

void mapdlg::OnRButtonDown(UINT nFlags, CPoint point)  {
  // TODO: Add your message handler code here and/or call default
  // tmp //
#if 1
  int xx = (point.x-9)<<8;
  //yy = (255-(point.y-9))<<8;

  int yy = (dmap->map(xx)>>8) + 9;

  /*
    char msg[128];
    sprintf(msg, "%d -> %d", xx, yy);

    MessageBox(msg,"FUCK",MB_OK);
  */
  
  ((CDialog*)0)->MessageBox("blah","blah",MB_OK);
  blahhead = new blah(xx,yy,blahhead);
#endif
  CDialog::OnRButtonDown(nFlags, point);
}

void mapdlg::mapdlg_ok() {
	delete picture; // clean up
	picture = 0;
	EndDialog(0);
}
