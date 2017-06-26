// whitebrdView.cpp : implementation of the CWhitebrdView class
//

#include "stdafx.h"
#include "whitebrd.h"

#include "whitebrdDoc.h"
#include "whitebrdView.h"

#include "tablet/tomtablet.h"

#include <wintab.h>
//#define PACKETDATA	(PK_NORMAL_PRESSURE | PK_ORIENTATION | PK_CURSOR)
//#define PACKETMODE      0

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define TIMER_INTERVAL 10

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdView

IMPLEMENT_DYNCREATE(CWhitebrdView, CView)

BEGIN_MESSAGE_MAP(CWhitebrdView, CView)
	//{{AFX_MSG_MAP(CWhitebrdView)
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_CREATE()
	ON_WM_TIMER()
	ON_COMMAND(TOM7_CLS, OnClsToolbar)
	ON_COMMAND(ID_COLORBUTT, OnColorbutt)
	ON_COMMAND(TOM7_BRUP, BrightnessUp)
	ON_COMMAND(TOM7_BRDOWN, BrightnessDOWN)
	ON_COMMAND(TOM7_BRIGHTDOWN, OnBrightdown)
	ON_COMMAND(TOM7_BRIGHTUP, OnBrightup)
	ON_COMMAND(TOM7_SENDHELPBOX, OnSendhelpbox)
	ON_WM_SETFOCUS()
	ON_WM_KILLFOCUS()
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CView::OnFilePrintPreview)
	ON_MESSAGE( WT_PACKET, OnTabletPacket )

END_MESSAGE_MAP()

void CWhitebrdView :: OnTabletPacket(WPARAM wParam, LPARAM lParam) {
	if (tablet) tablet -> dopacket(wParam, lParam);
}

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdView construction/destruction

/*

To change The Status Bar:

  CString s = "Text";
    CStatusBar* p =
     (CStatusBar*)AfxGetApp()->m_pMainWnd->GetDescendantWindow(AFX_IDW_STATUS_BAR);
    p->SetPaneText(1, s);

  */

CWhitebrdView::CWhitebrdView(){
	// TODO: add construction code here
	receiving = 0;
	tablet = 0;
}

CWhitebrdView::~CWhitebrdView() {
	delete tablet;
	delete bootmap;
}

BOOL CWhitebrdView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdView drawing
/* CView */
void CWhitebrdView::OnDraw(CDC* pDC) {
	CWhitebrdDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	CDC poop;
	poop.CreateCompatibleDC(pDC);
	/*CPen * old = */poop.SelectObject( bootmap );
	pDC->BitBlt(0,0,526,389,&poop,0,0,SRCCOPY);
	//poop.SelectObject ( old );
	SetWindowPos(&wndTopMost,0,0,532,394,0 | SWP_NOMOVE | SWP_NOZORDER);
}

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdView printing

BOOL CWhitebrdView::OnPreparePrinting(CPrintInfo* pInfo) {
	// default preparation
	return DoPreparePrinting(pInfo);
}

void CWhitebrdView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/) {
	// TODO: add extra initialization before printing
}

void CWhitebrdView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/) {
	// TODO: add cleanup after printing
}

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdView diagnostics

#ifdef _DEBUG
void CWhitebrdView::AssertValid() const
{
	CView::AssertValid();
}

void CWhitebrdView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CWhitebrdDoc* CWhitebrdView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CWhitebrdDoc)));
	return (CWhitebrdDoc*)m_pDocument;
}
#endif //_DEBUG


/////////////////////////////////////////////////////////////////////////////
// CWhitebrdView message handlers

void CWhitebrdView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	//strokecur = GetDocument ()-> newstroke();

	//strokecur -> pointarray.Add(point);

	CWhitebrdDoc * d = GetDocument();

	d -> CR = colours.redslide.GetPos()/* * (255./100)*/;
	d -> CG = colours.greenslide.GetPos()/* *(255./100)*/;
	d -> CB = colours.blueslide.GetPos()/* *(255/100.)*/;

	SetCapture();

	ptprev = point;	
}

void CWhitebrdView::OnLButtonUp(UINT nFlags, CPoint point) {
	// TODO: Add your message handler code here and/or call default
	
	if (GetCapture() != this) return;
	
	CWhitebrdDoc * doc = GetDocument();

	int pwid=0;

	if (tablet) pwid = scale(tablet->t_prsNew);
	if (!pwid) pwid = doc->penwidth;

	drawline(ptprev,point,doc->CR,doc->CG,doc->CB,pwid,bootmap);
	sendline(ptprev,point,pwid);

	ReleaseCapture();
}

void CWhitebrdView::OnMouseMove(UINT nFlags, CPoint point) {

	if (GetCapture() != this) return;

	CWhitebrdDoc * doc = GetDocument();

	int pwid=0;

	if (tablet) pwid = scale(tablet->t_prsNew);
	if (!pwid) pwid = doc->penwidth;

	drawline(ptprev,point,doc->CR,doc->CG,doc->CB,pwid,bootmap);
	sendline(ptprev,point,pwid);

	ptprev=point;
}


int CWhitebrdView::scale(int s) {
	// scale tablet pressure:

	s =(s / (5+(MAXPENW-	GetDocument()->penwidth)))  + 1;
	if (s <1 ) return 1; else return s;
}

int CWhitebrdView::OnCreate(LPCREATESTRUCT lpCreateStruct) {
	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;

	GetDocument () -> InstallWV(this); /* hack ! */

    CClientDC clientDC(this);
	bootmap = new CBitmap;
	
	bootmap->CreateCompatibleBitmap(&clientDC, 532,394);

	colours.Create( IDD_COLORS, 0);

	colours.redslide.SetRange(0,255);
	colours.greenslide.SetRange(0,255);
	colours.blueslide.SetRange(0,255);

	CBitmap * obomap = new CBitmap;
	obomap->CreateCompatibleBitmap(&clientDC,60,20);
	colours.generatebitmap(obomap);

	/*
	CDC memDC;
    memDC.CreateCompatibleDC(&clientDC);
	memDC.SelectObject(bootmap);
	*/
	// Fill the bitmap with white to begin:
	cls();

	/* initialise tablet if it exists: */

	tablet = new tomtablet(GetSafeHwnd());	

	return 0;
}

void CWhitebrdView::sendline(CPoint a, CPoint b, int pwid) {
	
	/*  @DR 0,1,0,16777215,39,63,41,65@ */
	/*      mode,brushsize,color,?,		x,y,x,y	*/
	CWhitebrdDoc * d = GetDocument();
	char buf[256];
	int z=
	sprintf(buf,"%cDR 0,%d,%d,%s,%d,%d,%d,%d%c\n",1,
		pwid,
		TRGB(d->CR,d->CG,d->CB),
		"16777215",
		a.x,a.y,
		b.x,b.y,
		1);
	d->suck.sendmsg(buf);
}

void CWhitebrdView::drawline(CPoint a, CPoint b, unsigned char cr, unsigned char cg, unsigned char cb, UINT pwidth, CBitmap * boo) {

	CClientDC clientDC(this);
	CDC memDC;
    memDC.CreateCompatibleDC(&clientDC);
    memDC.SelectObject(boo);

	CPen spent(0,pwidth,RGB(cr,cg,cb));

	CPen* oldBrush1 = memDC.SelectObject(&spent);
    CPen* oldBrush2 = clientDC.SelectObject(&spent);

	memDC.MoveTo(b);
	memDC.LineTo(a);
	clientDC.MoveTo(b);
	clientDC.LineTo(a);

    memDC.SelectObject(oldBrush1);
    clientDC.SelectObject(oldBrush2);
}

void CWhitebrdView::OnTimer(UINT nIDEvent) {
	if (receiving) {
		// are there any messages in the queue for me?

		CWhitebrdDoc * d = GetDocument ();
		while (d->suck.iqueue) {
			string s = d->suck.getmsg();
			whiteparse(s);
		}
	SetTimer(42,TIMER_INTERVAL,0);
	}
	CView::OnTimer(nIDEvent);
}
	
void CWhitebrdView::startreceive() {
	SetTimer(42,TIMER_INTERVAL,0);
	receiving = 1;
}

void CWhitebrdView::stopreceive() {
	receiving = 0;
}

void CWhitebrdView::whiteparse(string s) {
	// ooh baby.
	unsigned int numbers[15];
	if (s[0] != '\x01') return;
	
	switch (s[1]) {
	case 'C':
		// cls command -> clear screen.
		cls();
	break;
	case 'T':
		// txt command -> unsupported text drawering.
	break;
	case 'D':
	{
		// DR command -> stuff:
		int nidx=0;
		numbers[0] =0;
		for (int i=4;i<(s.length()-1);i++) {
			if (s[i] == ',') {
				numbers[++nidx] = 0;
				if (nidx > 12) break;
			} else numbers[nidx] = (numbers[nidx]*10) + (s[i]-'0');
		}
		// ok!
	drawline(CPoint(numbers[4],numbers[5]),
		     CPoint(numbers[6],numbers[7]),
			 numbers[2]&255,
			 (numbers[2]>>8)&255,
			 (numbers[2]>>16)&255,
			 numbers[1], bootmap);
	}
	default:
	break;
	}
}

void CWhitebrdView::cls() {
    CClientDC clientDC(this);
	
	CDC memDC;
    memDC.CreateCompatibleDC(&clientDC);
	memDC.SelectObject(bootmap);
	
	// Fill the bitmap with white to begin:
	CBrush brush(RGB(255,255,255));
	memDC.FillRect(CRect(0,0,532,394), &brush);
	clientDC.FillRect(CRect(0,0,532,394), &brush);
}

void CWhitebrdView::OnClsToolbar() {
	GetDocument () -> suck . sendmsg("\001CLS\001\n");
	cls();
}

void CWhitebrdView::OnColorbutt() {
	// TODO: Add your command handler code here
//	colours.ShowWindow(SW_SHOW);
/*
static SCROLLINFO jack;
jack.cbSize = sizeof (SCROLLINFO);
jack.nMin=0;
jack.nMax=255;
jack.nPage=10;
jack.nPos=255;
//jack.nScrollPos=0;
jack.fMask = SIF_POS | SIF_RANGE;
*/
if (!ALREADYOPEN) {
/*	
	if (!colours.redbar.SetScrollInfo(&jack)) MsgBox("FUCK!!!");
	colours.greenbar.SetScrollInfo(&jack);
	colours.bluebar.SetScrollInfo(&jack);
*/


	ALREADYOPEN = 1;
}
  colours.ShowWindow(SW_SHOW);

	
/*
	int a,b;
	
	colours.redbar.GetScrollRange(&a,&b);
	char stupidbuf[128];
	sprintf(stupidbuf,"%d and %d",a,b);
	MsgBox(stupidbuf);
	

*/	
	CWhitebrdDoc * d = GetDocument ();
/*
	char stupidbuf[128];
	sprintf(stupidbuf,"%d %d %d",d->CR,d->CG,d->CB);
	MsgBox(stupidbuf);
*/

  /*
	colordialog shh;

	shh.DoModal();

  */
}

void CWhitebrdView::BrightnessUp() 
{
	// TODO: Add your command handler code here
	CWhitebrdDoc * d = GetDocument ();
	int a;
	d -> CR = ((a=int(1.1 * d->CR))>255)?255:a;
	d -> CG = ((a=int(1.1 * d->CG))>255)?255:a;
	d -> CB = ((a=int(1.1 * d->CB))>255)?255:a;
	updatescrollies();
}

void CWhitebrdView::BrightnessDOWN() {
	// TODO: Add your command handler code here
	CWhitebrdDoc * d = GetDocument ();

	d -> CR = .9 * d->CR;
	d -> CG = .9 * d->CG;
	d -> CB = .9 * d->CB;
	updatescrollies();
}

void CWhitebrdView::updatescrollies() {
	CWhitebrdDoc * d = GetDocument ();

	colours.redslide.SetPos(d->CR );
	colours.greenslide.SetPos(d->CG );
	colours.blueslide.SetPos(d->CB );
	
	/*
	colours.redslide.SetPos(d->CR * (100./255));
	colours.greenslide.SetPos(d->CG * (100./255));
	colours.blueslide.SetPos(d->CB * (100./255));
	*/
	colours.UpdateData(FALSE);
	colours.changebitcolor();
}

void CWhitebrdView::OnBrightdown() {
	
	CWhitebrdDoc * d = GetDocument ();
	int a;
	d -> CR = ((a=int(-10 + d->CR))<0)?0:a;
	d -> CG = ((a=int(-10 + d->CG))<0)?0:a;
	d -> CB = ((a=int(-10 + d->CB))<0)?0:a;
	updatescrollies();
}

void CWhitebrdView::OnBrightup() {

	CWhitebrdDoc * d = GetDocument ();
	int a;
	d -> CR = ((a=int(10 + d->CR))>255)?255:a;
	d -> CG = ((a=int(10 + d->CG))>255)?255:a;
	d -> CB = ((a=int(10 + d->CB))>255)?255:a;
	updatescrollies();

}

void CWhitebrdView::OnSendhelpbox()  {
	hellobox.DoModal();	
}

void CWhitebrdView::OnSetFocus(CWnd* pOldWnd) {
	CView::OnSetFocus(pOldWnd);
	
	if (tablet)tablet->focuson();
}

void CWhitebrdView::OnKillFocus(CWnd* pNewWnd) {
	CView::OnKillFocus(pNewWnd);

	if (tablet) tablet->focusoff();
}
