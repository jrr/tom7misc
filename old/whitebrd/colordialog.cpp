// colordialog.cpp : implementation file
//

#include "stdafx.h"
#include "whitebrd.h"
#include "colordialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// colordialog dialog


colordialog::colordialog(CWnd* pParent /*=NULL*/)
	: CDialog(colordialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(colordialog)
	//}}AFX_DATA_INIT
	/*
	redslide.SetRange(2,99);
	blueslide.SetRange(2,99);
	greenslide.SetRange(2,99);
	CStatic
  */
	cdisplay =0;	

}


void colordialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(colordialog)
	DDX_Control(pDX, IDC_COLORBOX, staticbox);
	DDX_Control(pDX, IDC_RED, redslide);
	DDX_Control(pDX, IDC_GREEN, greenslide);
	DDX_Control(pDX, IDC_BLUE, blueslide);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(colordialog, CDialog)
	//{{AFX_MSG_MAP(colordialog)
	ON_WM_CREATE()
	ON_WM_MOUSEMOVE()
	ON_WM_HSCROLL()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// colordialog message handlers

int colordialog::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	return 0;
}

BOOL colordialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// TODO: Add extra initialization here
/*
	redbar.SetScrollRange(0,255);
*/
	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void colordialog::generatebitmap(CBitmap * c) {
	//Thanks!
	cdisplay = c;
}

void colordialog::changebitcolor() {
    CClientDC clientDC(this);
	
	CDC memDC;
    memDC.CreateCompatibleDC(&clientDC);
	memDC.SelectObject(cdisplay);
	
	// Fill the bitmap with white to begin:
	CBrush brush(RGB(redslide.GetPos(),greenslide.GetPos(),blueslide.GetPos()));
	memDC.FillRect(CRect(0,0,60,20), &brush);
	staticbox.SetBitmap(HBITMAP(*cdisplay));
}

void colordialog::OnMouseMove(UINT nFlags, CPoint point) {
	CDialog::OnMouseMove(nFlags, point);
}

void colordialog::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) {
	// something has scrolled:
	changebitcolor();
	CDialog::OnHScroll(nSBCode, nPos, pScrollBar);
}
