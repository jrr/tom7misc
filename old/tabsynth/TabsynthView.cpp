// TabsynthView.cpp : implementation of the CTabsynthView class
//

#include "stdafx.h"
#include "Tabsynth.h"

#include "TabsynthDoc.h"
#include "TabsynthView.h"

#include "util.h"

#include "mapdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTabsynthView

IMPLEMENT_DYNCREATE(CTabsynthView, CView)

BEGIN_MESSAGE_MAP(CTabsynthView, CView)
	//{{AFX_MSG_MAP(CTabsynthView)
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONUP()
	ON_COMMAND(TEST_MAPDLG, DebugOnMapdlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTabsynthView construction/destruction

CTabsynthView::CTabsynthView()
{
	running=0;

	// TODO: add construction code here

}

CTabsynthView::~CTabsynthView()
{
}

BOOL CTabsynthView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CTabsynthView drawing

void CTabsynthView::OnDraw(CDC* pDC)
{
	CTabsynthDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	// TODO: add draw code for native data here
}

/////////////////////////////////////////////////////////////////////////////
// CTabsynthView diagnostics

#ifdef _DEBUG
void CTabsynthView::AssertValid() const
{
	CView::AssertValid();
}

void CTabsynthView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CTabsynthDoc* CTabsynthView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CTabsynthDoc)));
	return (CTabsynthDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CTabsynthView message handlers

void CTabsynthView::OnLButtonDown(UINT nFlags, CPoint point) {
	
//	if (!sizer) MsgBox ("Sizer is null");

//	char a[512];
//	sprintf(a,"0x%p, %d", sizer,sizer);
//	MsgBox(a);

	int x = delme_map.map(point.x+2 << 7);
	int y = point.y;
	
	sizer -> setfreq (x);
	sizer -> setshape(y);
	sizer -> start ();
	running = 1;
	CView::OnLButtonDown(nFlags, point);
}

void CTabsynthView::OnMouseMove(UINT nFlags, CPoint point) {
	
//if (!sizer) MsgBox ("Sizer is null");
	if (running) {

	int x = delme_map.map(point.x+2 << 7);
	int y = point.y;
	
	sizer -> setfreq (x);
	sizer -> setshape(y);


	}
	CView::OnMouseMove(nFlags, point);
}

void CTabsynthView::OnLButtonUp(UINT nFlags, CPoint point) {
	// TODO: Add your message handler code here and/or call default

//	if (!sizer) MsgBox ("Sizer is null");

	sizer -> stop ();
	running = 0;
	CView::OnLButtonUp(nFlags, point);
}

void CTabsynthView::DebugOnMapdlg() {
	mapdlg xxx;	
	CClientDC ccc (this);

	xxx.init(ccc,&delme_map);
	xxx.DoModal();
}
