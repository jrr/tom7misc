// whitebrdView.h : interface of the CWhitebrdView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_WHITEBRDVIEW_H__A878E238_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_WHITEBRDVIEW_H__A878E238_81F9_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "colordialog.h"
#include "HelpBox.h"	// Added by ClassView
#include "tablet/tomtablet.h"

class CWhitebrdView : public CView
{
protected: // create from serialization only
	CWhitebrdView();
	DECLARE_DYNCREATE(CWhitebrdView)

	CStroke * strokecur;
	CPoint ptprev;

	CBitmap * bootmap;

// Attributes
public:
	CWhitebrdDoc* GetDocument();
	colordialog colours;
	void OnTabletPacket(WPARAM wParam, LPARAM lParam);
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CWhitebrdView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL

// Implementation
public:
	CHelpBox hellobox;
	void updatescrollies();
	void cls();
	void whiteparse(string s);
	int receiving;
	void stopreceive();
	void startreceive();
	void drawline(CPoint a, CPoint b, unsigned char cr, unsigned char cg, unsigned char cb, UINT pwidth, CBitmap* boo);
	void sendline(CPoint a, CPoint b,int);
	virtual ~CWhitebrdView();

	int scale(int);
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	tomtablet * tablet;

protected:
	int ALREADYOPEN;
// Generated message map functions
protected:
	//{{AFX_MSG(CWhitebrdView)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnTimer(UINT nIDEvent);
	afx_msg void OnClsToolbar();
	afx_msg void OnColorbutt();
	afx_msg void BrightnessUp();
	afx_msg void BrightnessDOWN();
	afx_msg void OnBrightdown();
	afx_msg void OnBrightup();
	afx_msg void OnSendhelpbox();
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in whitebrdView.cpp
inline CWhitebrdDoc* CWhitebrdView::GetDocument()
   { return (CWhitebrdDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_WHITEBRDVIEW_H__A878E238_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
