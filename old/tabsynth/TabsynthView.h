// TabsynthView.h : interface of the CTabsynthView class
//
/////////////////////////////////////////////////////////////////////////////

#include "synth.h"

#if !defined(AFX_TABSYNTHVIEW_H__50DF710F_B480_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_TABSYNTHVIEW_H__50DF710F_B480_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class CTabsynthView : public CView {
protected: // create from serialization only
	CTabsynthView();
	DECLARE_DYNCREATE(CTabsynthView)

// Attributes
public:
	CTabsynthDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTabsynthView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CTabsynthView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// temp
	int running;


protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CTabsynthView)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void DebugOnMapdlg();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in TabsynthView.cpp
inline CTabsynthDoc* CTabsynthView::GetDocument()
   { return (CTabsynthDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TABSYNTHVIEW_H__50DF710F_B480_11D2_A014_0080C8443AA1__INCLUDED_)
