// TabsynthDoc.h : interface of the CTabsynthDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_TABSYNTHDOC_H__50DF710D_B480_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_TABSYNTHDOC_H__50DF710D_B480_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "synth.h"


class CTabsynthDoc : public CDocument
{
protected: // create from serialization only
	CTabsynthDoc();
	DECLARE_DYNCREATE(CTabsynthDoc)

// Attributes
public:
//	void installhwnd(HWND s) : myhwnd(s) {}
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTabsynthDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CTabsynthDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	HWND    myhwnd;
	synth * delmetoo;

protected:

// Generated message map functions
protected:
	//((AFX_MSG(CTabsynthDoc)
	// afx_msg void test_tone();
	//))AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TABSYNTHDOC_H__50DF710D_B480_11D2_A014_0080C8443AA1__INCLUDED_)
