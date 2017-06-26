#if !defined(AFX_MAPDLG_H__4BDAADA3_C1F0_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_MAPDLG_H__4BDAADA3_C1F0_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// mapdlg.h : header file
//
#include "mapper.h"
/////////////////////////////////////////////////////////////////////////////
// mapdlg dialog

void drawmaps(CDC & memdc, mapnode * h);
void drawtree(CDC & memdc, mapnode *h);

struct blah {
	int x,y;
	blah * next;
	blah(int a, int b, blah * n) : x(a), y(b), next(n) {}
};

class mapdlg : public CDialog {
// Construction
public:
  CBitmap * picture;
  void init(CClientDC &, mapper *);
  mapdlg(CWnd* pParent = NULL);   // standard constructor
  ~mapdlg() { 
    delete picture; 
    for (blah * s = blahhead; s ; ) {
      blah * t = s;
      s = s->next;
      delete t;
    }
  }
// Dialog Data
	//{{AFX_DATA(mapdlg)
	enum { IDD = TOM7_MAPDLG };
	CStatic	display;
	//}}AFX_DATA

private:
	mapnode * nodeheld;

	blah * blahhead;

	void drawdots(CDC & memdc, mapnode * h);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(mapdlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	mapper * dmap;
	// Generated message map functions
	//{{AFX_MSG(mapdlg)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnPaint();
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void mapdlg_ok();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MAPDLG_H__4BDAADA3_C1F0_11D2_A014_0080C8443AA1__INCLUDED_)
