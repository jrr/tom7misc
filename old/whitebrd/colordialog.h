#if !defined(AFX_COLORDIALOG_H__A8D260E4_8477_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_COLORDIALOG_H__A8D260E4_8477_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// colordialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// colordialog dialog

class colordialog : public CDialog
{
// Construction
public:
	void changebitcolor();
	CBitmap * cdisplay;
	void generatebitmap(CBitmap*c);
	colordialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(colordialog)
	enum { IDD = IDD_COLORS };
	CStatic	staticbox;
	CSliderCtrl	redslide;
	CSliderCtrl	greenslide;
	CSliderCtrl	blueslide;
	//}}AFX_DATA

	DLGTEMPLATE shu;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(colordialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(colordialog)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	virtual BOOL OnInitDialog();
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_COLORDIALOG_H__A8D260E4_8477_11D2_A014_0080C8443AA1__INCLUDED_)
