#if !defined(AFX_HELPBOX_H__C91254C5_85F1_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_HELPBOX_H__C91254C5_85F1_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// HelpBox.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CHelpBox dialog

class CHelpBox : public CDialog
{
// Construction
public:
	CHelpBox(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CHelpBox)
	enum { IDD = IDD_HELPDIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHelpBox)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CHelpBox)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_HELPBOX_H__C91254C5_85F1_11D2_A014_0080C8443AA1__INCLUDED_)
