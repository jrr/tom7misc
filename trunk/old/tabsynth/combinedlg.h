#if !defined(AFX_COMBINEDLG_H__71AE4727_BD3C_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_COMBINEDLG_H__71AE4727_BD3C_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// combinedlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// combinedlg dialog

class combinedlg : public CDialog
{
// Construction
public:
	combinedlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(combinedlg)
	enum { IDD = IDD_COMBINEDLG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(combinedlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(combinedlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_COMBINEDLG_H__71AE4727_BD3C_11D2_A014_0080C8443AA1__INCLUDED_)
