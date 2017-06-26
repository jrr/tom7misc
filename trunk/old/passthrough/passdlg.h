// Copyright (c) 1999 Tom 7 - No warranty. Distributed under the GNU
// Public License: http://www.gnu.org/copyleft/gpl.html


// passdlg.h : header file
//

#if !defined(AFX_PASSDLG_H__CBEAF388_F4BB_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_PASSDLG_H__CBEAF388_F4BB_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <mmsystem.h>

/////////////////////////////////////////////////////////////////////////////
// Passdlg dialog

class Passdlg : public CDialog
{
// Construction
public:
	Passdlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(Passdlg)
	enum { IDD = IDD_PASSTHRU_DIALOG };
	CListBox	deviceout;
	CListBox	devicein;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(Passdlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation

public:
	void initdevices();
	void setdefs(int a, int b) {
		defaultin = a;
		defaultout = b;
	}

protected:

	HMIDIIN			handle;
	MIDIOUTCAPS     moc;
	MIDIINCAPS      mic;

	int defaultin, defaultout;


	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(Passdlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void onchangefrom();
	afx_msg void onchangeto();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PASSDLG_H__CBEAF388_F4BB_11D2_A014_0080C8443AA1__INCLUDED_)
