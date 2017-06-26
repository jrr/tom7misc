// Tabsynth.h : main header file for the TABSYNTH application
//

#if !defined(AFX_TABSYNTH_H__50DF7107_B480_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_TABSYNTH_H__50DF7107_B480_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include <string>
using namespace std;

#include "util.h"

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CTabsynthApp:
// See Tabsynth.cpp for the implementation of this class
//

#include "synth.h"

extern synth * sizer; // single synth for the app for sound output

class CTabsynthApp : public CWinApp {
public:
	CTabsynthApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTabsynthApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CTabsynthApp)
	afx_msg void OnAppAbout();
	afx_msg void OnOtone();
	afx_msg void edit_mappings();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

#if 0

class tabsynth {
public:
	tabsynth();
	~tabsynth();

};

#endif

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TABSYNTH_H__50DF7107_B480_11D2_A014_0080C8443AA1__INCLUDED_)
