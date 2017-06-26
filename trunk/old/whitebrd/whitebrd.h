// whitebrd.h : main header file for the WHITEBRD application
//

#if !defined(AFX_WHITEBRD_H__A878E230_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_WHITEBRD_H__A878E230_81F9_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdApp:
// See whitebrd.cpp for the implementation of this class
//

class CWhitebrdApp : public CWinApp
{
public:
	CWhitebrdApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CWhitebrdApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CWhitebrdApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_WHITEBRD_H__A878E230_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
