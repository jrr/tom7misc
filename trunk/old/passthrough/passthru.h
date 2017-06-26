// Copyright (c) 1999 Tom 7 - No warranty. Distributed under the GNU
// Public License: http://www.gnu.org/copyleft/gpl.html

// passthru.h : main header file for the PASSTHRU application
//

#if !defined(AFX_PASSTHRU_H__CBEAF386_F4BB_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_PASSTHRU_H__CBEAF386_F4BB_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// Passthrough:
// See passthru.cpp for the implementation of this class
//

class Passthrough : public CWinApp
{
public:
	Passthrough();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(Passthrough)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(Passthrough)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PASSTHRU_H__CBEAF386_F4BB_11D2_A014_0080C8443AA1__INCLUDED_)
