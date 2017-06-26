// Copyright (c) 1999 Tom 7 - No warranty. Distributed under the GNU
// Public License: http://www.gnu.org/copyleft/gpl.html

// link winmm.lib to your project or it won't work!

// passthru.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "passthru.h"
#include "passdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Passthrough

BEGIN_MESSAGE_MAP(Passthrough, CWinApp)
	//{{AFX_MSG_MAP(Passthrough)
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Passthrough construction

Passthrough::Passthrough()
{
}

/////////////////////////////////////////////////////////////////////////////
// The one and only Passthrough object

Passthrough theApp;

/////////////////////////////////////////////////////////////////////////////
// Passthrough initialization

BOOL Passthrough::InitInstance()
{
	// Standard initialization

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	int a=-1, b=-1;

    if (m_lpCmdLine[0] != '\0') {
		for (unsigned int ss=0;ss<strlen(m_lpCmdLine);ss++) {
			if (m_lpCmdLine[ss] == '=') {
				m_lpCmdLine[ss] = 0;
				a = atoi(m_lpCmdLine);
				b = atoi(m_lpCmdLine+ss+1);
				break;
			} 
		}
    }

	Passdlg dlg;
	m_pMainWnd = &dlg;

	/* get midi info */

	dlg.setdefs(a,b);

//	dlg.initdevices();

	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
	}
	else if (nResponse == IDCANCEL)
	{
	}

	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}
