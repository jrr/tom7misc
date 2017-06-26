// combinedlg.cpp : implementation file
//

#include "stdafx.h"
#include "Tabsynth.h"
#include "combinedlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// combinedlg dialog


combinedlg::combinedlg(CWnd* pParent /*=NULL*/)
	: CDialog(combinedlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(combinedlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void combinedlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(combinedlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(combinedlg, CDialog)
	//{{AFX_MSG_MAP(combinedlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// combinedlg message handlers
