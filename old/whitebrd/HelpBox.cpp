// HelpBox.cpp : implementation file
//

#include "stdafx.h"
#include "whitebrd.h"
#include "HelpBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHelpBox dialog


CHelpBox::CHelpBox(CWnd* pParent /*=NULL*/)
	: CDialog(CHelpBox::IDD, pParent)
{
	//{{AFX_DATA_INIT(CHelpBox)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CHelpBox::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHelpBox)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHelpBox, CDialog)
	//{{AFX_MSG_MAP(CHelpBox)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHelpBox message handlers
