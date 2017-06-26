// connectto.cpp : implementation file
//

#include "stdafx.h"
#include "whitebrd.h"
#include "connectto.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// connectto dialog


connectto::connectto(CWnd* pParent /*=NULL*/)
	: CDialog(connectto::IDD, pParent)
{
	//{{AFX_DATA_INIT(connectto)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void connectto::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(connectto)
	DDX_Control(pDX, IDC_IPADDR, ipedit);
	DDX_Control(pDX, IDC_PORT, portedit);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(connectto, CDialog)
	//{{AFX_MSG_MAP(connectto)
	ON_EN_CHANGE(IDC_IPADDR, OnChangeIpaddr)
	ON_EN_CHANGE(IDC_PORT, OnChangePort)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// connectto message handlers

void connectto::OnChangeIpaddr()  {
	
	THEIP[ipedit.GetLine(0, THEIP, 128)] = 0; /* null-terminate */
}

void connectto::OnChangePort() {
	char buffah[128]= {0};
	portedit.GetLine( 0, buffah, 128);
	THEPORT = atoi(buffah);
}

void connectto::setdefaults() {
	if (portedit.LineLength() == 0) {
	// empty. set to:
		portedit.ReplaceSel(DEFPORT);
	}
	if (ipedit.LineLength() == 0) {
		ipedit.ReplaceSel(DEFIP);
	}
}

void connectto::OnShowWindow(BOOL bShow, UINT nStatus) {
	CDialog::OnShowWindow(bShow, nStatus);
	setdefaults();
}
