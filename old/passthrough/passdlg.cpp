
// Copyright (c) 1999 Tom 7 - No warranty. Distributed under the GNU
// Public License: http://www.gnu.org/copyleft/gpl.html


// passdlg.cpp : implementation file
//

#include "stdafx.h"
#include "passthru.h"
#include "passdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
/*
using namespace std;
#include <string>
*/
const char * itoa(int d);

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

	HMIDIOUT        outhandle;
	MIDIHDR			midiHdr;
	unsigned char SysXBuffer[256];
	unsigned char SysXFlag = 0;


class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Passdlg dialog

void Passdlg::initdevices() {

//	MessageBox("I am poop","poop", MB_OK);

	devicein.ResetContent();
	deviceout.ResetContent();
//	MessageBox("Me too","poop", MB_OK);

//  deviceout.AddString("POOP");

	int i;
	int count = midiOutGetNumDevs();
	for (i = 0; i < count; i++)
	    if (!midiOutGetDevCaps(i, &moc, sizeof(MIDIOUTCAPS)))
		{
			deviceout.InsertString(-1,moc.szPname);
//			MessageBox(moc.szPname,"POOP",MB_OK);
		}
//		MessageBox("Dude.","poop", MB_OK);

	count = midiInGetNumDevs();
	for (i = 0; i < count; i++)
	    if (!midiInGetDevCaps(i, &mic, sizeof(MIDIINCAPS)))
			devicein.InsertString(-1, mic.szPname);

//	MessageBox(itoa(deviceout.GetCount()), "UH", MB_OK);
//	MessageBox(itoa(devicein.GetCount()), "UH IN", MB_OK);
//	MessageBox("Oh yeah?","poop", MB_OK);

}

Passdlg::Passdlg(CWnd* pParent /*=NULL*/)
	: CDialog(Passdlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(Passdlg)
	//}}AFX_DATA_INIT
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void Passdlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(Passdlg)
	DDX_Control(pDX, IDC_LISTTO, deviceout);
	DDX_Control(pDX, IDC_LISTFROM, devicein);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(Passdlg, CDialog)
	//{{AFX_MSG_MAP(Passdlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_LBN_SELCHANGE(IDC_LISTFROM, onchangefrom)
	ON_LBN_SELCHANGE(IDC_LISTTO, onchangeto)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Passdlg message handlers

BOOL Passdlg::OnInitDialog()
{

	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	// TODO: Add extra initialization here

//	initdevices();

	devicein.ResetContent();
	deviceout.ResetContent();
//	MessageBox("Me too","poop", MB_OK);

	//ResetContent

//  deviceout.AddString("POOP");

	int i;
	int count = midiOutGetNumDevs();
	for (i = 0; i < count; i++)
	    if (!midiOutGetDevCaps(i, &moc, sizeof(MIDIOUTCAPS)))
			deviceout.InsertString(-1,moc.szPname);

	count = midiInGetNumDevs();
	for (i = 0; i < count; i++)
	    if (!midiInGetDevCaps(i, &mic, sizeof(MIDIINCAPS)))
			devicein.InsertString(-1, mic.szPname);

	handle = 0;
	outhandle = 0;

	if (defaultin != -1) {
		devicein.SetCurSel(defaultin);
		UpdateData(FALSE);
		onchangefrom();		
	} 

	if (defaultout != -1) {
		deviceout.SetCurSel(defaultout);
		UpdateData(FALSE);
		onchangeto();		
	} 

	return TRUE;  // return TRUE  unless you set the focus to a control
}

void Passdlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void Passdlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

HCURSOR Passdlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

static char boof[128];

const char * itoa(int d) {
	sprintf(boof,"%d", d);
	return boof;
}

void CALLBACK midicallback(HMIDIIN handle, UINT uMsg, DWORD dwInstance, DWORD, DWORD);

void CALLBACK midicallback(HMIDIIN handle, UINT uMsg, DWORD dwInstance, DWORD dwParam1, DWORD dwParam2) {

	LPMIDIHDR		lpMIDIHeader;
	unsigned char *	ptr;
	//TCHAR			buffer[80];
	unsigned char 	bytes;
	unsigned long   err;

	/* Determine why Windows called me */
	switch (uMsg) {
		/* Received some regular MIDI message */
		case MIM_DATA:

			if (!outhandle) return;

			/* Display the time stamp, and the bytes. (Note: I always display 3 bytes even for
			Midi messages that have less) */
			
			if (err = midiOutShortMsg(outhandle, dwParam1)) {
				((CDialog*)0)->MessageBox("Can't Send Message", "ERROR", MB_OK);
				outhandle = 0;
			}

			break;

		/* Received all or part of some System Exclusive message */
		case MIM_LONGDATA:

			/*	Assign address of MIDIHDR to a LPMIDIHDR variable. Makes it easier to access the
				field that contains the pointer to our block of MIDI events */
			lpMIDIHeader = (LPMIDIHDR)dwParam1;

			/* Get address of the MIDI event that caused this call */
			ptr = (unsigned char *)(lpMIDIHeader->lpData);

			/* Is this the first block of System Exclusive bytes? */
			if (!SysXFlag)
			{
				/* Print out a noticeable heading as well as the timestamp of the first block.
					(But note that other, subsequent blocks will have their own time stamps). */
//				printf("*************** System Exclusive **************\r\n0x%08X ", dwParam2);

				/* Indicate we've begun handling a particular System Exclusive message */
				SysXFlag = 1;
			}

			/* Is this the last block (ie, the end of System Exclusive byte is here in the buffer)? */
			if (*(ptr + (lpMIDIHeader->dwBytesRecorded - 1)) == 0xF7)
			{
				/* Indicate we're done handling this particular System Exclusive message */
				SysXFlag = 0;
			}

			/* Display the bytes -- 16 per line */
			bytes = 16;

			while((lpMIDIHeader->dwBytesRecorded--))
			{
/*
				if (!(--bytes))
				{
					sprintf(&buffer[0], "0x%02X\r\n", *(ptr)++);
					bytes = 16;
				}
				else
					sprintf(&buffer[0], "0x%02X ", *(ptr)++);

				_cputs(&buffer[0]);
*/
			bytes=0;
			}

			/* Was this the last block of System Exclusive bytes? */
			if (!SysXFlag)
			{
				/* Print out a noticeable ending */
//				_cputs("\r\n******************************************\r\n");
			}

			/* Queue the MIDIHDR for more input */
			midiInAddBuffer(handle, lpMIDIHeader, sizeof(MIDIHDR));

			break;

		/* Process these messages if you desire */
/*
		case MIM_OPEN:
        case MIM_CLOSE:
        case MIM_ERROR:
        case MIM_LONGERROR:
        case MIM_MOREDATA:

			break;
*/
  }
}


void Passdlg::onchangefrom() {
	unsigned long err;
	UpdateData(TRUE);
	if (handle) {

		/* close old input */
		
		midiInReset(handle);

		midiInUnprepareHeader(handle, &midiHdr, sizeof(MIDIHDR));

		
		midiInClose(handle);

	}
	if (!(err = midiInOpen(&handle, devicein.GetCurSel(), (DWORD)midicallback, 0, CALLBACK_FUNCTION)))
	{
		/* Store pointer to our input buffer for System Exclusive messages in MIDIHDR */
		midiHdr.lpData = (char*)(LPBYTE)&SysXBuffer[0];

		/* Store its size in the MIDIHDR */
		midiHdr.dwBufferLength = sizeof(SysXBuffer);

		/* Flags must be set to 0 */
		midiHdr.dwFlags = 0;

		/* Prepare the buffer and MIDIHDR */
		err = midiInPrepareHeader(handle, &midiHdr, sizeof(MIDIHDR));
		if (!err)
		{
			/* Queue MIDI input buffer */
			err = midiInAddBuffer(handle, &midiHdr, sizeof(MIDIHDR));
			if (err)
			{
bad://			PrintMidiInErrorMsg(err);
				midiInUnprepareHeader(handle, &midiHdr, sizeof(MIDIHDR));
				midiInClose(handle);
				handle = 0;
				return;
			}
			else
			{
				/* Start recording Midi */
				err = midiInStart(handle);
				if (err) goto bad;

				/* ok */
			}
		} else {
			MessageBox("Error opening MIDI IN device!", "AGH!", MB_OK);
			handle = 0;
			return;
		}
	} else {
		MessageBox("Error opening the default MIDI In Device!", "ACK!", MB_OK);
		handle = 0;
		return;
	}
}

void Passdlg::onchangeto()  {
	UpdateData(TRUE);
	
	if (outhandle) {
		midiOutClose(outhandle);
		outhandle=0;
	}

	unsigned int
	err = midiOutOpen(&outhandle, deviceout.GetCurSel(), 0, 0, CALLBACK_NULL);
	if (err) {
		MessageBox("Can't open that MIDI OUT device!", "OOPS", MB_OK);
		outhandle=0;
	}
}
