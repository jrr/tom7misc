
/* some code courtesy of Wacom and 
   webmaster@clue.demon.co.uk
   */

#include "stdafx.h"

#include <math.h>	// required for example sections only

//#include "DocTabTest.h"
#include "tomtablet.h"
#include "../tomsock.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

//Used for tablet examples
#define FIX_DOUBLE(x)   ((double)(INT(x))+((double)FRAC(x)/65536))	// converts FIX32 to double
#define pi 3.14159265359	// or 3.141592653589793
//End of, used for tablet examples

/////////////////////////////////////////////////////////////////////////////
// tomtablet construction/destruction

tomtablet::tomtablet(HWND windowhandle)
{

		WORD errmode;
		BOOL fResult;
	
		/* suppress error box */
		errmode = SetErrorMode(SEM_NOOPENFILEERRORBOX);
	
		/* try wintab */
		fResult = WTInfo(0,0,NULL);
	
		SetErrorMode(errmode);
		if (!fResult) return;

	//TABLET: Graphics Tablet
	t_hTablet = NULL;		// Tablet context handle, required.
	t_prsNew = 0;
	t_curNew = 0;
	t_ortNew.orAzimuth = 0;		// 0 = north
	t_ortNew.orAltitude = 900;	// 900 = vertical
	t_ortNew.orTwist = 0;
	m_pntMouseLoc = CPoint(0,0);	//JCB initialise to zero

	//EXAMPLE: extra tablet adjustments, used for this example,
	//	they may be useful in some other applications
	t_bTiltSupport = TRUE;	// Table. Is tilt supported
	t_dblAltAdjust = 1;		// Tablet Altitude zero adjust
	t_dblAltFactor = 1;		// Tablet Altitude factor
	t_dblAziFactor = 1;		// Table Azimuth factor

	/* now initialize TOM */

//	MsgBox("Word up.");

	if(IsTabletInstalled()){
//		MsgBox("Dudical.");
		t_hTablet = InitTablet(windowhandle);

//		if (t_hTablet) {
//			MsgBox("Dude!", "DUDE!!", MB_OK);
//
//		}
	}
}

//extern FAR _UnlinkWintab(void); /* word. */
tomtablet::~tomtablet() {
	if (t_hTablet){
		WTClose(t_hTablet);
	}

//	_UnlinkWintab(); /* remove dll */
}

BOOL tomtablet::IsTabletInstalled()
{
	struct	tagAXIS TpOri[3];	// The capabilities of tilt (required)
	double	dblTpvar;				// A temp for converting fix to double (for example)

	BOOL bReturn = TRUE;

	// check if WinTab available.
	if (!WTInfo(0, 0, NULL)) {
//		TRACE("WinTab Services Not Available.");
		return false;
	}

		t_bTiltSupport = WTInfo(WTI_DEVICES,DVC_ORIENTATION,&TpOri);
		if (t_bTiltSupport) {

			// does the tablet support azimuth and altitude?
			if (TpOri[0].axResolution && TpOri[1].axResolution) {

				// convert azimuth resulution to double
				dblTpvar = FIX_DOUBLE(TpOri[0].axResolution);
				// convert from resolution to radians
				t_dblAziFactor = dblTpvar/(2*pi);  
				
				// convert altitude resolution to double
				dblTpvar = FIX_DOUBLE(TpOri[1].axResolution);
				// scale to arbitrary value to get decent line length
				t_dblAltFactor = dblTpvar/1000; 
				 // adjust for maximum value at vertical
				t_dblAltAdjust = (double)TpOri[1].axMax/t_dblAltFactor;
			}
		}
		else {  // no so don't do tilt stuff
			t_bTiltSupport = FALSE;
		}	//end tilt support
	return bReturn;
}

BOOL tomtablet::GetTabState(LPARAM lParam, WPARAM wParam, UINT & nTabPressure, int & nTabAltitude, int & nTabTwist, int & nTabCompass, UINT & nTabCursor) {
	//TABLET: Only return TRUE if something has changed
	PACKET	pkt;	// the current packet
	int nMaxPkts = 1;

			if (WTPacket((HCTX)lParam, wParam, &pkt)) {
				
				UINT  		prsOld = t_prsNew;
				UINT  		curOld = t_curNew;
				ORIENTATION ortOld = t_ortNew;

				t_curNew = pkt.pkCursor;
				t_prsNew = pkt.pkNormalPressure;
				t_ortNew = pkt.pkOrientation;

				nTabPressure = t_prsNew;
				nTabCompass = t_ortNew.orAzimuth ;	//Clockwise rotation about z-azis, 0 is north, 900 is east, 1800 is south and 2700 is west, 3599 is nearly north again.
				nTabAltitude = t_ortNew.orAltitude ;	//Tilt.  If Wacom then Positive = normal tip, negative = eraser tip
				nTabTwist = t_ortNew.orTwist ;	//I don't think it's used on Wacom pen tablets
				nTabCursor = t_curNew;	//1=normal 2=eraser.

				// If any state changes return TRUE
				return (t_prsNew != prsOld ||
					t_ortNew.orAzimuth != ortOld.orAzimuth ||
					t_ortNew.orAltitude != ortOld.orAltitude ||
					t_ortNew.orTwist != ortOld.orTwist ||
					t_curNew != curOld);

			}
	return FALSE;
}

void tomtablet::dopacket(WPARAM wParam, LPARAM lParam) {
	UINT nTabPressure = 0;	// 0 = nothing 255 = hardest
	int nTabAlt = 0;	// Which way up for Wacom pens, negative = eraser, positive = normal tip
	int nTabTwist = 0;	// Spin about x axis, I don't think this is used on Wacom pens
	int nTabCompass = 0;	// 0 = North, 900 = east etc.
	UINT nTabCursor = 0;	// A number for the selected cursor or pointer, Wacom 1=normal, 2=eraser
	
	GetTabState(lParam, wParam, nTabPressure, nTabAlt, nTabTwist, nTabCompass, nTabCursor);
}


HCTX tomtablet::InitTablet(HWND hWnd) {
	LOGCONTEXT      lcMine;           // The context of the tablet

	//TABLET: get current settings as a starting point for this context of the tablet.
	//WTInfo(WTI_DEFCONTEXT, 0, &lcMine);	// default settings may be different to current settings
	WTInfo(WTI_DEFSYSCTX, 0, &lcMine);	// current settings as set in control panel

	lcMine.lcOptions |= CXO_MESSAGES;	// keep existing options and make sure message handling is on for this context
	//TABLET: PACKETDATA must be defined FIRST before including pktdef.h. See the header file of this class for more details
	lcMine.lcPktData = PACKETDATA;	// these settings MUST be defined in the pktdef.h file, see notes
	lcMine.lcPktMode = PACKETMODE;
	lcMine.lcMoveMask = PACKETDATA;

	return WTOpen(hWnd, &lcMine, TRUE);

}

void tomtablet::focusoff() {
	if(t_hTablet){
		WTEnable(t_hTablet, FALSE);	// disable packet detection
	}

}

void tomtablet::focuson() {
	if(t_hTablet) {
		WTEnable(t_hTablet, TRUE);	// enable packet detection
	}
}

#if 0
CString tomtablet::GetTabletName() {
	char	chrWName[50];			// String to hold window name
	WTInfo(WTI_DEVICES, DVC_NAME, chrWName);
	CString strName = chrWName;
	return strName;
}
#endif

#if 0
void tomtablet::RepresentPen(CDC * pDC)
{
	//JCB Sample output to represent the tilt, direction and pressure of the pen
	//	This has been converted to MFC from the Tilttest example on the
	//	Wacom site. http://www.wacom.com
	//	ftp://ftp.wacom.com/pub/developer/ibmpc/tilttest.zip

			int nZAngle;         // Raw Altitude
			UINT nThata;         // Raw Azimuth
			double dblZAngle;     // Adjusted Altitude
			double dblThata;      // Adjusted Azimuth
			CPoint ptZAngle;      // Rect coords from polar coords

			if (t_bTiltSupport) {                             
				/* 
				   wintab.h defines .orAltitude 
				   as a UINT but documents .orAltitude 
				   as positive for upward angles 
				   and negative for downward angles.
				   WACOM uses negative altitude values to 
				   show that the pen is inverted; 
				   therefore we cast .orAltitude as an 
				   (int) and then use the absolute value. 
				*/
				nZAngle  = (int)t_ortNew.orAltitude;
				dblZAngle = t_dblAltAdjust - (double)abs(nZAngle)/t_dblAltFactor;
				// adjust azimuth
				nThata  = t_ortNew.orAzimuth;
				dblThata = (double)nThata/t_dblAziFactor;
				// get the length of the diagnal to draw
				ptZAngle.x = (int)(dblZAngle*sin(dblThata));
				ptZAngle.y = -1 * (int)(dblZAngle*cos(dblThata));
			}
			else {
				ptZAngle.x = 0;
				ptZAngle.y = 0;
			}

			// change fill colour based on cursor
			CBrush sbrCursor;
			if(sbrCursor.CreateSolidBrush( ChangeColour(t_curNew) )){
				pDC->SelectObject( &sbrCursor );
			}

			// draw circle based on tablet pressure
			pDC->Ellipse(m_pntMouseLoc.x - t_prsNew, m_pntMouseLoc.y - t_prsNew,
					m_pntMouseLoc.x + t_prsNew, m_pntMouseLoc.y + t_prsNew);

			// draw a line based on tablet tilt
			pDC->MoveTo(m_pntMouseLoc);
			pDC->LineTo(m_pntMouseLoc + ptZAngle);
			
			// draw CROSS based on tablet position
			pDC->MoveTo(m_pntMouseLoc.x - 20,m_pntMouseLoc.y     );
			pDC->LineTo(m_pntMouseLoc.x + 20,m_pntMouseLoc.y     );
			pDC->MoveTo(m_pntMouseLoc.x     ,m_pntMouseLoc.y - 20);
			pDC->LineTo(m_pntMouseLoc.x     ,m_pntMouseLoc.y + 20);

}
#endif