// PrgTabTest.h : interface of the PrgTabTest class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_TOMTABLET_H__7A49D2CC_2E9C_11D2_B19E_0060084FA860__INCLUDED_)
#define AFX_TOMTABLET_H__7A49D2CC_2E9C_11D2_B19E_0060084FA860__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
//TABLET: setup required here
//
#include <wintab.h>

#define PACKETDATA	(PK_NORMAL_PRESSURE | PK_ORIENTATION | PK_CURSOR)
#define PACKETMODE      0
#include <pktdef.h>

class tomtablet {
public:
	tomtablet(HWND);

	HCTX t_hTablet;			// Tablet context
	BOOL t_bTiltSupport;	// TRUE if tablet provides tilt and direction information
	//	current pen statistics
	UINT            t_prsNew;		// Pressure value
	UINT            t_curNew;		// Cursor number
	ORIENTATION     t_ortNew;		// Varios movement values, direction and tilt
	CPoint			m_pntMouseLoc;	// Mouse pointer location, which is also the pen location
	//	tablet specific functions
	HCTX InitTablet(HWND hWnd);

	void dopacket(WPARAM wParam, LPARAM lParam);
	BOOL GetTabState(LPARAM lParam, WPARAM wParam, UINT & nTabPressure, int & nTabAltitude, int & nTabTwist, int & nTabCompass, UINT & nTabCursor);
	BOOL IsTabletInstalled();
	CString GetTabletName();
	//  used for example
	void focuson(), focusoff();
	//	adjustments used in example
	double t_dblAltAdjust;		// Tablet Altitude zero adjust
	double t_dblAltFactor;	// Tablet Altitude factor
	double t_dblAziFactor;	// Table Azimuth factor

	~tomtablet();

};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TOMTABLET_H__7A49D2CC_2E9C_11D2_B19E_0060084FA860__INCLUDED_)
