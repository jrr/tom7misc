// Stroke.h: interface for the CStroke class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_STROKE_H__A878E240_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_STROKE_H__A878E240_81F9_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class CStroke
{
public:
		CStroke( UINT nPenWidth );
	protected:
		CStroke( );
    //DECLARE_DYNAMIC( CStroke );
	protected:
    UINT   penwidth;
	public:
    CArray<CPoint, CPoint> pointarray;
	public:
	BOOL drawstroke( CDC* pDC );
	public:
    //virtual void Serialize( CArchive& ar );
};

#endif // !defined(AFX_STROKE_H__A878E240_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
