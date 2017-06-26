// Stroke.cpp: implementation of the CStroke class.
//
//////////////////////////////////////////////////////////////////////
#define AFXDLL
#include "stdafx.h"
#include "whitebrd.h"
#include "Stroke.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CStroke :: CStroke (){}
CStroke :: CStroke (UINT pwidth) {
	penwidth = pwidth;
}
/*
void CStroke :: Serialize ( CArchive & ar ){
 // ha!
}
*/
BOOL CStroke :: drawstroke ( CDC * pDC) {
	CPen penstroke;
	if (!penstroke.CreatePen(PS_SOLID, penwidth, RGB(0,0,0)))
		return FALSE;

	CPen * old = pDC->SelectObject( &penstroke );
	pDC -> MoveTo ( pointarray[0] );
	for (int i=1; i < pointarray.GetSize(); i++) {
		pDC -> LineTo ( pointarray[i] );
	}
	pDC->SelectObject ( old );
	return TRUE;
}

// Jesus...
/*
const AFX_DATADEF CRuntimeClass CStroke::classCStroke = { 
		"CStroke", sizeof(class CStroke), 1, 0, 
			&CStroke::_GetBaseClass, NULL };


struct CRuntimeClass * CStroke::GetRuntimeClass(void)const {
	return &classCStroke;
}
*/

//IMPLEMENT_RUNTIMECLASS(CStroke, CObject, 1, 0)