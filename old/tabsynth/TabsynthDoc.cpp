// TabsynthDoc.cpp : implementation of the CTabsynthDoc class
//

#include "stdafx.h"
#include "Tabsynth.h"

#include "util.h"

#include "TabsynthDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTabsynthDoc

IMPLEMENT_DYNCREATE(CTabsynthDoc, CDocument)

BEGIN_MESSAGE_MAP(CTabsynthDoc, CDocument)
	//((AFX_MSG_MAP(CTabsynthDoc)
	// ON_COMMAND(TOM7_TEST_TONE, test_tone)
	//))AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTabsynthDoc construction/destruction

CTabsynthDoc::CTabsynthDoc() {
	// TODO: add one-time construction code here

}

CTabsynthDoc::~CTabsynthDoc() {
}

BOOL CTabsynthDoc::OnNewDocument() {
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CTabsynthDoc serialization

void CTabsynthDoc::Serialize(CArchive& ar) {
	if (ar.IsStoring()) {
		// TODO: add storing code here
	} else {
		// TODO: add loading code here
	}
}

/////////////////////////////////////////////////////////////////////////////
// CTabsynthDoc diagnostics

#ifdef _DEBUG
void CTabsynthDoc::AssertValid() const {
	CDocument::AssertValid();
}

void CTabsynthDoc::Dump(CDumpContext& dc) const {
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CTabsynthDoc commands

