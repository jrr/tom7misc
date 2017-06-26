// whitebrdDoc.cpp : implementation of the CWhitebrdDoc class
//

#include "stdafx.h"
#include "whitebrd.h"

#include "whitebrdDoc.h"
#include "connectto.h"
#include "whitebrdView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdDoc

IMPLEMENT_DYNCREATE(CWhitebrdDoc, CDocument)

BEGIN_MESSAGE_MAP(CWhitebrdDoc, CDocument)
	//{{AFX_MSG_MAP(CWhitebrdDoc)
	ON_COMMAND(TOM7_CONNECTO, OnConnectTo)
	ON_COMMAND(TOM7_PLUSKEY, OnPluskey)
	ON_COMMAND(TOM7_MINUSKEY, OnMinuskey)
	ON_COMMAND(ID_FILE_SAVE, OnFileSave)
	ON_COMMAND(ID_FILE_SAVE_AS, OnFileSaveAs)
	ON_COMMAND(TOM7_FATBRUSH, OnFatbrush)
	ON_COMMAND(TOM7_LINEBRUSH, OnLinebrush)
	ON_COMMAND(TOM7_THICKBRUSH, OnThickbrush)
	ON_COMMAND(TOM7_BRUSH, OnBrush)
	ON_COMMAND(TOM7_SETRED, OnSetred)
	ON_COMMAND(TOM7_SETGREEN, OnSetgreen)
	ON_COMMAND(TOM7_SETBLUE, OnSetblue)
	ON_COMMAND(TOM7_CONTRASTUP, OnContrastup)
	ON_COMMAND(TOM7_CONTRASTDOWN, OnContrastdown)
	ON_COMMAND(TOM7_HUELEFT, OnHueleft)
	ON_COMMAND(TOM7_HUERIGHT, OnHueright)
	ON_COMMAND(TOM7_SETYELLOW, OnSetyellow)
	ON_COMMAND(TOM7_SETMAGENTA, OnSetmagenta)
	ON_COMMAND(TOM7_SETCYAN, OnSetcyan)
	ON_COMMAND(TOM7_SETWHITE, OnSetwhite)
	ON_COMMAND(TOM7_SETBLACK, OnSetblack)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdDoc construction/destruction

CWhitebrdDoc::CWhitebrdDoc()
{
	// TODO: add one-time construction code here
	strokelist = 0;
	myview = 0;
	/* make sucket */
	CR=CG=CB = 0;
	suck.init();
	suck.InstallWD(this);
}

CWhitebrdDoc::~CWhitebrdDoc() {

}

BOOL CWhitebrdDoc::OnNewDocument()
{
//	static int ONCE;
//	if (!ONCE) {
	if (!CDocument::OnNewDocument())
		return FALSE;
	InitDocument();
	return TRUE;
//	ONCE++;
//
//	return FALSE;
}



/////////////////////////////////////////////////////////////////////////////
// CWhitebrdDoc serialization

void CWhitebrdDoc::Serialize(CArchive& ar)
{
	// screw serialization. No saving!

	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdDoc diagnostics

#ifdef _DEBUG
void CWhitebrdDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CWhitebrdDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CWhitebrdDoc commands

CStroke* CWhitebrdDoc::newstroke() {
	CStroke * strokeitem = new CStroke(penwidth);
	strokelist = new nodey(strokeitem,strokelist);
	SetModifiedFlag();
	return strokeitem;
}

void CWhitebrdDoc::InitDocument() {
	penwidth = 2;
	
	strokelist = 0;
}

BOOL CWhitebrdDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
	if (!CDocument::OnOpenDocument(lpszPathName))
		return FALSE;
	
	InitDocument();
	
	return TRUE;
}

void CWhitebrdDoc::DeleteContents() 
{
	// TODO: Add your specialized code here and/or call the base class
	
	while (strokelist) {
		nodey * tmp = strokelist->next;
		delete strokelist;
		strokelist= tmp;
	}
	CDocument::DeleteContents();
}

void CWhitebrdDoc::OnConnectTo() 
{
		// create a motherfucken dialog box to ask for the addresses:

	connectto booty;

	if (IDOK == booty.DoModal()) 

	MakeConnection(booty.THEIP,booty.THEPORT);

  // MessageBox(booty.THEIP,"Poop",MB_OK);

  //	booty.MessageBox(booty.THEIP);

}
void assaugebox(char * text);
void CWhitebrdDoc::MakeConnection(char * haddr, UINT hport) {
	
	suck.Connect(haddr,hport);
	suck.Send("Here\n\n",6);

}
/*
void assaugebox(char * text) { MessageBox(text,"Hi", MB_OK); }
*/

void CWhitebrdDoc::OnPluskey() {
		if ( ++ penwidth > MAXPENW) penwidth = MAXPENW;
}

void CWhitebrdDoc::OnMinuskey() {
		if ( -- penwidth == 0) penwidth = 1;
}

void CWhitebrdDoc::madeconnection() {
	MsgBox("Connected!");
	myview -> startreceive ();
}

void CWhitebrdDoc::OnFileSave() 
{
	// TODO: Add your command handler code here
	MsgBox("FileSave");
}

void CWhitebrdDoc::OnFileSaveAs() 
{
	// TODO: Add your command handler code here
	MsgBox("Filesaveas");	
}

void CWhitebrdDoc::OnFatbrush() {
	penwidth= MAXPENW;
}

void CWhitebrdDoc::OnLinebrush() {
	penwidth=1;
}

void CWhitebrdDoc::OnThickbrush() {
	penwidth=8;
}

void CWhitebrdDoc::OnBrush() {
	penwidth=3;	
}

void CWhitebrdDoc::OnSetred() {
	CB = CG = 0;
	CR = 200;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetgreen() {
	CB = CR = 0;
	CG = 200;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetblue() {
	CR = CG = 0;
	CB = 200;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnContrastup() {
	int avg = (CR+CG+CB)/3;
	// move everything away from the average:
	int a;
	CR = ( (a = CR+((CR-avg)/1.85))>255)?255:((a<0)?0:a);
	CG = ( (a = CG+((CG-avg)/1.85))>255)?255:((a<0)?0:a);
	CB = ( (a = CB+((CB-avg)/1.85))>255)?255:((a<0)?0:a);
	myview->updatescrollies
		();		
}

void CWhitebrdDoc::OnContrastdown() {
	int avg = (CR+CG+CB)/3;
	// move everything towards the average:
	CR = (3 * CR + avg) >> 2;
	CG = (3 * CG + avg) >> 2;
	CB = (3 * CB + avg) >> 2;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnHueleft() {
	int t = CB;
	CB = (3* CB + CG)>>2;
	CG = (3* CG + CR)>>2;
	CR = (3* CR + t )>>2;
	// prevent from getting muddled:
	OnContrastup();
	//myview->updatescrollies();
}

void CWhitebrdDoc::OnHueright() {
	int t = CR;
	CR = (3 * CR + CG) >>2;
	CG = (3 * CG + CB) >>2;
	CB = (3 * CB + t)  >>2;
	// prevent from getting muddled:
	OnContrastup();

//	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetyellow() {
	CR = CG = 200;
	CB = 0;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetmagenta() {
	CB = CR = 200;
	CG = 0;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetcyan() {
	CB = CG = 200;	
	CR = 0;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetwhite() {
	CR = CG = CB = 255;
	myview->updatescrollies();
}

void CWhitebrdDoc::OnSetblack() {
	CR = CG = CB = 0;
	myview->updatescrollies();
}
