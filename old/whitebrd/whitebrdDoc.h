// whitebrdDoc.h : interface of the CWhitebrdDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_WHITEBRDDOC_H__A878E236_81F9_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_WHITEBRDDOC_H__A878E236_81F9_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "Stroke.h"
#include "Tomsock.h"

class CWhitebrdView;

#define MAXPENW 15

#define TRGB(r,g,b) ((((b)&255)<<16)|(((g)&255)<<8)|((r)&255))

struct nodey {
	CStroke * stroke;
	nodey * next;
	nodey (CStroke *s,nodey*n) {stroke=s;next=n;}
};

class CWhitebrdDoc : public CDocument
{
protected: // create from serialization only
	CWhitebrdDoc();
	DECLARE_DYNCREATE(CWhitebrdDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CWhitebrdDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual void DeleteContents();
	//}}AFX_VIRTUAL

// Implementation
public:
	void madeconnection();
	unsigned char CB;
	unsigned char CG;
	unsigned char CR;
	//CBitmap bootmap;

	CStroke* newstroke();
	virtual ~CWhitebrdDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	nodey * strokelist;
//	CPen*   GetCurrentPen( ) { return &pencur; }
	Tomsock suck;
	UINT penwidth;

	CWhitebrdView * myview;
	void InstallWV(CWhitebrdView * s) { myview = s; }
	void MakeConnection(char * haddr, UINT hport);
protected:

// Generated message map functions
protected:
	void InitDocument();
	//CPen pencur;

	//{{AFX_MSG(CWhitebrdDoc)
	afx_msg void OnConnectTo();
	afx_msg void OnPluskey();
	afx_msg void OnMinuskey();
	afx_msg void OnFileSave();
	afx_msg void OnFileSaveAs();
	afx_msg void OnFatbrush();
	afx_msg void OnLinebrush();
	afx_msg void OnThickbrush();
	afx_msg void OnBrush();
	afx_msg void OnSetred();
	afx_msg void OnSetgreen();
	afx_msg void OnSetblue();
	afx_msg void OnContrastup();
	afx_msg void OnContrastdown();
	afx_msg void OnHueleft();
	afx_msg void OnHueright();
	afx_msg void OnSetyellow();
	afx_msg void OnSetmagenta();
	afx_msg void OnSetcyan();
	afx_msg void OnSetwhite();
	afx_msg void OnSetblack();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_WHITEBRDDOC_H__A878E236_81F9_11D2_A014_0080C8443AA1__INCLUDED_)

