// util.cpp: implementation of the util class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "Tabsynth.h"
#include "util.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

mapper delme_map;

string itos(const int x) {
	char a[64];
	sprintf(a,"%d",x);
	return a;
}

void MsgBox(const char*a,const char*b,int c) {
	CWnd crap;
	crap.MessageBox(a,b,c);
}

void MsgBox(string s, string m, int c) {
	MsgBox(s.c_str(),m.c_str(),c);
}