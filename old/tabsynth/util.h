// util.h: interface for the util class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_UTIL_H__CB1EC587_B794_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_UTIL_H__CB1EC587_B794_11D2_A014_0080C8443AA1__INCLUDED_

#include "mapper.h"

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

string itos(const int);
void MsgBox(const char*a,const char*b=0,int c=0);
void MsgBox(string s, string m="", int c=MB_OK);

extern mapper delme_map;


#endif // !defined(AFX_UTIL_H__CB1EC587_B794_11D2_A014_0080C8443AA1__INCLUDED_)
