// Tomsock.h: interface for the Tomsock class.
//
//////////////////////////////////////////////////////////////////////
#undef _STRING_

#include <string>
//typedef basic_string<char> string;
using namespace std;

#if !defined(AFX_TOMSOCK_H__A8D260E1_8477_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_TOMSOCK_H__A8D260E1_8477_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

void MsgBox(const char*a,const char*b=0,int c=0);

enum { SEND_ERROR, SEND_SENT, SEND_QUEUED };

struct stringq {
	string st;
	stringq * next;
	stringq (string s, stringq * n) { st=s; next=n; }
};

class CWhitebrdDoc;

class Tomsock : public CAsyncSocket  
{
public:
	int beserver(int port);
	void addtoreturnqueue(string s);
	string getmsg();
	void OnSend(int n);
	void clearqueue();
	void addtoqueue(string s);
	int sendmsg(string s);
	void init();
	Tomsock();
	virtual ~Tomsock();
    void OnConnect( int nErrorCode );
	void OnReceive( int );
	int connected;
	stringq * queue, * qtail,
		    *iqueue, *iqtail;
	CWhitebrdDoc * doc;
	void InstallWD(CWhitebrdDoc * s) { doc = s; }
};

#endif // !defined(AFX_TOMSOCK_H__A8D260E1_8477_11D2_A014_0080C8443AA1__INCLUDED_)
