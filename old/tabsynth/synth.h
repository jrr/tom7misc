// synth.h: interface for the synth class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SYNTH_H__50DF7117_B480_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_SYNTH_H__50DF7117_B480_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000


#include "mmsystem.h"
#include "dsound.h"
#include <string>
using namespace std;

#include "tone.h"

struct scont {
	char * name;
	int id;
	char * description;
};

class synth {
public:
	synth(HWND);
	~synth();

	operator bool() { return okay; }
	scont * enumerate();

	void start();

	void stop();

	void setfreq(DWORD x) { delme->setfreq(x); 
							sine ->setfreq(x); 
	}
	void setshape(int x) {
		delme -> setvol(x*(DSBVOLUME_MIN/1200));
		sine ->setvol((500-x)*(DSBVOLUME_MIN/1200));
	}

	int flag;
private:
	tone * delme, * sine;
	LPDIRECTSOUND directs;
	bool okay;
	string diagnostic;
};

#endif // !defined(AFX_SYNTH_H__50DF7117_B480_11D2_A014_0080C8443AA1__INCLUDED_)
