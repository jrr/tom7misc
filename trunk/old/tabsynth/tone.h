// tone.h: interface for the tone class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_TONE_H__02786C26_B560_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_TONE_H__02786C26_B560_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "wave.h"

class tone {
public:
	void setvol(DWORD x);
	tone(char * wavefile,LPDIRECTSOUND);
	~tone();
	void start (), stop ();
	operator bool(){return !!valid;}
	void setfreq(DWORD i) { if (valid)IDirectSoundBuffer_SetFrequency(buff,i); }

private:
	LPDIRECTSOUND directs;
	int size;
	unsigned long samples;
	WAVEFORMATEX * wfx;
	DSBUFFERDESC desc;
	BYTE * data;
	char valid;
	LPDIRECTSOUNDBUFFER buff;
};

#endif // !defined(AFX_TONE_H__02786C26_B560_11D2_A014_0080C8443AA1__INCLUDED_)
