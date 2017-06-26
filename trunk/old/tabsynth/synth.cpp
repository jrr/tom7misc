// synth.cpp: implementation of the synth class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "Tabsynth.h"
#include "synth.h"

#include "util.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

// enumerated controllers for this synth:
enum { C_VOL=1, C_FRQ, C_PAN, };

scont cntrls[] = {
	{"Volume", C_VOL, "The volume of the output"},
	{"Frequency", C_FRQ, "The frequency (pitch) of the waveform"},
	{"Pan", C_PAN, "The pan (stereo location) of the output"},
	{0,0,0}
};

scont * synth::enumerate() {
	return cntrls;
}

synth::synth(HWND windowhandle) {
	okay = false;
	directs = 0;
	flag = 42;
	if (DS_OK == DirectSoundCreate(0,&directs,0)) {
		if (DS_OK == IDirectSound_SetCooperativeLevel
			(directs, windowhandle, DSSCL_NORMAL)) {
                              // ->SetCooperativeLevel
			// ok!
			delme = new tone("square.wav", directs);
			sine  = new tone("sine.wav", directs);
			return;
		} else diagnostic = "Couldn't set Cooperative Mode.";
	} else diagnostic = "Couldn't create DirectSound object.";
	MsgBox(diagnostic.c_str(), "No directS!");
}

synth::~synth() {
	stop();
	if (directs) IDirectSound_Release(directs);
}

void synth::start() {
	if (*delme) delme -> start(); 
	if (*sine) sine -> start();
}

void synth::stop() {
	delme -> stop ();
	sine -> stop ();
}
