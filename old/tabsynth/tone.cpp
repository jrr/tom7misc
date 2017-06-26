// tone.cpp: implementation of the tone class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "Tabsynth.h"
#include "tone.h"

#include <stdlib.h>

#include "util.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
char a[512];
tone::tone(char * wavefile, LPDIRECTSOUND dirs) {
	valid = 0;
	directs = dirs;
//	return;
	if ( loadawave(wavefile, &size, &wfx, &data)) {

		desc.dwReserved=0;
		desc.dwFlags = DSBCAPS_CTRLDEFAULT | DSBCAPS_STATIC;
		desc.dwSize = sizeof (DSBUFFERDESC);
		desc.dwBufferBytes = size;
		desc.lpwfxFormat = wfx;

		if (DS_OK == IDirectSound_CreateSoundBuffer(
							directs,
							&desc,
							&buff,
							0)) {
			/* copy data in: */
			BYTE * d1 = 0, * d2 =0;
			DWORD l1, l2;
			if (DS_OK == IDirectSoundBuffer_Lock(buff,
											0,
											size,
											(void**)&d1,&l1,
											(void**)&d2,&l2,
											0L)) {
	
//	sprintf(a, "size: %d: %p(%d), %p(%d)",size,d1,l1,d2,l2);

				//FILE * grr = fopen("c:\\slut.raw","wb+");
				//for (int xx=0;xx<(size);xx++)
				//	fputc(data[xx],grr);
				//fclose(grr);

				memcpy(d1, data, l1);
//				for (int xx=0;xx<(l1>>1);xx++) {
//					((short*)data)[xx] = data[xx]; //rand();
//				}
				if (l2) memcpy(d2, data+l1, l2);
				/* free wav */
				
				//free(data);

				if (DS_OK == IDirectSoundBuffer_Unlock(buff,d1,l1,d2,l2)) {
	
					valid = 1;
				
				} else MsgBox("Unlock Failed!");
			} else MsgBox("Lock Failed");
		} else MsgBox("No CreateBuffer");
	} else MsgBox ("Loadwave failed...");

	if (!valid) { MsgBox("Tone Create failed"); }
}


void tone::start () {
//	IDirectSoundBuffer_SetCurrentPosition( buff, 0 );
	if (valid) IDirectSoundBuffer_Play( buff, 0, 0,  DSBPLAY_LOOPING );  
//	if (a[0]) MsgBox(a);
//	a[0]=0;
}

void tone :: stop ( ) {
	if (valid) IDirectSoundBuffer_Stop(buff);
}

tone::~tone() {
	// deallocate wfx, data...
	// FIXME

	if (valid) IDirectSound_Release(buff);
}

void tone::setvol(DWORD x) {
	if (valid) IDirectSoundBuffer_SetVolume(buff,x);
}
