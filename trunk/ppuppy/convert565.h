
#ifndef __CONVERT565_H
#define __CONVERT565_H

#include "screen.h"
#include <string>
#include <vector>

class ArcFour;

// Convert SNES RGB565 images to Screen format.
// For more general routines, see convert.h.

// Make sure to call this before any of the routines below!
void InitClosestColors565();

void MakePalette565(const void *data,
		    int width, int height, int pitch,
		    bool offset, Screen *screen);

void FillScreenFast565(const void *data,
		       int width, int height, int pitch,
		       bool offset, Screen *screen);

// data is flat array of 256x240 pixels, each of whose
// low 6 bits are an NTSC NES palette index.
void MakePaletteNES(const uint8 *data, Screen *screen);
void FillScreenFastNES(const uint8 *data, Screen *screen);

#endif
