
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
		    uint8 *palette_cache,
		    ArcFour *rc, Screen *screen);

void FillScreenFast565(const void *data,
		       int width, int height, int pitch,
		       Screen *screen);

#endif
