
#ifndef __DECONVERSION_H
#define __DECONVERSION_H

#include "screen.h"
#include "../cc-lib/image.h"

ImageRGBA Deconvert(const Screen &screen);

ImageRGBA NTSCSwatches();

#endif
