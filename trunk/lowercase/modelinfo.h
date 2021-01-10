
#ifndef _LOWERCASE_MODELINFO_H
#define _LOWERCASE_MODELINFO_H

#include <memory>
#include <string>
#include <cstdint>
#include <cmath>

#include "image.h"
#include "network.h"

ImageRGBA ModelInfo(const Network &net, int width, int height);

#endif
