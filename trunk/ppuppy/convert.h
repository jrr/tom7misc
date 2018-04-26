
#ifndef __CONVERT_H
#define __CONVERT_H

#include "screen.h"
#include <string>

// Convert images to Screen format.

Screen ScreenFromFileDithered(const string &filename);

Screen ScreenFromFile(const string &filename);

std::vector<Screen> MultiScreenFromFile(const string &filename);

#endif
