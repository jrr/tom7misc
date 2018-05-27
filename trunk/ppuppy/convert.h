
#ifndef __CONVERT_H
#define __CONVERT_H

#include "screen.h"
#include <string>
#include <vector>

class ArcFour;

// Convert images to Screen format.

Screen ScreenFromFileDithered(const string &filename);

Screen ScreenFromFile(const string &filename);

std::vector<Screen> MultiScreenFromFile(const string &filename);

// Dense r-g-b triplets.
// TODO: to cc-lib image.h?
struct ImageRGB {
  ImageRGB(std::vector<uint8> rgb, int width, int height) :
    width(width), height(height), rgb(std::move(rgb)) {}
  ImageRGB(int width, int height) :
    width(width), height(height) {
    rgb.resize(width * height * 3);
  }
  static ImageRGB *Load(const string &filename);
  ImageRGB *Clone() const {
    return new ImageRGB(rgb, width, height);
  }
  const int width, height;
  // Size width * height * 3.
  std::vector<uint8> rgb;
};

enum class PaletteMethod {
  // TODO: fixed, etc.
  GREYSCALE,
  MOST_COMMON,
  MOST_COMMON_SHUFFLED,
  GREEDY_BIGRAMS,
};

void MakePalette(PaletteMethod method, const ImageRGB *img,
		 ArcFour *rc, bool offset,
		 const vector<int> &forced,
		 Screen *screen);

// Put the magic bytes in unused palette slots that tell
// ppuppy to turn off debugging.
inline void NoDebugPalette(Screen *screen) {
  screen->palette[8] = 0x2A;
  screen->palette[12] = 0xA7;
}

void FillScreenSelective(ImageRGB *img, bool offset,
			 Screen *screen);

#endif
