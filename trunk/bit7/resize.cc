// "Resize" a font (just for reference or padding) by blitting
// cropped characters 1:1.

#include <cstdint>
#include <string>

#include "image.h"

using namespace std;
using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

constexpr int CHARS_ACROSS = 16;
constexpr int CHARS_DOWN = 8;

int main(int argc, char **argv) {
  // XXX all this from command-line.
  const int OLD_CHAR_WIDTH = 8;
  const int OLD_CHAR_HEIGHT = 8;
  const string old_png = "test8x8.png";

  const int NEW_CHAR_WIDTH = 9;
  const int NEW_CHAR_HEIGHT = 9;
  // const string new_png = "test9x9.png";

  ImageRGBA *input = ImageRGBA::Load(old_png);
  CHECK(input) << old_png;
  ImageRGBA out{NEW_CHAR_WIDTH * CHARS_ACROSS, NEW_CHAR_HEIGHT * CHARS_DOWN};
  out.Clear(0, 0, 0, 0);

  for (int cy = 0; cy < CHARS_DOWN; cy++) {
    for (int cx = 0; cx < CHARS_ACROSS; cx++) {
      for (int y = 0; y < NEW_CHAR_HEIGHT; y++) {
	for (int x = 0; x < NEW_CHAR_WIDTH; x++) {
	  if (x < OLD_CHAR_WIDTH && y < OLD_CHAR_HEIGHT) {
	    int sx = cx * OLD_CHAR_WIDTH + x;
	    int sy = cy * OLD_CHAR_HEIGHT + y;
	    uint32 c = input->GetPixel(sx, sy);
	    int dx = cx * NEW_CHAR_WIDTH + x;
	    int dy = cy * NEW_CHAR_HEIGHT + y;
	    out.SetPixel32(dx, dy, c);
	  }
	}
      }
    }
  }
  
  // XXX filename including dimensions, or from command line?
  out.Save("resize.png");

  delete input;
  return 0;
}
