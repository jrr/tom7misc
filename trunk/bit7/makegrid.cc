
#include <cstdint>

#include "image.h"

using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

constexpr int CHARS_ACROSS = 16;
constexpr int CHARS_DOWN = 8;

int main(int argc, char **argv) {
  // XXX from command-line.
  const int CHAR_WIDTH = 8;
  const int CHAR_HEIGHT = 8;

  ImageRGBA grid{CHAR_WIDTH * CHARS_ACROSS, CHAR_HEIGHT * CHARS_DOWN};
  grid.Clear(0, 0, 0, 0xFF);

  static constexpr uint32 ODD_COLOR = 0x000022FF;
  static constexpr uint32 EVEN_COLOR = 0x222300FF;
  
  for (int cy = 0; cy < CHARS_DOWN; cy++) {
    for (int cx = 0; cx < CHARS_ACROSS; cx++) {
      for (int y = 0; y < CHAR_HEIGHT; y++) {
	for (int x = 0; x < CHAR_WIDTH; x++) {
	  int xx = cx * CHAR_WIDTH + x;
	  int yy = cy * CHAR_HEIGHT + y;
	  uint32 c = ((cx + cy) & 1) ? ODD_COLOR : EVEN_COLOR;
	  grid.SetPixel32(xx, yy, c);
	}
      }
    }
  }
  
  // XXX filename including dimensions, or from command line?
  grid.Save("grid.png");

  for (int y = 0; y < CHARS_DOWN; y++) {
    for (int x = 0; x < CHARS_ACROSS; x++) {
      int c = y * CHARS_ACROSS + x;
      printf("%c ", (c < 32 || c >= 127) ? '_' : c);
    }
    printf("\n");
  }
  
  return 0;
}
