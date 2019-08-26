
#include <cstdint>

#include "image.h"

using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

constexpr int CHARS_ACROSS = 16;
constexpr int CHARS_DOWN = 8;

int main(int argc, char **argv) {
  // XXX from command-line.
  const int CHAR_WIDTH = 9;
  const int CHAR_HEIGHT = 9;

  ImageRGBA grid{CHAR_WIDTH * CHARS_ACROSS, CHAR_HEIGHT * CHARS_DOWN};
  grid.Clear(0, 0, 0, 0xFF);

  static constexpr uint32 ODD_COLOR = 0x000033FF;
  static constexpr uint32 ODD_BORDER = 0x000027FF;
  static constexpr uint32 EVEN_COLOR = 0x333300FF;
  static constexpr uint32 EVEN_BORDER = 0x272700FF;
  
  for (int cy = 0; cy < CHARS_DOWN; cy++) {
    for (int cx = 0; cx < CHARS_ACROSS; cx++) {
      for (int y = 0; y < CHAR_HEIGHT; y++) {
	for (int x = 0; x < CHAR_WIDTH; x++) {
	  int xx = cx * CHAR_WIDTH + x;
	  int yy = cy * CHAR_HEIGHT + y;
	  bool border = y == CHAR_HEIGHT - 1 || x == CHAR_WIDTH - 1;
	  uint32 c = ((cx + cy) & 1) ? 
	    (border ? ODD_BORDER : ODD_COLOR) :
	    (border ? EVEN_BORDER : EVEN_COLOR);
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
