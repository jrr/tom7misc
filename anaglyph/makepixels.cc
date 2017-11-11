// Simple program that generates a pixel font ('letters' file)
// from a png file for anaglyph.

#include <memory>

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "image.h"

using namespace std;

// Width and height of an individual letter in pixels.
#define WIDTH 8
#define HEIGHT 8
// Size of a pixel in anaglyph grid coordinates. Should be even.
#define SCALE 4

int main(int argc, char **argv) {
  unique_ptr<ImageRGBA> font{ImageRGBA::Load("pixelfont.png")};
  CHECK(font.get() != nullptr);

  printf("const letters =\n"
	 "{");
  for (int i = 0; i < 26; i++) {
    printf(R"( "%c":{"m":{"w":%d},"p":[)",
	   'a' + i,
	   WIDTH * SCALE);
    // Skip capitals
    const int yy = HEIGHT;
    const int xx = i * WIDTH;
    for (int y = 0; y < HEIGHT; y++) {
      for (int x = 0; x < WIDTH; x++) {
	uint32 p = font->GetPixel(xx + x, yy + y);
	if ((p & 255) > 128) {
	  fprintf(stderr, "#");
	  printf(R"({"a":"a","x":%d,"y":%d,"r":0},)",
		 (x * SCALE) + (SCALE >> 1),
		 // ?
		 /* (HEIGHT * SCALE) - */
		 (y * SCALE) + (SCALE >> 1));
	} else {
	  fprintf(stderr, "_");
	} 
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
    printf("]},\n");
  }

  printf("};\n");
  
  return 0;
}
