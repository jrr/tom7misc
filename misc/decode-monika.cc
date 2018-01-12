#include "../cc-lib/stb_image.h"

#include <memory>

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "image.h"

using namespace std;

// Width and height of an individual letter in pixels.
#define WIDTH 8
#define HEIGHT 8
// Size of a pixel in anagraph grid coordinates. Should be even.
#define SCALE 4

int main(int argc, char **argv) {
  unique_ptr<ImageRGBA> image{ImageRGBA::Load("monika.chr.png")};
  CHECK(image.get() != nullptr);

  auto Bit = [&image](int x) -> int {
    return (image->rgba[x * 4] > 128) ? 1 : 0;
  };
  
  for (int i = 0; i < image->width * image->height; i += 8) {
    /*
    if (i < 10)
    printf("%c%c%c%c%c\n",
	   (Bit(i + 0) ? '#' : '_'),
	   (Bit(i + 1) ? '#' : '_'),
	   (Bit(i + 2) ? '#' : '_'),
	   (Bit(i + 3) ? '#' : '_'),
	   (Bit(i + 4) ? '#' : '_'));
    */	   
    int c =
      Bit(i + 0) * 128 +
      Bit(i + 1) * 64 +      
      Bit(i + 2) * 32 +
      Bit(i + 3) * 16 +
      Bit(i + 4) * 8 +
      Bit(i + 5) * 4 +
      Bit(i + 6) * 2 +
      Bit(i + 7) * 1;
    printf("%c", c);
    // if (i % 140 == 0) printf("\n");
  }
  // printf("\n");
}
