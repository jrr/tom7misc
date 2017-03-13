
#include <cstdio>
#include <string>
#include <cstdint>
#include <cmath>

#include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image.cc"

using namespace std;

using uint8 = uint8_t;

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr, "Usage: png2ascii.exe 0123456789ABCDEF file.png\n");
    return -1;
  }

  const char *chars = argv[1];
  const char *filename = argv[2];

  fprintf(stderr, "Load %s\n", filename);
  int width, height, bpp;
  uint8 *stb_rgba = stbi_load(filename,
			      &width, &height, &bpp, 4);
  if (!stb_rgba) {
    fprintf(stderr, "Failed to load %s\n", filename);
    return 0;
  }
  if (width != 160 || height != 128) {
    fprintf(stderr, "Expected image of size 160x128.");
    return -1;
  }

  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      int idx = y * width * 4 + x * 4;
      uint8 r = stb_rgba[idx + 0];
      uint8 g = stb_rgba[idx + 1];
      uint8 b = stb_rgba[idx + 2];

      // 2 bit.
      uint8 lum = floor(16.0 * (r + g + b) / (256 + 256 + 256.0));
      printf("%c", chars[lum]);
    }
  }
  
  stbi_image_free(stb_rgba);

  return 0;
}
