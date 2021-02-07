// Mark fonts whose characters for a-z seem to be simple
// transformations (x = ax + b, y = cx + d) of A-Z.

#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>
#include <unordered_set>
#include <mutex>
#include <unordered_map>

#include "util.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "city/city.h"

#include "ttf.h"
#include "stb_truetype.h"
// #include "ttfops.h"

using namespace std;

using uint8 = uint8_t;
using int64 = int64_t;

int main(int argc, char **argv) {

  // TTF ttf{"laser-italic.ttf"};
  TTF ttf{"exedoreli.ttf"};
  const stbtt_fontinfo *info = ttf.Font();

  float stb_scale = stbtt_ScaleForPixelHeight(info, 200.0f);

  int width2, height2, x2, y2;
  uint8 *bit2 = stbtt_GetCodepointBitmapSubpixel(info,
                                                 0.4972374737262726,
                                                 0.4986416995525360,
                                                 0.2391788959503174,
                                                 0.1752119064331055,
                                                 'd',
                                                 &width2, &height2,
                                                 &x2, &y2);
  
  stbtt_FreeBitmap(bit2, nullptr);

  printf("OK.\n");
  return 0;
}



