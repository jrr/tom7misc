// Create a test pattern.

#include <cstdint>
#include <string>
#include <vector>

#include "util.h"
#include "image.h"
#include "bit7chars.h"

#include "embed9x9.h"

using namespace std;
using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

constexpr int CHARS_ACROSS = 16;
constexpr int CHARS_DOWN = 8;

constexpr int CHAR_WIDTH = EmbeddedFont::CHAR_WIDTH;
constexpr int CHAR_HEIGHT = EmbeddedFont::CHAR_HEIGHT;

constexpr int PX = 3;

int main(int argc, char **argv) {

  string text =
    B7_FILL_7 B7_FILL_6 B7_FILL_5 B7_FILL_4
    "  Welcome to my font!  it is cozy here " B7_SMILE "  (ok) ";
  int cols = text.size();
  
  ImageRGBA out{cols * CHAR_WIDTH * PX, CHAR_HEIGHT * PX};
  out.Clear32(0x000000FF);
  auto SetPixel = [&out](int x, int y) {
    for (int yy = 0; yy < PX; yy++) {
      for (int xx = 0; xx < PX; xx++) {
	out.SetPixel32(x * PX + xx, y * PX + yy, 0xFFFFFFFF);
      }
    }
  };
  auto ClearPixel = [&out](int x, int y) {
    for (int yy = 0; yy < PX; yy++) {
      for (int xx = 0; xx < PX; xx++) {
	out.SetPixel32(x * PX + xx, y * PX + yy, 0x000033FF);
      }
    }
  };
  
  for (int x = 0; x < (int)text.size(); x++) {
    uint8 ch = text[x];
    EmbeddedFont::Blit(
	(int)ch, x * CHAR_WIDTH, 0, SetPixel, ClearPixel);
  }
  
  out.Save("testembed.png");
  return 0;
}
