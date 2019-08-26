// Create a test pattern.

#include <cstdint>
#include <string>
#include <vector>

#include "util.h"
#include "image.h"
#include "bit7chars.h"
#include "bitmap-font.h"

using namespace std;
using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

constexpr int CHARS_ACROSS = 16;
constexpr int CHARS_DOWN = 8;

constexpr int CHAR_WIDTH = 9;
constexpr int CHAR_HEIGHT = 9;
using BF = BitmapFont<CHAR_WIDTH, CHAR_HEIGHT>;

constexpr int PX = 3;

int main(int argc, char **argv) {
  // XXX This stuff to some utilities, perhaps internal ones if the fonts
  // are shipped as headers?
  const string png = "test9x9.png";
  ImageRGBA *input = ImageRGBA::Load(png);
  CHECK(input) << png;
  vector<bool> bits;
  bits.resize(CHAR_WIDTH * CHAR_HEIGHT * CHARS_ACROSS * CHARS_DOWN);
  for (int cy = 0; cy < CHARS_DOWN; cy++) {
    for (int cx = 0; cx < CHARS_ACROSS; cx++) {
      const int c = CHARS_ACROSS * cy + cx;
      for (int y = 0; y < CHAR_HEIGHT; y++) {
	for (int x = 0; x < CHAR_WIDTH; x++) {
	  if (x < CHAR_WIDTH && y < CHAR_HEIGHT) {
	    int sx = cx * CHAR_WIDTH + x;
	    int sy = cy * CHAR_HEIGHT + y;
	    bool bit = (input->GetPixel(sx, sy) & 0x000000FF) > 0x0000007F;
	    bits[c * (CHAR_WIDTH * CHAR_HEIGHT) + y * CHAR_WIDTH + x] = bit;
	  }
	}
      }
    }
  }

  BF font{bits};

  vector<string> testpattern =
    {(string)B7_ES + string(46, *B7_EW) + (string)B7_SW,

     "  Welcome to my font!  it is cozy here " B7_SMILE "  (ok) ",
     "  Now is the FALL-TIME of our DISCONTENT !!!!!! ",
     "",
     "  " B7_FILL_6 B7_FILL_6 B7_FILL_6 
     B7_FILL_5 B7_FILL_5 B7_FILL_5 
     B7_FILL_4 B7_FILL_4 B7_FILL_4 
     B7_FILL_3 B7_FILL_3 B7_FILL_3 
     B7_FILL_2 B7_FILL_2 B7_FILL_2 
     B7_FILL_1 B7_FILL_1 B7_FILL_1 
     B7_FILL_0 B7_FILL_0 B7_FILL_0 "  LET'S", 
     "  " B7_FILL_6 B7_FILL_6 B7_FILL_6 
     B7_FILL_5 B7_FILL_5 B7_FILL_5 
     B7_FILL_4 B7_FILL_4 B7_FILL_4 
     B7_FILL_3 B7_FILL_3 B7_FILL_3 
     B7_FILL_2 B7_FILL_2 B7_FILL_2 
     B7_FILL_1 B7_FILL_1 B7_FILL_1 
     B7_FILL_0 B7_FILL_0 B7_FILL_0 "    GET ", 
     "  " B7_FILL_6 B7_FILL_6 B7_FILL_6 
     B7_FILL_5 B7_FILL_5 B7_FILL_5 
     B7_FILL_4 B7_FILL_4 B7_FILL_4 
     B7_FILL_3 B7_FILL_3 B7_FILL_3 
     B7_FILL_2 B7_FILL_2 B7_FILL_2 
     B7_FILL_1 B7_FILL_1 B7_FILL_1 
     B7_FILL_0 B7_FILL_0 B7_FILL_0 "      B-L-A-S-T-E-D !", 
     "",
     "  " B7_UNCHECKED " Enable hyper-drive      for (;;) {",
     "  " B7_CHECKED   " Enable ultra-disc         printf(\"hi?\\n\"); ",
     "  " B7_CHECKED   " Disable introspection   }",
     "",
     "  Dr. Jock, TV Quiz Ph.D., bags few lynx!  ",
     "  (glib jocks quiz nymph to vex dwarf) ",
     "  (SYMPATHIZING WOULD FIX QUAKER OBJECTIVES.) ",
     (string)B7_NE + string(46, *B7_EW) + (string)B7_NW,};

  const int LINES = testpattern.size();
  const int COLS = [&testpattern]() {
    int cols = 0;
    for (const string &line : testpattern)
      cols = std::max(cols, (int)line.size());
    return cols;
  }();

  CHECK(COLS > 0 && LINES >= 2);
  for (int i = 1; i < testpattern.size() - 1; i++) {
    string *line = &testpattern[i];
    *line = Util::Pad(COLS, std::move(*line));
    (*line)[0] = *B7_NS;
    (*line)[COLS - 1] = *B7_NS;
  }

  ImageRGBA out{COLS * CHAR_WIDTH * PX, LINES * CHAR_HEIGHT * PX};
  out.Clear(0, 0, 0, 0);
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
  
  for (int y = 0; y < LINES; y++) {
    const string &line = testpattern[y];
    for (int x = 0; x < line.size(); x++) {
      uint8 ch = line[x];
      // printf("%c", ch);
      font.Blit((int)ch, x * CHAR_WIDTH, y * CHAR_HEIGHT, SetPixel, ClearPixel);
    }
  }
  
  // XXX filename including dimensions, or from command line?
  out.Save("testpattern.png");

  delete input;
  return 0;
}
