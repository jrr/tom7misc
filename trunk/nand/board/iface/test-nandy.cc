
#include "nandy.h"

#include <cstdint>
#include <vector>
#include <tuple>
#include <deque>
#include <functional>

#include "arcfour.h"
#include "image.h"
#include "md5.h"
#include "base/stringprintf.h"

#define VERBOSE if (0)

using namespace std;
using uint8 = uint8_t;
using uint64 = uint64_t;

#define SAVE_IMAGE 0

static void TestNandy() {
  ArcFour rc{"nandy"};
  auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
  Nandy nandy;
  for (int i = 0; i < Nandy::MEM_SIZE; i++) {
    nandy.MEM[i] = Rand3();
  }
  
  static constexpr int NUM_STEPS = 2048;
  const int width = nandy.GetState().size();
# if SAVE_IMAGE
  const int height = NUM_STEPS;
  ImageRGBA image(width, height);
# endif
  
  string trace = "start";
  trace.reserve(48 + width);
  for (int y = 0; y < NUM_STEPS; y++) {
    nandy.Step();
    vector<Binary3> row = nandy.GetState();
    for (int x = 0; x < row.size(); x++) {
      uint8 rgb = row[x].Bits();
#     if SAVE_IMAGE
      image.SetPixel(x, y,
		     rgb & 0b100 ? 255 : 0,
		     rgb & 0b010 ? 255 : 0,
		     rgb & 0b001 ? 255 : 0,
		     255);
#     endif
      trace.push_back('0' + rgb);
    }
    trace = MD5::Ascii(MD5::Hash(trace));
  }

# if SAVE_IMAGE
  image.Save("nandy-trace.png");
# endif

  printf("Trace hash: %s\n", trace.c_str());
  CHECK_EQ(trace, "6dcd0f2d4041009b704ce11d393dce24");
}

void Test2Nandy() {
  Nandy::Nandwork work = Nandy::MakeNandwork();

  // Always with the same identical state.
  auto Initialize = [](Nandy *nandy) {
      ArcFour rc{"nandy"};
      auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
      for (int i = 0; i < Nandy::MEM_SIZE; i++) {
	nandy->MEM[i] = Rand3();
      }
    };
  
  Nandy nandy, nandy_nand;
  Initialize(&nandy);
  Initialize(&nandy_nand);

  printf("\n\nTest 2 in parallel:\n");
  
  /*
  CHECK_EQ(nandy.GetStateString(),
	   nandy_nand.GetStateString());
  */
  
  for (int i = 0; i < 10; i++) {
    string s1 = nandy.GetStateString();
    string s2 = nandy_nand.GetStateString();
    printf("%s <- step\n%s <- step_nand\n",
	   s1.c_str(), s2.c_str());
    CHECK_EQ(s1, s2);

    nandy.Step();
    nandy_nand.StepNand(work);
  }
}


int main(int argc, char **argv) {
  TestNandy();
  
  (void)TestNandy;
  (void)Test2Nandy;
  // Nandy nandy;
  // nandy.Step();

  #if 0
  Nandy::Binary3D ip = 123;
  printf("Getbit: ");
  for (int i = 0; i < 3 * Nandy::D; i++) {
    printf("%c", Nandy::Binary3DGetBit(ip, i) ? '1' : '0');
  }
  printf("\n");
  #endif
  
  // Nandy::MakeNandwork();
  
  Test2Nandy();
  
  return 0;
}
