
#include "nandy.h"

#include <cstdint>
#include <vector>
#include <tuple>
#include <deque>
#include <functional>

#include "arcfour.h"
#include "image.h"
#include "crypt/md5.h"
#include "base/stringprintf.h"

#define VERBOSE if (0)

using namespace std;
using uint8 = uint8_t;
using uint64 = uint64_t;

#define SAVE_IMAGE 0

static void BenchNandy() {
  ArcFour rc{"nandy"};
  auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
  Nandy nandy;
  for (int i = 0; i < Nandy::MEM_SIZE; i++) {
    nandy.MEM[i] = Rand3();
  }

  int64 start = time(nullptr);
  static constexpr int NUM_STEPS = 100000000;
  for (int y = 0; y < NUM_STEPS; y++) {
    nandy.Step();
  }

  int64 elapsed = time(nullptr) - start;
  printf("%d steps in %lld = %.2f/s\n",
	 NUM_STEPS, elapsed,
	 (double)NUM_STEPS / (double)elapsed);

  // Make sure result is used.
  vector<Binary3> row = nandy.GetState();
  string trace;
  for (int x = 0; x < row.size(); x++) {
    uint8 rgb = row[x].Bits();
    trace.push_back('0' + rgb);
  }
  printf("Final: %s\n",
	 MD5::Ascii(MD5::Hash(trace)).c_str());
}

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

static string StateString(const Nandy &nandy) {
  string ret;

  string hashsrc;
  hashsrc.reserve(nandy.MEM.size());
  for (Binary3 b : nandy.MEM)
    hashsrc.push_back('0' + b.Bits());
  string mem = MD5::Ascii(MD5::Hash(hashsrc));
  StringAppendF(&ret, "%d/%d/%d:%s|%s|%s|%s:(%s)",
		nandy.IP, nandy.ADDR, nandy.ADDR_COUNT,
		nandy.Z.ToString().c_str(),
		nandy.A.ToString().c_str(),
		nandy.B.ToString().c_str(),
		nandy.C.ToString().c_str(),
		mem.c_str());
  return ret;
}

void Test2Nandy() {
  Nandy::Nandwork work = Nandy::MakeNandwork();
  printf("\n\nTest 2 in parallel:\n");

  // Initialize to some random setup. 
  auto Initialize = [](Nandy *nandy, int i) {
    ArcFour rc{StringPrintf("nandy%d", i)};
    auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
    for (int i = 0; i < Nandy::MEM_SIZE; i++) {
      nandy->MEM[i] = Rand3();
    }
  };

  for (int count = 0; count < 100; count++) {
    Nandy nandy, nandy_nand;
    Initialize(&nandy, count);
    Initialize(&nandy_nand, count);
    
    CHECK_EQ(StateString(nandy),
	     StateString(nandy_nand));
  
    for (int i = 0; i < 1000; i++) {
      string s1 = StateString(nandy);
      string s2 = StateString(nandy_nand);
      /*
	printf("%s <- step\n%s <- step_nand\n",
	s1.c_str(), s2.c_str());
      */
      CHECK_EQ(s1, s2) << s1 << "\nvs\n" << s2;

      nandy.Step();
      nandy_nand.StepNand(work);
    }
    printf("%d ok\n", count);
  }
}


int main(int argc, char **argv) {
  (void)BenchNandy;
    
  // TestNandy();
  
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
