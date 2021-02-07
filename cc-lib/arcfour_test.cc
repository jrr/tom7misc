#include "arcfour.h"

#include "base/logging.h"
#include <stdio.h>
#include <cstdint>
#include <vector>
#include <string>

using namespace std;
using uint8 = uint8_t;

// Check that the next bytes of the arcfour stream are as expected.
static void TestCase(const vector<uint8> &expected,
                     ArcFour *rc) {
  for (int i = 0; i < (int)expected.size(); i++) {
    uint8 got = rc->Byte();
    CHECK_EQ(got, expected[i]) << i;
  }
}

static void TestBasic() {
  {
    ArcFour rc("asdf");

    const vector<uint8> expected = {
      0x5c, 0x6f, 0xbf, 0xbe, 0x17, 0x82, 0x7e, 0x90,
      0x4e, 0x24, 0x19, 0x56, 0xc6, 0x32, 0xf5, 0x76,
    };
    TestCase(expected, &rc);
  }

  {
    const vector<uint8> init = {
      0xde, 0xfa, 0xce, 0xd0, 0x0d, 0x12, 0x34, 0x56, 0x78, 0x9a,
    };
    const vector<uint8> expected = {
      0x3d, 0xe2, 0xba, 0x04, 0x63, 0x61, 0x78, 0xad,
      0x60, 0xf5, 0x4c, 0x80, 0x25, 0xa3, 0x12, 0x83,
      0x8c, 0x1e, 0x81, 0xd9, 0x03, 0x1e, 0x4b, 0x99,
      0x13, 0x81, 0x80, 0x75, 0x51, 0x7b, 0x1b, 0xa2,
    };
    ArcFour rc(init);
    
    TestCase(expected, &rc);
  }
}

int main(int argc, char **argv) {
  TestBasic();
  printf("OK\n");
  return 0;
}

