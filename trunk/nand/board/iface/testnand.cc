// Tests the FPU NAND gate through the testing header.
// This requires the following connections:
//
// chip      testing header pin   BCM
//  PC9    x      13              22
//  PC10   x      12              27
//  PC11   x      11              17
//  PC12   y      10              18
//  PC13   y       9              15
//  PC14   y       8              14
//  PB13   z       6              6
//  PB14   z       5              26
//  PB15   z       4              16
//
// On the FPU board, pin 1 of the header is on the left
// (farthest from the pi header).

// BCM 11,25 are used for SWD, so don't touch these

// testing pin 7=PC15 is a strobe "chip awake" pin
// that we could also test.

// Expected output is like this:
//  pi@raspberrypi:~/tom7misc/nand/board/iface$ ./testnand.exe
//  I/O initialized.
//  000 000 -> 010
//  000 001 -> 010
//  000 010 -> 010
//  000 011 -> 010
//  001 000 -> 010
//  001 001 -> 010
//  001 010 -> 010
//  001 011 -> 010
//  010 000 -> 010
//  010 001 -> 010
//  010 010 -> 011   <- this is inf, inf -> nan
//  010 011 -> 010   <- everything else outputs inf
//  011 000 -> 010
//  011 001 -> 010
//  011 010 -> 010
//  011 011 -> 010


#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>

#include <string>
#include <cstdint>
#include <vector>

#include "bcm2835.h"
#include "base/logging.h"
#include "arcfour.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;

// Yield to OS (so that we can ctrl-c, process ethernet, etc.)
inline void Yield() {
  struct timespec t;
  t.tv_sec = 0;
  // 150 microseconds. Tune this?
  t.tv_nsec = 150 * 1000;
  nanosleep(&t, nullptr);
}

int main(int argc, char **argv) {
  CHECK(bcm2835_init());

  vector<int> outputs = {22, 27, 17, 18, 15, 14};
  vector<int> inputs = {6, 26, 16};

  for (int pin : inputs) {
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
  }

  for (int pin : outputs) {
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
  }
  
  printf("I/O initialized.\n");

  // with the low 3 bits of x,y encoding a binary3 number,
  // returning same.
  auto GetNand = [](uint8 x, uint8 y) -> uint8 {
    //  PC9    x      13              22
    //  PC10   x      12              27
    //  PC11   x      11              17
    //  PC12   y      10              18
    //  PC13   y       9              15
    //  PC14   y       8              14

    static constexpr uint32_t OUTPUT_MASK =
	(1 << 22) | (1 << 27) | (1 << 17) |
	(1 << 18) | (1 << 15) | (1 << 14);

#   define GET(z, b) ((uint32_t)(((z) >> (b)) & 1))
    // distribute the three bits of z to bit positions msb, mb, lsb.
#   define DISTRIBUTE(z, msb, mb, lsb) \
    ((GET(z, 2) << (msb)) | (GET(z, 1) << (mb)) | (GET(z, 0) << (lsb)))

    const uint32 value =
        DISTRIBUTE(x, 22, 27, 17) | DISTRIBUTE(y, 18, 15, 14);

    // Write x,y to the appropriate output pins.
    bcm2835_gpio_write_mask(value, OUTPUT_MASK);
    
    // Now delay a bit...
    {
      struct timespec t;
      t.tv_sec = 0;
      // 15000 usec = 15 ms
      t.tv_nsec = 15000 * 1000;
      nanosleep(&t, nullptr);
    }
    uint32 inputs = bcm2835_gpio_lev_multi();
    //  PB13   z       6              6
    //  PB14   z       5              26
    //  PB15   z       4              16
    uint8 z = (((inputs >> 6) & 1) << 2) |
	(((inputs >> 26) & 1) << 1) |
	(((inputs >> 16) & 1) << 0);
    return z;
  };

  auto Bits = [](uint8 v) -> string {
    string ret = "...";
    ret[0] = (v & 0b100) ? '1' : '0';
    ret[1] = (v & 0b010) ? '1' : '0';
    ret[2] = (v & 0b001) ? '1' : '0';
    return ret;
  };
  
  for (int x = 0; x < 4; x++) {
    for (int y = 0; y < 4; y++) {
      uint8 z = GetNand(x, y);
      printf("%s %s -> %s\n",
	     Bits(x).c_str(),
	     Bits(y).c_str(),
	     Bits(z).c_str());
    }
  }
  
  CHECK(bcm2835_close());
  return 0;
}
