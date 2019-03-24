// Interface from raspberry pi (the one soldered onto
// the pi header) to the META FPU.

#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>

#include <utility>
#include <tuple>
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

  // Low 18 bits all set.
  static constexpr uint32 OUTPUT_MASK = 0b111111111111111111;

  vector<int> outputs = {
    0, 1, 2,
    3, 4, 5,
    6, 7, 8,
    9, 10, 11,
    12, 13, 14,
    15, 16, 17,
  };
  vector<int> inputs = {18, 19, 20,
			21, 22, 23,
			24, 25, 26};

  for (int pin : inputs) {
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
  }

  for (int pin : outputs) {
    CHECK((1 << pin) & OUTPUT_MASK) << pin;
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
  }
  
  printf("I/O initialized.\n");

  // Write 6 binary3-coded floats to the board, and return
  // 3. Each of a-f encodes a single "bit" of a binary3 number
  // as either nan or inf, and so to does the result.
  auto GetInnerNand = [](uint8 a, uint8 b, uint8 c,
			 uint8 d, uint8 e, uint8 f) ->
    std::tuple<uint8, uint8, uint8> {
#   define GET(z, b) ((uint32_t)(((z) >> (b)) & 1))
    // distribute the three bits of z to bit positions msb, mb, lsb.
#   define DISTRIBUTE(z, msb, mb, lsb) \
    ((GET(z, 2) << (msb)) | (GET(z, 1) << (mb)) | (GET(z, 0) << (lsb)))

    const uint32 value =
	DISTRIBUTE(a, 2, 1, 0) |
	DISTRIBUTE(b, 5, 4, 3) |
	DISTRIBUTE(c, 8, 7, 6) |
	DISTRIBUTE(d, 11, 10, 9) |
	DISTRIBUTE(e, 14, 13, 12) |
	DISTRIBUTE(f, 17, 16, 15);
    
    // Write x,y to the appropriate output pins.
    bcm2835_gpio_write_mask(value, OUTPUT_MASK);
    
    // Now delay a bit...
    {
      struct timespec t;
      t.tv_sec = 0;
      // 150000 usec = 150 ms
      t.tv_nsec = 15000 * 1000;
      nanosleep(&t, nullptr);
    }
    const uint32 inputs = bcm2835_gpio_lev_multi();

#   define THREE(v, msb, mb, lsb) \
    ((GET(v, msb) << 2) | (GET(v, mb) << 1) | (GET(v, lsb)))

    const uint8 g = THREE(inputs, 20, 19, 18);
    const uint8 h = THREE(inputs, 23, 22, 21);
    const uint8 i = THREE(inputs, 26, 25, 24);
    return std::make_tuple(g, h, i);
  };
  
  auto Bits = [](uint8 v) -> string {
    string ret = "...";
    ret[0] = (v & 0b100) ? '1' : '0';
    ret[1] = (v & 0b010) ? '1' : '0';
    ret[2] = (v & 0b001) ? '1' : '0';
    return ret;
  };

  static constexpr uint8 NAN3 = 0b011;
  static constexpr uint8 INF3 = 0b010;
  
  uint8 g, h, i;
  std::tie(g, h, i) = GetInnerNand(NAN3, INF3, NAN3,
				   NAN3, INF3, NAN3);
  printf("Result: %s | %s | %s\n",
	 Bits(g).c_str(),
	 Bits(h).c_str(),
	 Bits(i).c_str());

#if 0
  for (int x = 0; x < 4; x++) {
    for (int y = 0; y < 4; y++) {
      uint8 z = GetNand(x, y);
      printf("%s %s -> %s\n",
	     Bits(x).c_str(),
	     Bits(y).c_str(),
	     Bits(z).c_str());
    }
  }
#endif
  
  CHECK(bcm2835_close());
  return 0;
}
