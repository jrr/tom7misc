#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>

#include <string>
#include <cstdint>

#include "bcm2835.h"
#include "base/logging.h"
#include "arcfour.h"
#include "ppuppy.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;

int main(int argc, char **argv) {
  CHECK(bcm2835_init());

  if (argc <= 1) {
    fprintf(stderr, "Give an 8-bit number to set the data port to.\n");
    return -1;
  }

  uint8 data = atoi(argv[1]);

  static constexpr uint32 OUTPUT_MASK =
    (1 << POUT_D0) | (1 << POUT_D1) |
    (1 << POUT_D2) | (1 << POUT_D3) |
    (1 << POUT_D4) | (1 << POUT_D5) |
    (1 << POUT_D6) | (1 << POUT_D7);
  
  for (uint8 p : {POUT_D0, POUT_D1, POUT_D2, POUT_D3,
	POUT_D4, POUT_D5, POUT_D6, POUT_D7}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  #if 0
  for (int i = 0; i < 256; i++) {
    uint8 data = i & 255;
    uint8 revdata = data;
    revdata = (revdata & 0xF0) >> 4 | (revdata & 0x0F) << 4;
    revdata = (revdata & 0xCC) >> 2 | (revdata & 0x33) << 2;
    revdata = (revdata & 0xAA) >> 1 | (revdata & 0x55) << 1;
    if (i % 16 == 0) printf ("\n");
    printf("%02x, ", revdata);
  }
  #endif
  
  // for (int i = 0; i < 256; i++) {
  // uint8 data = i & 255;
    bcm2835_gpio_write_mask(data << 2, OUTPUT_MASK);
    printf("Set to 0x%02x.\n", data);
    // sleep(1);
    // }
  
  CHECK(bcm2835_close());
  return 0;
}
