#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>

#include <string>
#include <cstdint>

#include "bcm2835.h"
#include "base/logging.h"
#include "arcfour.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;

int main(int argc, char **argv) {
  CHECK(bcm2835_init());
  uint8 pin = 26;
  // XXX from command line

  bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
  bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);

  uint8 value = 0;
  bcm2835_gpio_write_mask((value << PIN), (1 << PIN));
  
  CHECK(bcm2835_close());
  return 0;
}
