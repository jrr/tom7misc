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

  if (argc != 2) {
    fprintf(stderr, "Name one BCM pin on the command line to read from.\n");
    return -1;
  }

  uint8 pin = atoi(argv[1]);
  CHECK_GE(pin, 0) << argv[1];
  CHECK_LT(pin, 32) << argv[1];

  bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_INPT);
  bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);

  uint32 inputs = bcm2835_gpio_lev_multi();

  bool ret = !!(inputs & (1 << pin));
  CHECK(bcm2835_close());
  return ret;
}
