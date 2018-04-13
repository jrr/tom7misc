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

  if (argc <= 1) {
    fprintf(stderr, "Name some BCM pins on the command line to clear.\n");
    return -1;
  }

  for (int arg = 1; arg < argc; arg++) {
    uint8 pin = atoi(argv[arg]);
    CHECK_GE(pin, 0) << argv[arg];
    CHECK_LT(pin, 32) << argv[arg];

    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);

    uint8 value = 0;
    bcm2835_gpio_write_mask((value << pin), (1 << pin));
    printf("Clear pin %d.\n", pin);
  }
  
  CHECK(bcm2835_close());
  return 0;
}
