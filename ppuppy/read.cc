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

  if (argc <= 1) {
    fprintf(stderr, "Name some BCM pins on the command line to read from.\n");
    return -1;
  }

  bool pins[32] = {false};
  for (int arg = 1; arg < argc; arg++) {
    uint8 pin = atoi(argv[arg]);
    CHECK_GE(pin, 0) << argv[arg];
    CHECK_LT(pin, 32) << argv[arg];

    pins[pin] = true;
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
    printf("Set input %d.\n", pin);
  }

  for (;;) {
    uint32 inputs = bcm2835_gpio_lev_multi();
    string out = ".... .... .... .... .... .... .... ....\n";
    for (int i = 0; i < 32; i++) {
      int idx = 38 - (i + (i >> 2));
      if (pins[i]) {
	char value = (inputs & (1 << i)) ? '1' : '0';
	out[idx] = value;
      }
    }
    printf("%s", out.c_str());
    usleep(250 * 1000);
    Yield();
  }
    
  CHECK(bcm2835_close());
  return 0;
}
