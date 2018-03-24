#include <sched.h>
#include <sys/mman.h>

#include <string>
#include <cstdint>

#include "bcm2835.h"
#include "util.h"
#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;
using uint8 = uint8_t;

int main(int argc, char **argv) {

  struct sched_param sp;
  memset(&sp, 0, sizeof(sp));
  sp.sched_priority = sched_get_priority_max(SCHED_FIFO);
  sched_setscheduler(0, SCHED_FIFO, &sp);
  mlockall(MCL_CURRENT | MCL_FUTURE);
  printf("NOLOCK.\n");
  
  CHECK(bcm2835_init());

  printf("START.\n");
  static constexpr uint8 PIN = 26;
  bcm2835_gpio_fsel(PIN, BCM2835_GPIO_FSEL_OUTP);
  bcm2835_gpio_set_pud(PIN, BCM2835_GPIO_PUD_OFF);

  uint8 value = 0;
  for (;;) {
    // bcm2835_delayMicroseconds(1000); // 1ms
    bcm2835_gpio_write(PIN, value & 1);
    value++;
  }
  
  CHECK(bcm2835_close());
  return 0;
}
