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
using uint32 = uint32_t;

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
  static constexpr uint8 PIN2 = 6;
  static constexpr uint8 PIN3 = 5;
  static constexpr uint8 PIN4 = 22;
  // Set output and disable pulldown
  for (uint8 p : {PIN, PIN2, PIN3, PIN4}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  uint8 value = 0;
  for (;;) {
    // bcm2835_delayMicroseconds(1000); // 1ms
    // bcm2835_gpio_write(PIN, value & 1);
    /*
    uint8 v = value & 1;
    bcm2835_gpio_write_mask(
	(v << PIN) | (v << PIN2) | (v << PIN3) | (v << PIN4),
	(1 << PIN) | (1 << PIN2) | (1 << PIN3) | (1 << PIN4));
    */
    uint32 word = ((value & 1) << PIN) |
      (((value >> 1) & 1) << PIN2) |
      (((value >> 2) & 1) << PIN3) |
      (((value >> 3) & 1) << PIN4);
    bcm2835_gpio_write_mask(word,
			    (1 << PIN) | (1 << PIN2) | (1 << PIN3) | (1 << PIN4));
    
    value++;
  }
  
  CHECK(bcm2835_close());
  return 0;
}
