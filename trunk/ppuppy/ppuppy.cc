#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>

#include <string>
#include <cstdint>

#include "bcm2835.h"
// #include "util.h"
#include "base/logging.h"
#include "arcfour.h"
// #include "base/stringprintf.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using int64 = uint64_t;

// input pins
static constexpr uint8 PIN_RD = 16;

// output pins
static constexpr uint8 POUT = 26;
static constexpr uint8 POUT2 = 6;
static constexpr uint8 POUT3 = 5;
static constexpr uint8 POUT4 = 22;

int main(int argc, char **argv) {

  struct sched_param sp;
  memset(&sp, 0, sizeof(sp));
  sp.sched_priority = sched_get_priority_max(SCHED_FIFO);
  sched_setscheduler(0, SCHED_FIFO, &sp);
  mlockall(MCL_CURRENT | MCL_FUTURE);
  printf("NOLOCK.\n");
  
  CHECK(bcm2835_init());

  ArcFour rc("ppuppy");
  uint8 mem[8192] = {};
  for (int i = 0; i < 8192; i++) {
    mem[i] = rc.Byte();
  }
  
  printf("START.\n");

  // Set input and enable pulldown.
  for (uint8 p : {PIN_RD}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_DOWN);
  }

  // Set output and disable pulldown.
  for (uint8 p : {POUT, POUT2, POUT3, POUT4}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  // return SlowStrobe();
  // return FastStrobe();

  // Need to delay every few ms or it's impossible to kill the
  // process. (XXX figure out a better way to do this though!)

  int64 rd_edge = 0LL;
  for (;;) {
    bcm2835_delayMicroseconds(16000); // 16ms, about one 60Hz frame
    uint8 last = 0;
    for (int i = 0; i < 0x0FFFFF; i++) {
      // XXX need to implement lev_multi; how does that not exist?!
      if (bcm2835_gpio_lev(PIN_RD)) {
	// Went high.
	if (!last) last = 1;
      } else {
	// Went low.
	if (last) {
	  rd_edge++;
	  last = 0;
	}
      }
    }
    printf("%lld .\n" , rd_edge);
  }
  
  CHECK(bcm2835_close());
  return 0;
}

int SlowStrobe() {
  for (int i = 0; i < 20; i++) {
    uint8 v = i & 1;
    printf("%s\n", v ? "ON" : "OFF");
    bcm2835_gpio_write_mask((v << POUT), (1 << POUT));
    sleep(3);
  }

  return 0;
}

int FastStrobe() {
  ArcFour rc("ppuppy");
  uint8 mem[8192] = {};
  for (int i = 0; i < 8192; i++) {
    mem[i] = rc.Byte();
  }

  int index = 0;
  uint32 value = 0;
  for (;;) {
    // bcm2835_delayMicroseconds(1000); // 1ms
    // bcm2835_gpio_write(POUT, value & 1);

    uint8 v = (value >> 14) & 1;
    bcm2835_gpio_write_mask(
	(v << POUT) | (v << POUT2) | (v << POUT3) | (v << POUT4),
	(1 << POUT) | (1 << POUT2) | (1 << POUT3) | (1 << POUT4));
    value++;

    if (0 == (value & 0x3FFFF)) {
      bcm2835_delayMicroseconds(16000); // 16ms
    }
    
    #if 0
    // uint8 value = mem[index];
    uint32 word = ((value & 1) << POUT) |
      (((value >> 1) & 1) << POUT2) |
      (((value >> 2) & 1) << POUT3) |
      (((value >> 3) & 1) << POUT4);
    bcm2835_gpio_write_mask(word,
			    (1 << POUT) | (1 << POUT2) | (1 << POUT3) | (1 << POUT4));
    #endif
    // index++;
    // index &= 8191;
  }
  return 0;
}
