#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

#include <string>
#include <cstdint>

#include "bcm2835.h"
// #include "util.h"
#include "base/logging.h"
#include "arcfour.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using uint64 = uint64_t;
using int32 = int32_t;
using int64 = int64_t;

// input pins
static constexpr uint8 PIN_RD = 16;
static constexpr uint8 PIN_ADDR0 = 25;
static constexpr uint8 PIN_ADDR1 = 24;
static constexpr uint8 PIN_ADDR13 = 23;

// output pins
// static constexpr uint8 POUT = 26;
static constexpr uint8 POUT_A = 5;
static constexpr uint8 POUT_B = 6;
// static constexpr uint8 POUT4 = 22;


// Maximum resolution timer; a spin-loop purely on the CPU.
// Intended for a small number of ticks.
// Disabled inlining for predictability, although this adds
// some overhead.
// Note that power scaling and stuff could cause the instructions
// to take an unpredictable amount of time.
void __attribute__ ((noinline)) delayTicks(volatile uint32_t ticks) {
  asm volatile("@ delayTicks inline start" : : :"memory");
  volatile int f = 0;
  while (ticks--) {
    f++;
  }
  asm volatile("@ delayTicks inline end" : : :"memory");
}


int main(int argc, char **argv) {

#if 1
  // This magic locks our memory so that it doesn't get
  // swapped out, which improves our scheduling latency.
  struct sched_param sp;
  memset(&sp, 0, sizeof(sp));
  sp.sched_priority = sched_get_priority_max(SCHED_FIFO);
  if (0 != sched_setscheduler(0, SCHED_FIFO, &sp)) {
    printf("Failed to set scheduling priority...\n");
    perror("setup: ");
  }
  mlockall(MCL_CURRENT | MCL_FUTURE);
  printf("NOLOCK.\n");
#endif

  CHECK(bcm2835_init());

  ArcFour rc("ppuppy");
  uint8 mem[8192] = {};
  for (int i = 0; i < 8192; i++) {
    mem[i] = rc.Byte();
  }
  
  printf("START.\n");

  // Set input. Pulldown doesn't make sense for input pins, right?
  for (uint8 p : {PIN_RD, PIN_ADDR0, PIN_ADDR1}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  // For the tri-state experiment, pulldown makes sense.
  for (uint8 p : {POUT_A, POUT_B}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_DOWN);
  }

  enum class State {
    UNKNOWN,
    IN_VBLANK,
    RENDERING,  
  };

  State state = State::UNKNOWN;

  #if 0
  // slow toggle -- looks good!
  uint8 bit = 0;
  for(;;) {
    bcm2835_delayMicroseconds(500000);
    bit = bit ^ 1;
    bcm2835_gpio_write_mask(
	(bit << POUT_A) | (~bit << POUT_B),
	(1 << POUT_A) | (1 << POUT_B));
  }
  return 0;
  #endif
  
  // falling edge on PPU RD.
  int64 edges = 0LL;
  int64 reads[256] = {};
  int32 num_hi = 0, frames = 0, sync = 0;

  // last value of PPU /RD
  uint8 rd_last = 0;
  uint32 bit = 1;
  for (;;) {
    uint32_t inputs = bcm2835_gpio_lev_multi();
    if (inputs & (1 << PIN_RD)) {
      // PPU /RD is high (so not reading)
      if (!rd_last) {
	// rising edge.

	// Is this a read from CHR ROM or CIRAM?
	if (!(inputs & (1 << PIN_ADDR13))) {
	  // static constexpr uint32 SET_HIGH = (1 << POUT_A) | (0 << POUT_B);
	  static constexpr uint32 MASK = (1 << POUT_A) | (1 << POUT_B);
	  // Delay here?
	  
	  // The logic for the tri-state output is:
	  //
	  //    A B out
	  //    0 0  Z   (high-impedance; "disconnected")
	  //    1 0  1   (supply current)
	  //    0 1  0   (sink current)
	  //    1 1  no!
	  //
	  // So to drive a logic level, send the bit to A and ~bit to
	  // B.
	  bcm2835_gpio_write_mask(
	      (bit << POUT_A) | ((bit ^ 1) << POUT_B),
	      MASK);

	  // XXX HAX. Need to tune this timing and maybe dynamically
	  // adjust it.
	  delayTicks(4);

	  // Now reset the values soon after.
	  // We need to drive the bus long enough for the PPU to read.
	  // No idea how long is correct!
	  // PERF just do a fast 'clear'.
	  bcm2835_gpio_write_mask(
	      // Clear both, disconnecting from bus.
	      0,
	      MASK);

	} else {
	  // Read from 0x0000-0x1FFF (CIRAM).
	  // In this case we don't want to output.
	  // We assume that we're still in high-impedance mode.
	}

      }

      // Have we been in this state long enough to
      // recognize vblank?
      num_hi++;
      if (num_hi > 100) {
	if (state != State::IN_VBLANK) {
	  // yield to OS so we can ctrl-c at least.
	  // PPU vblank is 1.334072ms.
	  if ( frames % 60 == 0) {
	    bit ^= 1;
	    printf("%lld edge, %d frames, %d last sync, %lld %lld %lld %lld.\n",
		   edges, frames, sync, reads[0], reads[1], reads[2], reads[3]);
	  }
	  state = State::IN_VBLANK;
	  
	  bcm2835_delayMicroseconds(500); // half millisecond
	}
      }
      rd_last = 1;
    } else {
      // rd is low (reading)
      if (rd_last == 1) {
	// Did we just begin a frame?
	if (state == State::IN_VBLANK) {
	  sync = 0;
	  frames++;
	  state = State::RENDERING;
	}

	sync++;

	// Falling edge.
	edges++;
      }
      num_hi = 0;
      rd_last = 0;
    }
  }

  return 0;

  // return SlowStrobe();
  // return FastStrobe();

  CHECK(bcm2835_close());
  return 0;
}
