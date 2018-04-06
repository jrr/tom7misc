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
static constexpr uint8 POUT = 26;
static constexpr uint8 POUT2 = 6;
static constexpr uint8 POUT3 = 5;
static constexpr uint8 POUT4 = 22;

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

  // Set input and enable pulldown.
  for (uint8 p : {PIN_RD, PIN_ADDR0, PIN_ADDR1}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF); // XXX set to no pulls
  }

  // Set output and disable pulldown.
  for (uint8 p : {POUT, POUT2, POUT3, POUT4}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  // Memory actually returned for each read.
  // XXX extend to 16-bit (actually 13) addresses obv
  uint8 values[256];
  for (int i = 0; i < 256; i++) {
    values[i] = i >> 4;
  }

  enum class State {
    UNKNOWN,
    IN_VBLANK,
    RENDERING,  
  };

  State state = State::UNKNOWN;
  
  // falling edge on PPU RD.
  int64 edges = 0LL;
  int64 reads[256] = {};
  int32 num_hi = 0, frames = 0, sync = 0;

  // last value of PPU /RD
  uint8 rd_last = 0;
  for (;;) {
    uint32_t inputs = bcm2835_gpio_lev_multi();
    if (inputs & (1 << PIN_RD)) {
      // PPU /RD is high (so not reading)
      if (!rd_last) {
	// Rising edge. Stop outputting during this interval, so
	// that the data bus isn't still full when the next read
	// starts.
	// PERF: This can be just set_multi or clr_multi, I think.
	bcm2835_gpio_write_mask(
	    // We actually write 3v3 0 here, which is inverted to a
	    // 5v 1, which is then the unit on the bus conflict
	    // (which is basically AND).
	    0,
	    (1 << POUT) | (1 << POUT2) | (1 << POUT3) | (1 << POUT4));
      }

      // Have we been in this state long enough to
      // recognize vblank?
      num_hi++;
      if (num_hi > 100) {
	if (state != State::IN_VBLANK) {
	  // yield to OS so we can ctrl-c at least.
	  // PPU vblank is 1.334072ms.
	  if ( frames % 60 == 0) {
	    printf("%lld edge, %d frames, %d last sync, %lld %lld %lld %lld.\n",
		   edges, frames, sync, reads[0], reads[1], reads[2], reads[3]);
	  }
	  state = State::IN_VBLANK;
	  
	  bcm2835_delayMicroseconds(500); // half a millisecond
	}
      }
      rd_last = 1;
    } else {
      // rd is low (reading)
      if (rd_last == 1) {
	// Falling edge.
	edges++;
	// Did we just begin a frame?
	if (state == State::IN_VBLANK) {
	  sync = 0;
	  frames++;
	  state = State::RENDERING;
	}

	// Is this a read from CHR ROM or CIRAM?
	if (inputs & (1 << PIN_ADDR13)) {
	  // Read from 0x2000-0x3FFF (ROM).
	  uint8 addr =
	    (((inputs >> PIN_ADDR0) & 1) << 0) |
	    (((inputs >> PIN_ADDR1) & 1) << 1);
	  // obviously get more bits...
	  reads[addr]++;

	  /*
	    uint8 data = ((sync > 4000 && sync < 12000) ||
	    (sync > 24000 && sync < 26000))
	    ? 0xFF : 0x00;
	  */
	  /*
	  uint8 data = (sync & 1) ? 255 : 0;
	
	  uint32 word = ((data & 1) << POUT) |
	    (((data >> 1) & 1) << POUT2) |
	    (((data >> 2) & 1) << POUT3) |
	    (((data >> 3) & 1) << POUT4);
	  */
	  uint32 word = ((sync > 4000 && sync < 12000) ||
	    (sync > 24000 && sync < 26000))
	    ? 0xFFFFFFFF : 0x00;
	  bcm2835_gpio_write_mask(
	      // Note: All writes use transistor for level shifting, so
	      // are inverted.
	      ~word,
	      (1 << POUT) | (1 << POUT2) | (1 << POUT3) | (1 << POUT4));
	  
	} else {
	  // Read from 0x0000-0x1FFF (CIRAM).
	  // In this case we don't want to output.
	  // But outputting "0" is not the same as
	  // "let the other chip assert its value."
	  // But allegedly, bus conflicts resolve
	  // to zero, so the fact that we pull up
	  // this bit (currently 4.7k) on the 5V side
	  // may be just what we need, as long as
	  // we write 0 (= 5v logic level 1) here.
	  //
	  // Since we already wrote this on the rising
	  // edge, there's nothing to do.
	}

	sync++;
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
    value++;
    #endif
    index++;
    index &= 8191;
  }
  return 0;
}
