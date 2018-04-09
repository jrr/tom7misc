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
// #include "arcfour.h"

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

static constexpr bool WRITE_NAMETABLE = false;
static constexpr bool WRITE_ATTRIBUTE = false;

static constexpr bool TRY_RESYNC = true;

static constexpr int ON_TICKS = 8;

// Screen image data.

// Anyway. Right now we have just one bit per CHR output. So the
// resolution of the screen is 32 tiles (pixels) wide * 30 high.

// These are the values we return for the visible portion of the
// screen. There are 240 scanlines, each 32 tiles wide. On each
// scanline, each of those 32 tiles takes 4 reads to render.
// (At the end of the scanline there is some sprite and prefetch
// stuff.)
#define SCANLINES 240
#define TILESW 32
#define SPRITESW 8
#define PACKETSW (TILESW + SPRITESW)

// In the steady state, the PPU does four reads per 8-pixel
// strip (i.e., the slice from one scanline). Let's call this a packet:
//   A. character from nametable
//   B. attribute byte from attribute table
//   C. pixel bits for corresponding CHR (low)
//   D. pixel bits for corresponding CHR (high)
// Should we really store the tile index, or just return it
// programmatically?
#define PACKETBYTES 4
uint8 screen[SCANLINES * PACKETSW * PACKETBYTES] = {0};
uint8 GetByte(int idx) {
  // XXX should record how often this happens...
  if (idx >= SCANLINES * PACKETSW * PACKETBYTES) return 0;
  else return screen[idx];
}

// Yield to OS (so that we can ctrl-c, process ethernet, etc.)
inline void Yield() {
  struct timespec t;
  t.tv_sec = 0;
  // 150 microseconds. Tune this?
  t.tv_nsec = 150 * 1000;
  nanosleep(&t, nullptr);
}

// PERF! the screen should already be decoded, right?
inline uint32 Decode(uint8 byte) {
  uint32 bit = byte & 1;
  return (bit << POUT_A) | ((bit ^ 1) << POUT_B);
}

void InitImage() {
  // First just clear everything to zero.
  for (int i = 0; i < SCANLINES * PACKETSW * PACKETBYTES; i++) {
    screen[i] = 0;
  }

  // Palette 0 is fine for these tests.
  // (On the team select screen, 0 is like black,grey,red,orange)

  // But draw a circle in the CHR bytes.
  const int dsquared = 200 * 200;
  for (int y = 0; y < 240; y++) {
    for (int t = 0; t < 32; t++) {
      uint8 bits_lo = 0, bits_hi = 0;
      for (int b = 0; b < 8; b++) {
	bits_lo <<= 1;
	bits_hi <<= 1;

	int x = t * 8 + b;
	// x,y are pixel coordinates.
	// distance from center of screen
	int dx = (x - 16 * 8);
	int dy = (y - 120);

	// XXX fill whole screen for now
	if (true || dx * dx + dy * dy < dsquared) {
	  // TODO: Get some color info in here.
	  bits_lo |= 1;
	  bits_hi |= 1;
	}
      }
      int idx = (y * PACKETSW + t) * PACKETBYTES;
      screen[idx + 2] = bits_lo;
      screen[idx + 3] = bits_hi;
    }
  }
}

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

  if (geteuid() != 0) {
    fprintf(stderr, "Please run as root.\n");
    return -1;
  }

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

  InitImage();

  // Set input.
  for (uint8 p : {PIN_RD, PIN_ADDR0, PIN_ADDR1}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  for (uint8 p : {POUT_A, POUT_B}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    // XXX does pull-up/down even make sense for output? We should
    // always be driving the output line in this state.
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_DOWN);
  }

  printf("START.\n");

  // mask for output word.
  static constexpr uint32 MASK = (1 << POUT_A) | (1 << POUT_B);
  
  // We have to deal with serious latency constraints here. It's funny
  // how this makes the program regress into some kind of 6th-grader
  // style, but the goal is to have as few instructions between an
  // input edge that we care about and the output that it causes;
  // we're talking nanoseconds here.

  // About the labels in here:
  // They correspond to the steps A/B/C/D in a packet
      // With a word prepared (next_word), wait for the RD pin to
    // go high and then write our word. RD *low* is actually what
    // should be signaling us to write, but the high edge predicts
    // the low edge reliably, and eats up some useful latency.
    //
    // assumes (whether true or not) that RD is already low.

  // PERF: In these loops, only need a memory barrier after the
  // last read. And that memory barrier can be the same one that
  // happens before the first write!
  #define UNTIL_RD_HIGH \
    while (! ((inputs = bcm2835_gpio_lev_multi()) & (1 << PIN_RD))) {}

  // Wait until RD goes (or is) low. If RD stays high for too many
  // cycles, then assume we are in vblank and transition directly
  // to that state.
  #define UNTIL_RD_LOW	   \
    for (int hi_count = 0; \
	 (inputs = bcm2835_gpio_lev_multi()) & (1 << PIN_RD);	\
	 hi_count++) { \
      if (hi_count > 250) goto vblank; \
    }

  {
    // Number of times we entered vsync.
    int frames = 0;
    int min_desync = 999999, max_desync = -1;
    int min_scanlines = 999999, max_scanlines = -1;
    // Number of times we appeared to be desynchronized on
    // this frame.
    int desync = 0;
    // Number of scanlines we observed cleanly.
    int scanlines = 0;
    
    // This is the packet index in the screen array.
    int packetsync = 0;
      
    // This is the next word we'll write, as soon as the read cycle
    // starts. By prepping the next word in advance, we give ourselves
    // the minimal latency between the read and write.
    uint32 next_word = 0;
    // The last value read of the input lines.
    uint32 inputs = 0;

    // Number of consecutive nametable reads. At the end of a
    // scanline there are two nametable fetches (unpaired with
    // tile fetches), which we can use to sync the scanline.
    uint8 nt_reads = 0;

  a_phase:
    // CHECK_LT(packetsync * 4, (SCANLINES * PACKETSW * PACKETBYTES))
    // << "packetsync: " << packetsync
    // << " but size " << SCANLINES * PACKETSW * PACKETBYTES;

    // packetsync should indicate the correct packet.
    // Get the attribute bits word.
    next_word = Decode(GetByte(packetsync * 4 + 0));
    
    UNTIL_RD_HIGH;
    
    // Immediately write the prepared word.
    if (WRITE_NAMETABLE) bcm2835_gpio_write_mask(next_word, MASK);
    // XXX tune this. Also some possibility to do work here.
    delayTicks(ON_TICKS);
    bcm2835_gpio_clr_multi_nb(MASK);

    // Now check address bits. ADDR13 should be high because this
    // was a nametable read.
    if (inputs & (1 << PIN_ADDR13)) {
      // "VRAM" as expected.
      nt_reads++;
    } else {
      nt_reads = 0;
      desync++;
      // We must have been in phase C or D, so the next phase
      // is either D or A. Guess A since it's closer.
      // (Advance packet here?)
      if (TRY_RESYNC) goto a_phase;
    }
    
    // In case we were too fast, wait for RD to go low. (Necessary?)
    // (Note that this overwrites any address info we had.)
    UNTIL_RD_LOW;

  b_phase:
    // CHECK_LT(packetsync * 4, (SCANLINES * PACKETSW * PACKETBYTES))
    // << "packetsync: " << packetsync
    // << " but size " << SCANLINES * PACKETSW * PACKETBYTES;

    // Get the attribute bits word.
    next_word = Decode(GetByte(packetsync * 4 + 1));
    
    UNTIL_RD_HIGH;
    if (WRITE_ATTRIBUTE) bcm2835_gpio_write_mask(next_word, MASK);
    // XXX tune this. Also some possibility to do work here.
    delayTicks(ON_TICKS);
    bcm2835_gpio_clr_multi_nb(MASK);

    if (inputs & (1 << PIN_ADDR13)) {
      // "VRAM" as expected.
      nt_reads++;
    } else {
      nt_reads = 0;
      desync++;
      // We must have been in phase C or D, so the next phase
      // is either D or A. Guess D since it's closer.
      if (TRY_RESYNC) goto d_phase;
    }
    
    UNTIL_RD_LOW;

  c_phase:
    // Get the low pixels word.
    next_word = Decode(GetByte(packetsync * 4 + 2));

    UNTIL_RD_HIGH;
    bcm2835_gpio_write_mask(next_word, MASK);
    // XXX tune this. Also some possibility to do work here.
    delayTicks(ON_TICKS);
    bcm2835_gpio_clr_multi_nb(MASK);

    if (inputs & (1 << PIN_ADDR13)) {
      // "VRAM"
      // Best explanation for this is the extra reads at the
      // end of a scanline. We're not desynchronized if the
      // count looks right.
      nt_reads++;
      if (nt_reads != 3) {
	desync++;
	// We must have been in phase A or B, so next is B or C.
	// Guess C because it's closest.
	if (TRY_RESYNC) goto c_phase;
      }
    } else {
      // "ROM" as expected.
      nt_reads = 0;
      // XXX shift the address here so that we can check
      // for sync on the next byte too (should differ by 8).
    }
    
    UNTIL_RD_LOW;

  d_phase:
    // Get the high pixels word.
    next_word = Decode(GetByte(packetsync * 4 + 3));

    UNTIL_RD_HIGH;
    bcm2835_gpio_write_mask(next_word, MASK);
    // XXX tune this. Also some possibility to do work here.
    delayTicks(ON_TICKS);
    bcm2835_gpio_clr_multi_nb(MASK);

    UNTIL_RD_LOW;


    if (inputs & (1 << PIN_ADDR13)) {
      // "VRAM"
      // Best explanation for this is the extra reads at the
      // end of a scanline. We're not desynchronized if the
      // count looks right. But either way, we need to change
      // states.
      nt_reads++;
      if (nt_reads == 4) {
	// We read four consecutive nametable addresses. This
	// is because we just finished a scanline and then read
	// two more. Those count as the A and B phases to start
	// the scanline.
	scanlines++;
	// So the next phase is C. 
	goto c_phase;
      } else {
	desync++;
	// We must have been in phase A or B, so next is B or C.
	// Guess B because it's closest.
	if (TRY_RESYNC) {
	  packetsync++;
	  goto b_phase;
	}
      }
    } else {
      // "ROM" as expected.
      nt_reads = 0;
      // XXX can check address vs the saved one from phase C.
    }
    
    // next packet.
    packetsync++;
    goto a_phase;


  vblank:
    frames++;
    if (desync < min_desync) min_desync = desync;
    if (desync > max_desync) max_desync = desync;
    if (scanlines < min_scanlines) min_scanlines = scanlines;
    if (scanlines > max_scanlines) max_scanlines = scanlines;

    if (frames % 60 == 0) {
      printf("%d frames. %d packets, %d desync (%d--%d) %d sl (%d--%d)\n",
	     frames, packetsync,
	     desync, min_desync, max_desync,
	     scanlines, min_scanlines, max_scanlines);
      min_desync = 999999;
      max_desync = -1;
      min_scanlines = 999999;
      max_scanlines = -1;
    }
    desync = 0;
    scanlines = 0;

    // Yield to OS. For production, should perhaps disable this
    // so that nothing else is ever scheduled.
    Yield();
    // back to first packet.
    packetsync = 0;

    // Now wait until end of vblank.
    while ((inputs = bcm2835_gpio_lev_multi()) & (1 << PIN_RD)) {}
    goto a_phase;
  }

  CHECK(bcm2835_close());
  return 0;
}
