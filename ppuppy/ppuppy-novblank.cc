#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

#include <string>
#include <vector>
#include <cstdint>

#include "bcm2835.h"
#include "base/logging.h"

#include "ppuppy.h"
#include "screen.h"
#include "demos.h"
#include "convert.h"

// Gives good timing, but requires a hard restart to
// get back to linux.
static constexpr bool DISABLE_INTERRUPTS = true;

BouncingBalls bouncing;

inline uint32 GetEncodedByte(Screen *screen, int scanline, int col, int b) {
  static constexpr int WORDS = NUM_SCANLINES * NUM_COLS;
  const int idx = scanline * NUM_COLS + col;
  if (idx >= WORDS) return 0;
  // PERF could skip this by just zero padding.
  if (!b) return 0;
  uint8 *addr = ((uint8*)screen) + (WORDS * (b - 1)) + idx;
  return ((uint32)*addr) << 2;
}

// Yield to OS (so that we can ctrl-c, process ethernet, etc.)
inline void Yield() {
  if (!DISABLE_INTERRUPTS) {
    struct timespec t;
    t.tv_sec = 0;
    // 150 microseconds. Tune this?
    t.tv_nsec = 150 * 1000;
    nanosleep(&t, nullptr);
  }
}

// mask for output word.
static constexpr uint32 OUTPUT_MASK =
  (1 << POUT_D0) | (1 << POUT_D1) |
  (1 << POUT_D2) | (1 << POUT_D3) |
  (1 << POUT_D4) | (1 << POUT_D5) |
  (1 << POUT_D6) | (1 << POUT_D7);

// Decode the low 10 bits of the address.
inline uint16 DecodeAddress(uint32 inputs) {
  static_assert(PIN_A0 == 14, "hard-coded for speed");
  // ... check these too ...
  static_assert(PIN_A9 == 23, "hard-coded for speed");
  // TODO: Also include A13 here? It's in the right position now.
  static constexpr uint16 ADDR_MASK =
    // 10 bits
    ((1 << 10) - 1);
  return (inputs >> PIN_A0) & ADDR_MASK;
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

  if (DISABLE_INTERRUPTS) {
    if (argc != 2 ||
	0 != strcmp(argv[1], "noint")) {
      fprintf(stderr, "Add 'noint' to the commandline to "
	      "confirm DISABLE_INTERRUPTS. sync first!\n");
      return -1;
    }
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

  for (uint8 p : { PIN_RD, PIN_A0, PIN_A1, PIN_A2, PIN_A3,
	PIN_A4, PIN_A5, PIN_A6, PIN_A7, PIN_A8, PIN_A9,
	PIN_A13 }) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  for (uint8 p : {POUT_D0, POUT_D1, POUT_D2, POUT_D3,
	POUT_D4, POUT_D5, POUT_D6, POUT_D7}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  printf("LOAD.\n");
  Slideshow slideshow(vector<string>{
        "images/titletest.png",
	"images/self.jpg",
	"images/flower.png",
	"images/robot.png",
	"images/ppuppy-logo.png"});
  
  printf("START.\n");
  fflush(stdout);
  
  // Shift the raw input history. Read a new raw input, then compute the
  // new deglitched inputs. The deglitched values are computed by voting.
  // (TODO: Consider reading until we have consecutive successes, or so?)
  //
  // Voting is nominally (a & b) | (b & c) | (c & a).
  // This can be improved to: (a & (b | c)) | (b & c)
  // However, note that when we perform this multiple times, the former
  // computes expressions that can be reused. So we prefer that version.
  // Here we explicitly reuse the value. PERF: Experiment with different
  // ways of doing this; compiler might be smarter than us.
  #define DEGLITCH_READ \
    inputs_2 = inputs_1;	 \
    inputs_1 = inputs_0; \
    inputs_0 = bcm2835_gpio_lev_multi_nb(); \
    inputs_0and1 = inputs_1and2;			\
    inputs_1and2 = inputs_1 & inputs_2; \
    inputs = inputs_0and1 | inputs_1and2 | (inputs_0 & inputs_2);

  #define UNTIL_RD_HIGH \
    do { DEGLITCH_READ } while (! (inputs & (1 << PIN_RD)))
  
  // Wait until RD goes (or is) low. If RD stays high for too many
  // cycles, then assume we are in vblank and transition directly
  // to that state.
  #define UNTIL_RD_LOW	   \
    for (int hi_count = 0; \
         /* in loop */ ; \
	 hi_count++) { \
      DEGLITCH_READ;				\
      if (! (inputs & (1 << PIN_RD))) break; \
      if (hi_count > 250) goto vblank;		\
    }

  if (DISABLE_INTERRUPTS) {
    bcm2835_int_disable_all();
  }
  
  {
    // Number of times we entered vsync.
    int frames = 0;

    // Number of times we appeared to be desynchronized on
    // this frame.
    int desync = 0;
    int min_desync = 0, max_desync = 0;
    int complete_packets = 0;
    
    // This is the y tile index from 0 to 29.
    // Determined from the nametable read.
    // Scanline from 0-240 (although PPU does read outside this range).
    // The low three bits are determined from the row of the tile bitmap.
    // The upper bits are determined from the nametable read.
    int scanline = 0;
    // Tile column from 0 to 31. Determined from the nametable read.
    int col = 0;
    
    // Value we'll write, encoded for the pins.
    uint32 output_word = 0;
    // The last value read of the input lines.
    uint32 inputs = 0;
    // The previous three reads, without deglitching. 0 is the most
    // recent, 1 before that, 2 before THAT. These are used in the
    // deglitching code. Everything else should just trust "inputs".
    uint32 inputs_0 = 0, inputs_1 = 0, inputs_2 = 0;
    // As we shift inputs back, we can also shift these expressions
    // to reuse them. (0and2 is not reusable.)
    uint32 inputs_0and1 = 0, inputs_1and2 = 0;
    // Decoded address read from input.
    uint16 addr = 0;

    // What byte of the packet are we in? 0-3.
    int packetbyte = 0;

    Screen *screen = nullptr;
    
  next_frame:
    // In the steady state, this needs to complete during vsync.

    // bouncing.Draw();
    screen = slideshow.GetScreen();
    
    // Assume we are at the top-left.
    col = 0;
    scanline = 0;
    packetbyte = 0;
    
    // Now wait until end of vblank.
    do {
      DEGLITCH_READ;
    } while (inputs & (1 << PIN_RD));
    
  next_cycle:

    // TODO: Maybe this section should just be asm.
    // It's very sensitive to timing!

    UNTIL_RD_HIGH;

   
    asm volatile("@ early clear/write " : : :);
    // immediately output the output word
    // PERF: Can move this around, but the bus transciever should be
    // taking care of shutting off the outputs for us. For example,
    // it seems to work before the UNTIL_RD_HIGH loop too.
    bcm2835_gpio_clr_multi_nb(OUTPUT_MASK);
    
    // Now we always write data. /OE pin controls whether/when the bus
    // transciever actually outputs it to bus.
    // We previously set the whole output mask to 0, so we only need to
    // worry about the 1 bits here (assumes output_word & ~OUTPUT_MASK = 0!)

    bcm2835_gpio_set_multi_nb(output_word);
    asm volatile("@ deglitch start " : : :);
    asm volatile("nop" : : :);

    // Read a few more times, since RD seems to go high earlier than the
    // address lines become stable. This makes a huge difference.
    // (But the timing is very sensitive here. 2 is too early, 4 too
    // slow!)
    // delayTicks(0);
    DEGLITCH_READ;
    asm volatile("nop" : : :);
    // Ugh, figure out a way to either tune this or make the timing more
    // automatic?
    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);    asm volatile("nop" : : :);
    DEGLITCH_READ;
    asm volatile("@ deglitch end " : : :);

    asm volatile("@ addr decode start " : : :);
    addr = DecodeAddress(inputs);
    asm volatile("@ addr decode end " : : :);
    
    // Now check address bits. A13 tells us whether this was in
    // the first or second half of the packet.

    if (inputs & (1 << PIN_A13)) {
      // "VRAM": nametable or attribute. We only decoded the low 10
      // bits, so this ignores mirroring.
      if (addr < 960) {
	// nametable tile. we learn the coarse scanline.
	// 32 tiles per row.
	scanline = ((addr >> 5) << 3) | (scanline & 7);

	// and column.
	// (XXX I think this fetch is actually like 2 tiles ahead of
	// where we really are?)
	col = addr & 31;
	
	packetbyte = 0;
      } else {
	// Attribute read. Can't learn much from the address.
	packetbyte = 1;
      }
    } else {
      // pattern table reads.
      // here the address tells us the fine scanline.
      // Every one of the 256 tiles is 16 bytes, and we first
      // read (tile << 4) and then ((tile << 4) | 8).

      // (Note: We could use this to get the coarse scanline, assuming
      // that we didn't corrupt the tile fetched in packet 0.)
      // The low three bits are always supplied by the PPU so they
      // reliably indicate the scanline.
      scanline = (scanline & ~7) | (addr & 7);

      // Was this the first fetch or second?
      if (!(addr & 8)) {
	// First (low color bit)
	packetbyte = 2;
      } else {
	// Second (high color bit)
	packetbyte = 3;
      }
    }

    asm volatile("@ sync end " : : :);

    // Get the output word for the NEXT cycle.
    {
      if (packetbyte < 3) {
	output_word = GetEncodedByte(screen, scanline, col, packetbyte + 1);
      } else {
	// XXX this reads beyond the last column (and probably fails on
	// the first).
	// But we have to deal with sprites and crap out there anyway.
	output_word = GetEncodedByte(screen, scanline, col + 1, 0);
      }
    }

    asm volatile("@ getbyte end " : : :);

    asm volatile("@ wait low loop " : : :);
    // In case we were too fast, wait for RD to go low. (Necessary?)
    // (Note that this overwrites any address info we had.)
    UNTIL_RD_LOW;
    asm volatile("@ wait low loop end " : : :);
    
    goto next_cycle;

  vblank:
    frames++;

    if (desync < min_desync) min_desync = desync;
    if (desync > max_desync) max_desync = desync;

    if (!DISABLE_INTERRUPTS && frames % 60 == 0) {
      printf("%d frames. %d comp. packets, %d desync (%d--%d)\n",
	     frames, complete_packets,
	     desync, min_desync, max_desync);
      min_desync = 999999;
      max_desync = -1;
    }
    desync = 0;
    complete_packets = 0;
    
    // Yield to OS. Does nothing if interrupts are disabled.
    Yield();

    slideshow.Update();
    goto next_frame;
  }

  CHECK(bcm2835_close());
  printf("Clean exit\n");
  return 0;
}
