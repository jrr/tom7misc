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

// Gives good timing, but requires a hard restart to
// get back to linux.
static constexpr bool DISABLE_INTERRUPTS = false;

static constexpr int TRACE_FRAME = -10;

static constexpr bool WRITE_NAMETABLE = false;
static constexpr bool WRITE_ATTRIBUTE = false;

static constexpr int ON_TICKS = 4;

BouncingBalls bouncing;
EncodedScreen encoded_screen;

// PERF storing coarse and fine separately wastes some ops
uint32 GetEncodedByte(int coarse, int fine, int col, int b) {
  const int idx = (coarse + fine) * NUM_COLS + col;
  if (idx >= NUM_SCANLINES * NUM_COLS) return 0;
  
  switch (b) {
    default:
    case 0: return 0;
    case 1: return encoded_screen.encoded_attr[idx];
    case 2: return encoded_screen.encoded_color_lo[idx];
    case 3: return encoded_screen.encoded_color_hi[idx];
    break;
  }
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
  (1 << POUT_D0) | (1 << POUT_D1) | (1 << POUT_D2) | (1 << POUT_D3);

// Decode the low 10 bits of the address.
inline uint16 DecodeAddress(uint32 inputs) {
  // PERF probably can do this with tricks (e.g. a few lookup tables).
  // PERF also could mask the bit with a constant and then shift it
  // once (needs some more pin constants so that we know which way to
  // shift and how much).
  // PERF: Does this compute as uint32 but should be uint16?
  return
    (((inputs >> PIN_A0) & 1) << 0) |
    (((inputs >> PIN_A1) & 1) << 1) |
    (((inputs >> PIN_A2) & 1) << 2) |
    (((inputs >> PIN_A3) & 1) << 3) |
    (((inputs >> PIN_A4) & 1) << 4) |
    (((inputs >> PIN_A5) & 1) << 5) |
    (((inputs >> PIN_A6) & 1) << 6) |
    (((inputs >> PIN_A7) & 1) << 7) |
    (((inputs >> PIN_A8) & 1) << 8) |
    (((inputs >> PIN_A9) & 1) << 9);
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

  // Set input.
  struct Trace {
    Trace(uint16 addr, int coarse_sl, int fine_sl, int col, int packetbyte,
	  uint32 output_word)
      : addr(addr), coarse_sl(coarse_sl), fine_sl(fine_sl),
      col(col), packetbyte(packetbyte), output_word(output_word) {}
    uint16 addr;
    int coarse_sl;
    int fine_sl;
    int col;
    int packetbyte;
    uint32 output_word;
  };
  std::vector<Trace> trace;
  trace.reserve(50000);

  for (uint8 p : { PIN_RD, PIN_A0, PIN_A1, PIN_A2, PIN_A3,
	PIN_A4, PIN_A5, PIN_A6, PIN_A7, PIN_A8, PIN_A9,
	PIN_A13 }) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

  for (uint8 p : {POUT_D0, POUT_D1, POUT_D2, POUT_D3}) {
    bcm2835_gpio_fsel(p, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(p, BCM2835_GPIO_PUD_OFF);
  }

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
    int coarse_scanline = 0;
    // From 0 to 7. Determined from the row of the tile bitmap read.
    int fine_scanline = 0;
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

  next_frame:
    // In the steady state, this needs to complete during vsync.
    bouncing.Draw();
    EncodeScreen(bouncing.screen, &encoded_screen);

    // Assume we are at the top-left.
    col = 0;
    coarse_scanline = 0;
    fine_scanline = 0;
    packetbyte = 0;
    
    // Now wait until end of vblank.
    do {
      DEGLITCH_READ;
    } while (inputs & (1 << PIN_RD));
    
  next_cycle:
      
    UNTIL_RD_HIGH;
    // Read a few more times, since RD seems to go high earlier than the
    // address lines become stable. This makes a huge difference.
    // (But the timing is very sensitive here. 2 is too early, 4 too
    // slow!)
    DEGLITCH_READ;
    DEGLITCH_READ;
    DEGLITCH_READ;
    addr = DecodeAddress(inputs);

    // Now check address bits. A13 tells us whether this was in
    // the first or second half of the packet.
    if (inputs & (1 << PIN_A13)) {
      // "VRAM": nametable or attribute. We only decoded the low 10
      // bits, so this ignores mirroring.
      if (addr < 960) {
	// nametable tile. we learn the coarse scanline.
	// 32 tiles per row.
	coarse_scanline = addr >> 5;
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
      fine_scanline = addr & 7;

      // Was this the first fetch or second?
      if (!(addr & 8)) {
	// First (low color bit)
	packetbyte = 2;
      } else {
	// Second (high color bit)
	packetbyte = 3;
      }
    }
    
    output_word = GetEncodedByte(coarse_scanline, fine_scanline, col,
				 packetbyte);
    if (false && !DISABLE_INTERRUPTS && frames == TRACE_FRAME) {
      uint16 full = addr | (!!(inputs & (1 << PIN_A13)) << 13);
      trace.emplace_back(full, coarse_scanline, fine_scanline, col,
			 packetbyte, output_word);
    }

    // Now we always write data. /OE pin controls whether/when the bus
    // transciever actually outputs it to bus.
    bcm2835_gpio_write_mask_nb(output_word, OUTPUT_MASK);

    // In case we were too fast, wait for RD to go low. (Necessary?)
    // (Note that this overwrites any address info we had.)
    UNTIL_RD_LOW;

    goto next_cycle;

  vblank:
    frames++;
    if (!DISABLE_INTERRUPTS && frames == TRACE_FRAME + 3) {
      FILE *f = fopen("trace.txt", "wb");
      for (const Trace &t : trace) {
	fprintf(f,
		"     %04x\n"
		"[%c] write %08x cy: %d fy: %d col: %d\n",
		t.addr, 
		"ABCD"[t.packetbyte], t.output_word,
		t.coarse_sl, t.fine_sl, t.col);
      }
      fclose(f);
      goto done;
    }

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

    goto next_frame;
  }

 done:
  CHECK(bcm2835_close());
  printf("Clean exit\n");
  return 0;
}
