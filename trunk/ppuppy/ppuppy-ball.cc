#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

#include <string>
#include <vector>
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

// Gives good timing, but requires a hard restart to
// get back to linux.
static constexpr bool DISABLE_INTERRUPTS = false;

static constexpr int TRACE_FRAME = -10;

// input pins, as wired on red solder board
static constexpr uint8 PIN_RD = 16;
static constexpr uint8 PIN_A0 = 14;
static constexpr uint8 PIN_A1 = 15;
static constexpr uint8 PIN_A2 = 18;
static constexpr uint8 PIN_A3 = 23;
static constexpr uint8 PIN_A4 = 24;
static constexpr uint8 PIN_A5 = 25;
static constexpr uint8 PIN_A6 = 8;
static constexpr uint8 PIN_A7 = 7;
static constexpr uint8 PIN_A8 = 12;
static constexpr uint8 PIN_A9 = 20;
static constexpr uint8 PIN_A13 = 21;

// output pins, wired to bus transciever
static constexpr uint8 POUT_D0 = 2;
static constexpr uint8 POUT_D1 = 3;
static constexpr uint8 POUT_D2 = 4;
static constexpr uint8 POUT_D3 = 5;
// Use 6 next, of course. But 7 is used on address side.

static constexpr bool WRITE_NAMETABLE = false;
static constexpr bool WRITE_ATTRIBUTE = false;

static constexpr bool TRY_RESYNC = true;

static constexpr int ON_TICKS = 4;

// static constexpr bool DO_WRITE[4] = { true, true, true, true, };

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

// In the steady state, the PPU does four reads per 8-pixel
// strip (i.e., the slice from one scanline). Let's call this a packet:
//   A. character from nametable
//   B. attribute byte from attribute table
//   C. pixel bits for corresponding CHR (low)
//   D. pixel bits for corresponding CHR (high)
// Should we really store the tile index, or just return it
// programmatically?
#define PACKETBYTES 4
uint8 screen[SCANLINES * TILESW * PACKETBYTES] = {0};

// Cheap bouncing-ball effect.
#define FRAMES 2
// Number of times we entered vsync.
int frames = 0;
struct Ball {
  int bdx = 1, bdy = 2, bx = 8 * 8, by = 15 * 8;
  void Update() {
    bx += bdx;
    by += bdy;
    if (bx < 0) { bx = 0; bdx = -bdx; }
    if (by < 0) { by = 0; bdy = -bdy; }
    if (bx > (31 * 8)) { bx = 31 * 8; bdx = -bdx; }
    if (by > (29 * 8)) { by = 29 * 8; bdy = -bdy; }
  }
};
Ball ball1;
Ball ball2{-3, 1, 20, 180};

void UpdateFrame() {
  ball1.Update();
  ball2.Update();
}

uint8 GetByte(int coarse, int fine, int col, int b) {
  // Checkerboard is easier to see through all the noise,
  // and does "work".
  switch (b) {
    // default:
    // return (col > 16) != (coarse > 15);

    // This confirms that we can switch the palette
    // every scanline
  case 0:
    // case 1:
    // return fine & 1;
    // return 0;
    // return frames & 0xFF;
    return 0;
  case 1:
    return 0;
    // return (fine & 1) ? 0xFF : 0x00;
  case 2:
    {
      int dx = (col * 8) - ball2.bx;
      int dy = (coarse * 8 + fine) - ball2.by;
      static constexpr int sqdia = 56 * 56;
      return (((dx * dx) + (dy * dy)) < sqdia) ? 0xFF : 0x00;
    }

  case 3:
    {
      int dx = (col * 8) - ball1.bx;
      int dy = (coarse * 8 + fine) - ball1.by;
      static constexpr int sqdia = 56 * 56;
      return (((dx * dx) + (dy * dy)) < sqdia) ? 0xFF : 0x00;
    }

    // return ((col > 16) != (coarse > 15)) ? 0xFF : 0x00;
    // return ((col >> 1) & 1);
    // return (coarse >> 2) & 1;
    return 0;
    // return (fine >> 2) & 1;
    // very noisy
    // return ((fine >> 1) & 1) ^ ((col >> 1) & 1);
  default:
    break;
  }
  int idx = (((coarse << 3) | fine) + col) * PACKETBYTES + b;
  // XXX should record how often this happens...
  if (idx >= SCANLINES * TILESW * PACKETBYTES) return 0;
  else return screen[idx];
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

// PERF! the screen should be stored as pre-decoded words!
inline uint32 Encode(uint8 byte) {
  static_assert(POUT_D0 == 2, "hard-coded for performance");
  static_assert(POUT_D1 == 3, "hard-coded for performance");
  static_assert(POUT_D2 == 4, "hard-coded for performance");
  static_assert(POUT_D3 == 5, "hard-coded for performance");
  // Just four bits supported right now.
  uint32 word = (uint32)(byte & 15) << 2;
  return word;
}

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

void InitImage() {
  // First just clear everything to zero.
  for (int i = 0; i < SCANLINES * TILESW * PACKETBYTES; i++) {
    screen[i] = 0;
  }

  // Set nametable to identity.
  for (int y = 0; y < 30; y++) {
    for (int x = 0; x < 32; x++) {
      int idx = y * 32 + x;
      for (int s = 0; s < 8; s++) {
	screen[(y * 8 * TILESW + s * TILESW + x) * 4] = idx & 255;
      }
    }
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
	int dx = (x - 128);
	int dy = (y - 120);

	if (dx * dx + dy * dy < dsquared) {
	  // TODO: Get some color info in here.
	  bits_lo |= 1;
	  bits_hi |= 1;
	}
      }
      int idx = (y * TILESW + t) * PACKETBYTES;
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

  InitImage();

  // Set input.
  struct Trace {
    Trace(uint16 addr, int coarse_sl, int fine_sl, int col, int packetbyte,
	  uint8 next_byte)
      : addr(addr), coarse_sl(coarse_sl), fine_sl(fine_sl),
	col(col), packetbyte(packetbyte), next_byte(next_byte) {}
    uint16 addr;
    int coarse_sl;
    int fine_sl;
    int col;
    int packetbyte;
    uint8 next_byte;
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
  
  // mask for output word.
  static constexpr uint32 OUTPUT_MASK =
    (1 << POUT_D0) | (1 << POUT_D1) | (1 << POUT_D2) | (1 << POUT_D3);

  // Shift the raw input history. Read a new raw input, then compute the
  // new deglitched inputs. The deglitched values are computed by voting.
  // (TODO: Consider reading until we have consecutive successes, or so?)
  #define DEGLITCH_READ \
    inputs_2 = inputs_1; \
    inputs_1 = inputs_0; \
    inputs_0 = bcm2835_gpio_lev_multi_nb(); \
    inputs = (inputs_2 & inputs_1) | (inputs_1 & inputs_0) | (inputs_0 & inputs_2);
    
  // PERF: In these loops, only need a memory barrier after the
  // last read. And that memory barrier can be the same one that
  // happens before the first write!
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
    
    // This is the next word we'll write, as soon as the read cycle
    // starts. By prepping the next word in advance, we give ourselves
    // the minimal latency between the read and write.
    uint32 next_word = 0;
    // The last value read of the input lines.
    uint32 inputs = 0;
    // The previous three reads, without deglitching. 0 is the most
    // recent, 1 before that, 2 before THAT. These are used in the
    // deglitching code. Everything else should just trust "inputs".
    uint32 inputs_0 = 0, inputs_1 = 0, inputs_2 = 0;
    // Decoded address read from input.
    uint16 addr = 0;

    // What byte of the packet are we in? 0-3.
    int packetbyte = 0;

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
    
    {
      uint8 next_byte = GetByte(coarse_scanline, fine_scanline, col,
				packetbyte);
      // XXX Note: this works, but the computation in InitScreen does not
      // put the right byte here. Look closely at that method (maybe should
      // be separating the different planes?)
      // if (!packetbyte) next_byte = addr;

      next_word = Encode(next_byte);
      if (false && !DISABLE_INTERRUPTS && frames == TRACE_FRAME) {
	uint16 full = addr | (!!(inputs & (1 << PIN_A13)) << 13);
	trace.emplace_back(full, coarse_scanline, fine_scanline, col, packetbyte,
			   next_byte);
      }
    }

    // Now we always write data. /OE pin controls whether/when the bus transciever
    // actually outputs it to bus.
    bcm2835_gpio_write_mask_nb(next_word, OUTPUT_MASK);

    // In case we were too fast, wait for RD to go low. (Necessary?)
    // (Note that this overwrites any address info we had.)
    UNTIL_RD_LOW;

    goto next_cycle;
    

  vblank:
    frames++;
    UpdateFrame();
    if (!DISABLE_INTERRUPTS && frames == TRACE_FRAME + 3) {
      FILE *f = fopen("trace.txt", "wb");
      for (const Trace &t : trace) {
	fprintf(f,
		"     %04x\n"
		"[%c] write %02x cy: %d fy: %d col: %d\n",
		t.addr, 
		"ABCD"[t.packetbyte], t.next_byte,
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
    
    // Yield to OS. For production, should perhaps disable this
    // so that nothing else is ever scheduled.
    Yield();
    // Back to beginning of screen.
    col = 0;
    coarse_scanline = 0;
    fine_scanline = 0;
    packetbyte = 0;
    
    // Now wait until end of vblank.
    do {
      DEGLITCH_READ;
    } while (inputs & (1 << PIN_RD));
    // while ((inputs = bcm2835_gpio_lev_multi()) & (1 << PIN_RD)) {}
    goto next_cycle;
  }

 done:
  CHECK(bcm2835_close());
  printf("Clean exit\n");
  return 0;
}
