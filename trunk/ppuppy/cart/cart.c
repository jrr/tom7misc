
#define PPU_CTRL                ((unsigned char*)0x2000U)
#define PPU_MASK                ((unsigned char*)0x2001U)
#define PPU_STATUS              ((unsigned char*)0x2002U)
#define SCROLL                  ((unsigned char*)0x2005U)
#define PPU_ADDRESS             ((unsigned char*)0x2006U)
#define PPU_DATA                ((unsigned char*)0x2007U)

#define OAM_ADDRESS             ((unsigned char*)0x2003U)
#define OAM_DMA                 ((unsigned char*)0x4014U)

#define JOY1                    ((unsigned char*)0x4016U)
#define JOY2                    ((unsigned char*)0x4017U)

#define APU_STATUS              ((unsigned char*)0x4015U)
#define APU_PULSE1_ENV          ((unsigned char*)0x4000U)
#define APU_PULSE1_SWEEP        ((unsigned char*)0x4001U)
#define APU_PULSE1_TIMER        ((unsigned char*)0x4002U)
#define APU_PULSE1_LEN          ((unsigned char*)0x4003U)
#define APU_PULSE2_ENV          ((unsigned char*)0x4004U)
#define APU_PULSE2_SWEEP        ((unsigned char*)0x4005U)
#define APU_PULSE2_TIMER        ((unsigned char*)0x4006U)
#define APU_PULSE2_LEN          ((unsigned char*)0x4007U)
// TODO TRIANGLE/NOISE/DMC

#define KNOCK_ADDR ((1U << 13) | 0x002AU)

// Note, this is backwards from simplefm2.
#define RIGHT    0x01
#define LEFT     0x02
#define DOWN     0x04
#define UP       0x08
#define START    0x10
#define SELECT   0x20
#define B_BUTTON 0x40
#define A_BUTTON 0x80

// Two writes to this memory-mapped register.
#define SET_PPU_ADDRESS(addr) \
  do { \
    *PPU_ADDRESS = (addr >> 8) & 0xff; \
    *PPU_ADDRESS = addr & 0xff; \
  } while (0)

#pragma bss-name(push, "ZEROPAGE")
unsigned char ignore;
// Incremented whenever NMI happens.
unsigned char client_nmi;

unsigned char joy1;
unsigned char joy2;
// XXX Would be nice for these two implementation details to just be
// defined in input.s...
unsigned char joy1test;
unsigned char joy2test;

unsigned char scroll_x;
#pragma bss-name(pop)

#pragma bss-name(push, "OAM")
unsigned char SPRITES[256];
// OAM equals ram addresses 200-2ff
#pragma bss-name(pop)

// Globals
unsigned char index;
unsigned char sindex = 0;

// Counter for demo
unsigned char counter;

unsigned char old_joy1, old_joy2;

unsigned char knock_ack;

// input.s
void GetInput();
// dma.s
void DoDMA();

const unsigned char TEXT[] = {
  "\x31\x31\x19\x15\x12\x0a\x1c\x12\x31\x1c\x1d\x0a\x1b\x1d\x31\x31"
};

// Greys from darkest to lightest:
// 1D 2D 00 10 3D 20
// In fceux there are really just four
// distinct colors: black (e.g. 1f),
// dark grey (e.g. 2d and 00)
// light grey (e.g. 10 and 3d)
// and white (e.g. 20, 30)

// So here are some combinations of greys and sepias.
const unsigned char PALETTE[] = {
  0x1f, 0x00, 0x10, 0x20,
  0x1f, 0x08, 0x18, 0x28,
  0x1f, 0x18, 0x28, 0x20,
  0x1f, 0x00, 0x10, 0x08,
  // sprite palette is just for debugging; these should
  // not be visible in normal operation
  0x1f, 0x21, 0x22, 0x23,
  0x1f, 0x24, 0x25, 0x26,
  0x1f, 0x27, 0x28, 0x29,
  0x1f, 0x2a, 0x2b, 0x2c,
};

unsigned char fromppu[16] = {
  0x00,  0x00,  0x00,  0x00,
  0x00,  0x00,  0x00,  0x00,
  0x00,  0x00,  0x00,  0x00,
  0x00,  0x00,  0x00,  0x00,
};

// red palette to write when we didn't sync
const unsigned char bad_sync[16] = {
  0x1f,  0x07,  0x16,  0x25,
  0x1f,  0x07,  0x16,  0x25,
  0x1f,  0x07,  0x16,  0x25,
  0x1f,  0x07,  0x16,  0x25,
};

// spritz.html

const int sinetable[64] = {0, 1, 2, 3, 4, 5, 6, 6, 7, 8, 8, 9, 9, 10, 10, 10, 10, 10, 10, 10, 9, 9, 8, 8, 7, 6, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5, -6, -6, -7, -8, -8, -9, -9, -10, -10, -10, -10, -10, -10, -10, -9, -9, -8, -8, -7, -6, -6, -5, -4, -3, -2, -1, };

const int costable[64] = {10, 10, 10, 10, 9, 9, 8, 8, 7, 6, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5, -6, -6, -7, -8, -8, -9, -9, -10, -10, -10, -10, -10, -10, -10, -9, -9, -8, -8, -7, -6, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 6, 7, 8, 8, 9, 9, 10, 10, 10, };

const unsigned char spr_x[64] = {56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 64, 72, 80, 88, 96, 96, 96, 96, 96, 96, 176, 176, 176, 176, 176, 176, 176, 168, 176, 168, 168, 168, 168, 168, 168, 168, 104, 104, 104, 104, 104, 104, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 96, 0, 0, 0, 0, 0, };

const unsigned char spr_y[64] = {64, 72, 80, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 120, 112, 112, 112, 120, 128, 136, 144, 152, 160, 160, 152, 144, 136, 128, 120, 96, 96, 88, 88, 120, 128, 136, 144, 152, 160, 160, 152, 144, 136, 128, 120, 160, 152, 144, 136, 128, 120, 112, 104, 96, 88, 80, 72, 64, 112, 251, 251, 251, 251, 251, };

unsigned char jiggleframe;
unsigned char debug_red_mode;

void main() {
  // turn off the screen
  *PPU_CTRL = 0;
  *PPU_MASK = 0;

  client_nmi = 0;
  counter = 0;
  old_joy1 = 0;
  old_joy2 = 0;
  joy1 = 0;
  joy2 = 0;
  jiggleframe = 0;
  // This starts enabled.
  debug_red_mode = 1;

  knock_ack = 0;

  *APU_STATUS = 0x0f;
  *APU_PULSE1_ENV = 0x0f;
  *APU_PULSE1_LEN = 0x01;

  // load the palette
  // set an address in the PPU of 0x3f00
  SET_PPU_ADDRESS(0x3f00U);
  for (index = 0; index < sizeof(PALETTE); ++index) {
    *PPU_DATA = PALETTE[index];
  }

  // Stick some arbitrary colors in the attribute table.
  SET_PPU_ADDRESS(0x23d3);
  for (index = 0; index < 16; ++index) {
    counter += 0x37;
    counter = (counter << 3) | (counter >> 5);
    *PPU_DATA = counter;
  }

  // Blit text.
  // This is about the middle of the screen
  SET_PPU_ADDRESS(0x21caU);
  for (index = 0; index < sizeof(TEXT); ++index) {
    *PPU_DATA = TEXT[index];
  }

  // Reset the scroll position.
  SET_PPU_ADDRESS(0x0000U);
  *SCROLL = 0;
  *SCROLL = 0;

  // turn on screen
  *PPU_CTRL = 0x90;      // screen is on, NMI on
  *PPU_MASK = 0x1e;

  // Put sprites in their initial positions. The loop
  // below also does this dynamically.
  for (index = 0; index < 64; index++) {
    SPRITES[index * 4 + 0] = spr_y[index];
    SPRITES[index * 4 + 1] = 0x7A;
    SPRITES[index * 4 + 2] = index & 3;
    SPRITES[index * 4 + 3] = spr_x[index];
  }

  // Wait for ppuppy loop.
  for (;;) {
    // Sync to NMI flag (set in interrupt handler in reset.s)
    while (!client_nmi) {}
    DoDMA();

    SET_PPU_ADDRESS(KNOCK_ADDR + 5);
    // This read is just garbage (whatever ppuppy wrote last).
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 4);
    // Also garbage.
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 3);
    // Also garbage.
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 2);
    // Also garbage.
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 1);
    // Now, expect the ack byte.
    knock_ack = *PPU_DATA;

    SET_PPU_ADDRESS(0x0000U);
    *SCROLL = jiggleframe;
    *SCROLL = 0;

    client_nmi = 0;

    // assuming now out of vblank. do what we gotta do.

    // Jiggle sprites a little. It's actually too slow (!) to do all the sprites
    // in one frame, so we split them in half for this.
    if (jiggleframe & 1) {
      sindex = 32;
      index = 32 * 4;
      while (sindex < 64 - 5) {
        SPRITES[index] = spr_y[sindex] + sinetable[(jiggleframe + sindex) & 63];
        index += 3;
        SPRITES[index] = spr_x[sindex] + costable[(jiggleframe + sindex) & 63];
        index++;
        sindex++;
      }
    } else {
      sindex = 0;
      index = 0;

      while (sindex < 32) {
        SPRITES[index] = spr_y[sindex] + sinetable[(jiggleframe + sindex) & 63];
        index += 3;
        SPRITES[index] = spr_x[sindex] + costable[(jiggleframe + sindex) & 63];
        index++;
        sindex++;
      }
    }

    // Play sounds to assist in diagnosing problems if the
    // video isn't working.
    if ((jiggleframe & 15) == 0) {
      *APU_STATUS = 0x0f;
      *APU_PULSE1_ENV = 0x0f;
      *APU_PULSE1_TIMER = jiggleframe;
      *APU_PULSE1_LEN = 0x01;

      *APU_PULSE2_ENV = 0x0f;
      // One tone depends on the controller input.
      *APU_PULSE2_TIMER = (joy1 + 1);
      *APU_PULSE2_LEN = 0x01;
    }

    jiggleframe++;
    jiggleframe &= 63;

    GetInput();

    if (knock_ack == 0x27)
      break;
  }

  // Turn off sprites!
  // BGRs bMmG
  // 0000 1010 <- what we set = 10
  // BGR are NTSC emphasis
  // s: enable sprites
  // b: enable background
  // Mm: show sprites/background in leftmost column
  // G: Greyscale
  *PPU_MASK = 10;

  // Turn off sound!
  *APU_STATUS = 0x00;

  // Game loop. This can be interrupted at any moment by NMI
  for (;;) {
    // Sync to NMI flag (set in interrupt handler in reset.s)
    while (!client_nmi) {}

    // ------------------------------------------------------
    // In this region we can access the PPU, but we only have
    // 1.6ms before it starts rendering again.

    // Here: read PPU to get palette and fine scroll.
    // Recall that ppuppy can't quite keep up with PPU reads
    // (it's one behind). So we have to prime it. ppuppy
    // decodes addr lines 0-9 (1k of address space) and A13
    // Since fetches with A13 high are nametable fetches,
    // these always happen in a predictable order. So we'll
    // knock a sequence of reads that the PPU never does on
    // its own:

    // PERF: This stuff is actually pretty slow (takes about
    // half of vblank today, 0.81ms). We can improve both
    // the protocol (maybe don't need so much knocking; also the
    // palette reads can be sequential) and its implementation
    // (unroll loops, use asm).

    // Note: I haven't figured out why, yet, but the data lags
    // by TWO packets here. I should try to fix that, but for
    // now, give a long lead-in so that ppuppy can sync.

    SET_PPU_ADDRESS(KNOCK_ADDR + 5);
    // This read is just garbage (whatever ppuppy wrote last).
    // PERF: cc65 still stores to the variable "ignore"
    // even though the stores are dead. Consider (void)?
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 4);
    // Also garbage.
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 3);
    // Also garbage.
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 2);
    // Also garbage.
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 1);
    // Now, expect the ack byte.
    knock_ack = *PPU_DATA;

    // Simpler than sprite trick, just do some reads here to
    // pass the joystick data.
    // TODO: Getting the wrong joystick input is kinda bad, so
    // it might make sense to include some redundancy here.
    // (Could send twice; use checksum bits, etc.)
    // But ppuppy can also just integrate across frames; we
    // can give up some controller latency for sure.
    *PPU_ADDRESS = 0x20;
    *PPU_ADDRESS = joy1;
    ignore = *PPU_DATA;
    *PPU_ADDRESS = 0x20;
    *PPU_ADDRESS = joy2;
    ignore = *PPU_DATA;

    // Now read 16 bytes. At this point ppuppy isn't even looking at
    // the address (except A13), so just read somewhere else in the
    // nametable (we don't want a read of e.g. KNOCK_ADDR + 5 here to
    // be confused for the beginning of the knock sequence if we are
    // desynchronized).
    SET_PPU_ADDRESS(0x2100);
    for (index = 0; index < 16; ++index) {
      fromppu[index] = *PPU_DATA;
    }

    // Do a final read so that ppuppy can go back into its normal
    // state. (XXX I think this can be eliminated.)
    ignore = *PPU_DATA;

    // If we got a correct knock, write the palette back
    // into internal PPU memory.
    if (knock_ack == 0x27) {
      SET_PPU_ADDRESS(0x3f00U);
      for (index = 0; index < 16; ++index) {
        *PPU_DATA = fromppu[index];
      }
      // Since the 0th color is mirrored, we can use this
      // slot after the first full palette (so, index 4) to
      // pass more data. Here, the x scroll.
      scroll_x = fromppu[4];
      if (fromppu[8] == 0x2A &&
          fromppu[12] == 0xA7) {
        debug_red_mode = 0;
      }
    } else {
      // Didn't get the expected pattern, so we are desynchronized
      // or disconnected.
      if (debug_red_mode) {
        // Unless we disabled this by a previous code, show a
        // red palette to indicate the desynchronization.
        SET_PPU_ADDRESS(0x3f00U);
        for (index = 0; index < 16; ++index) {
          *PPU_DATA = bad_sync[index];
        }
      }
      // (otherwise, we leave the palette as it is.)

      // Assume scroll 0 if we're desynced.
      scroll_x = 0;
    }


    // We always need to set the scroll position at the end of our PPU
    // work. But anyway, we want to reflect the fine x scroll.
    SET_PPU_ADDRESS(0x0000U);
    *SCROLL = scroll_x;
    *SCROLL = 0;

    client_nmi = 0;

    // ------------------------------------------------------
    // After this point we can do lots of computation, but
    // don't touch the PPU.

    // Update controller state.
    old_joy1 = joy1;
    old_joy2 = joy2;

    GetInput();
  }
};

