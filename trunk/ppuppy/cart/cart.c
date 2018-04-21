
#define PPU_CTRL                ((unsigned char*)0x2000U)
#define PPU_MASK                ((unsigned char*)0x2001U)
#define PPU_STATUS              ((unsigned char*)0x2002U)
#define SCROLL                  ((unsigned char*)0x2005U)
#define PPU_ADDRESS             ((unsigned char*)0x2006U)
#define PPU_DATA                ((unsigned char*)0x2007U)

#define JOY1                    ((unsigned char*)0x4016U)
#define JOY2                    ((unsigned char*)0x4017U)

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

// #pragma bss-name(push, "OAM")
// unsigned char SPRITES[256];
// OAM equals ram addresses 200-2ff

// Globals
unsigned char index;

// Counter for demo
unsigned char counter;

unsigned char old_joy1, old_joy2;

unsigned char knock_ack;

void GetInput();

unsigned int screen_pos;

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

  knock_ack = 0;

  // load the palette
  // set an address in the PPU of 0x3f00
  SET_PPU_ADDRESS(0x3f00U);
  for (index = 0; index < sizeof(PALETTE); ++index) {
    *PPU_DATA = PALETTE[index];
  }

  // Stick some arbitrary colors
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

  screen_pos = 0x21caU;
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

    SET_PPU_ADDRESS(KNOCK_ADDR + 3);
    // This read is just garbage (whatever ppuppy wrote last).
    ignore = *PPU_DATA;

    SET_PPU_ADDRESS(KNOCK_ADDR + 2);
    // ppuppy will return whatever is predicted by a normal
    // read for knock_addr + 3 (so, some attribute byte).
    ignore = *PPU_DATA;
    SET_PPU_ADDRESS(KNOCK_ADDR + 1);
    // but now since it saw two consecutive reads that can't
    // happen in normal ppu operation, it's synced.
    // TODO: could check that we get the expected byte here,
    // and ignore the update (and do something visible, e.g.
    // blank palette for debugging) if not.
    knock_ack = *PPU_DATA;

    // Simpler than sprite trick, just do some reads here to
    // pass the joystick data.
    // TODO: Getting the wrong joystick input is kinda bad, so
    // it might make sense to include some redundancy here.
    // (Could send twice; use checksum bits, etc.)
    // But ppuppy can also just integrate across frames; we
    // can give up some latency for sure.
    // TODO: could be checking more knock_ack
    *PPU_ADDRESS = 0x20;
    *PPU_ADDRESS = joy1;
    ignore = *PPU_DATA;
    *PPU_ADDRESS = 0x20;
    *PPU_ADDRESS = joy2;
    ignore = *PPU_DATA;

    // Now read 16 bytes from the knock addr
    for (index = 0; index < 16; ++index) {
      // XXX: Actually, don't use the knock address here?
      // We don't even care what it is on the ppuppy side,
      // but we don't want to confuse it for a knock if
      // we're desynchronized?
      SET_PPU_ADDRESS(KNOCK_ADDR);
      fromppu[index] = *PPU_DATA;
    }

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
    } else {
      // Otherwise (for debugging), a visual indication that
      // we did not.
      SET_PPU_ADDRESS(0x3f00U);
      for (index = 0; index < 16; ++index) {
        *PPU_DATA = bad_sync[index];
      }
      // Assume scroll 0 if we're desynced.
      scroll_x = 0;
    }

    // XXX demo stuff.
    SET_PPU_ADDRESS(screen_pos);
    *PPU_DATA = joy1; // counter++;

    // Seems like we always need to set the scroll position
    // at the end of our PPU work.
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

    // XXX demo stuff.
    {
      unsigned char new1 = joy1 & (~old_joy1);
      if (new1 & UP) {
        screen_pos -= 32;
      } else if (new1 & DOWN) {
        screen_pos += 32;
      }

      if (new1 & LEFT) {
        screen_pos --;
      } else if (new1 & RIGHT) {
        screen_pos ++;
      }
    }

  }
};

