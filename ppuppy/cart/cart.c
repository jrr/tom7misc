
#define PPU_CTRL                ((unsigned char*)0x2000U)
#define PPU_MASK                ((unsigned char*)0x2001U)
#define PPU_STATUS              ((unsigned char*)0x2002U)
#define SCROLL                  ((unsigned char*)0x2005U)
#define PPU_ADDRESS             ((unsigned char*)0x2006U)
#define PPU_DATA                ((unsigned char*)0x2007U)

#define JOY1                    ((unsigned char*)0x4016U)
#define JOY2                    ((unsigned char*)0x4017U)

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
// Incremented whenever NMI happens.
unsigned char client_nmi;

unsigned char joy1;
unsigned char joy2;
// Would be nice for these two impl. details to just be
// define in input.s
unsigned char joy1test;
unsigned char joy2test;
#pragma bss-name(pop)

// #pragma bss-name(push, "OAM")
// unsigned char SPRITES[256];
// OAM equals ram addresses 200-2ff

// Globals
unsigned char index;

// Counter for demo
unsigned char counter;

unsigned char old_joy1, old_joy2;

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

#if 0
unsigned char ReadJoy(unsigned char *addr) {
  unsigned char ret = 0;
  *addr = 1U;
  ret = *addr;
  ret <<= 1;
  ret |= *addr;
  ret <<= 1;
  ret |= *addr;
  ret <<= 1;
  ret |= *addr;
  ret <<= 1;
  ret |= *addr;
  ret <<= 1;
  ret |= *addr;
  ret <<= 1;
  ret |= *addr;
  ret <<= 1;
  ret |= *addr;
  return ret;
}
#endif

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

    SET_PPU_ADDRESS(screen_pos);
    *PPU_DATA = joy1; // counter++;

    // Seems like we always need to set the scroll position
    // at the end of our PPU work.
    SET_PPU_ADDRESS(0x0000U);
    *SCROLL = 0;
    *SCROLL = 0;

    client_nmi = 0;

    // ------------------------------------------------------
    // After this point we can do lots of computation, but
    // don't touch the PPU.

    // Update controller state.
    old_joy1 = joy1;
    old_joy2 = joy2;

    GetInput();

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

