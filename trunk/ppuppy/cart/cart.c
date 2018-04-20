
#define PPU_CTRL                *((unsigned char*)0x2000U)
#define PPU_MASK                *((unsigned char*)0x2001U)
#define PPU_STATUS              *((unsigned char*)0x2002U)
#define SCROLL                  *((unsigned char*)0x2005U)
#define PPU_ADDRESS             *((unsigned char*)0x2006U)
#define PPU_DATA                *((unsigned char*)0x2007U)

// Two writes to this memory-mapped register.
#define SET_PPU_ADDRESS(addr) \
  do { \
    PPU_ADDRESS = (addr >> 8) & 0xff; \
    PPU_ADDRESS = addr & 0xff; \
  } while (0)

// Globals
unsigned char index;
// Incremented whenever NMI happens.
unsigned char client_nmi;

unsigned char prev;

const unsigned char TEXT[] = {
  "Hello World!"
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
};


void main() {
  // turn off the screen
  PPU_CTRL = 0;
  PPU_MASK = 0;

  client_nmi = 0;
  prev = 0;

  // load the palette
  // set an address in the PPU of 0x3f00
  SET_PPU_ADDRESS(0x3f00U);
  for (index = 0; index < sizeof(PALETTE); ++index) {
    PPU_DATA = PALETTE[index];
  }

  // load the text
  // This is about the middle of the screen
  SET_PPU_ADDRESS(0x21caU);
  for (index = 0; index < sizeof(TEXT); ++index) {
    PPU_DATA = TEXT[index];
  }

  // reset the scroll position
  SET_PPU_ADDRESS(0x0000U);
  SCROLL = 0;
  SCROLL = 0;

  // turn on screen
  PPU_CTRL = 0x90;      // screen is on, NMI on
  PPU_MASK = 0x1e;

  // Game loop. This can be interrupted at any moment by NMI
  for (;;) {
    if (client_nmi) {
      SET_PPU_ADDRESS(0x21ca);
      PPU_DATA = prev++;

      SET_PPU_ADDRESS(0x0000U);
      SCROLL = 0;
      SCROLL = 0;

      client_nmi = 0;
    }
  }
};

