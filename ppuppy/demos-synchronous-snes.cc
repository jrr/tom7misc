
#include <stdio.h>
#include <string.h>

#include "demos.h"
#include "convert.h"

#include "armsnes/libretro/libretro.h"
#include "util.h"

void BouncingBalls::Ball::Update() {
  bx += bdx;
  by += bdy;
  if (bx < 0) { bx = 0; bdx = -bdx; }
  if (by < 0) { by = 0; bdy = -bdy; }
  if (bx > (31 * 8)) { bx = 31 * 8; bdx = -bdx; }
  if (by > (29 * 8)) { by = 29 * 8; bdy = -bdy; }
}

inline bool Contains(const BouncingBalls::Ball &ball, int x, int y) {
  int dx = x - ball.bx;
  int dy = y - ball.by;
  return ((dx * dx) + (dy * dy)) < ball.sqdia;
}

void BouncingBalls::Draw() {
  frames++;
  if (true || frames % 8 == 0) {
    ball1.Update();
    ball2.Update();
  }

  screen.palette[0] = 0x1d;
  screen.palette[1] = 0x15;
  screen.palette[2] = 0x27;
  screen.palette[3] = 0x2a;
  screen.palette[4] = 0x0;
  
  // Here, using palette 0 for the entire screen.
  for (int y = 0; y < NUM_SCANLINES; y++) {
    for (int x = 0; x < NUM_COLS; x++) {
      int idx = y * NUM_COLS + x;
      screen.attr[idx] = 0; // (frames & 1) ? 0xFF : 0x00;
    }
  }
  
  // Compute the image a single pixel at a time. Assumes we have
  // plenty of time during vblank.
  for (int y = 0; y < NUM_SCANLINES; y++) {
    for (int x = 0; x < 256; x++) {
      // Write the bits into the appropriate byte.
      const int idx = (y * NUM_COLS) + (x >> 3);
      // The bits are arranged from msb to lsb, like
      // you would want. (1 << 7) is the leftmost.
      const int bidx = 7 - (x & 7);
      const int b = ((x & 7) == 2) | ((y & 7) == 3);
      uint8 lobit = Contains(ball1, x, y) ? (1 << bidx) : (b << bidx);
      uint8 hibit = Contains(ball2, x, y) ? (1 << bidx) : 0;
      // Keep all other bits the same.
      const uint8 keep_mask = ~(1 << bidx);
      screen.color_lo[idx] = (screen.color_lo[idx] & keep_mask) | lobit;
      screen.color_hi[idx] = (screen.color_hi[idx] & keep_mask) | hibit;
    }
  }
}

Slideshow::Slideshow(const vector<string> &filenames) {
  screens.reserve(filenames.size());
  for (const string &f : filenames) {
    screens.push_back(ScreenFromFile(f));
  }
}

void Slideshow::Update(uint8 joy1, uint8 joy2) { frames++; }

Screen *Slideshow::GetScreen() {
  return &screens[(frames >> 8) % screens.size()];
}

// Note that due to all the global variables in armsnes, two instances
// of this cannot coexist!
//
// That's actually sort of bad because we can't show multiple
// games within a session (once we lose access to linux).
// But it'll suffice for the talk. (Could always reboot, too.)
// Last joystick value
static uint8 snes_joy = 0;
// PERF store directly as 565?
ImageRGB snes_img{256, 240};

SNES::SNES(const string &cart) : rc("snes") {
  retro_set_environment([](unsigned cmd, void *data) {
    return false;
  });
  retro_init();
  retro_set_audio_sample_batch([](const int16_t *data,
                                  size_t frames) -> size_t {
      return 0;
   });

  retro_set_get_inputs([]() -> uint32 {
    // XXX need to expand this correctly (and probably use both
    // joysticks) (but coincidentally this has at least left/right
    // working as well as some buttons hooked up to NES buttons,
    // so mario is playable)
    return snes_joy |
      (snes_joy << 8) |
      (snes_joy << 16) |
      (snes_joy << 24);
  });

  retro_game_info gameinfo;
  gameinfo.path = strdup(cart.c_str());
  string game = Util::ReadFile(gameinfo.path);
  gameinfo.data = game.data();
  gameinfo.size = game.size();
  gameinfo.meta = "";

  printf("Load %s ...\n", cart.c_str());
  retro_load_game(&gameinfo);
  printf("Loaded.\n");

  retro_set_video_refresh([](const void *data,
                          unsigned width, unsigned height, size_t pitch) {
    int idx = 0;
    for (int y = 0; y < height; y++) {
      uint16 *line = (uint16*)&((uint8 *)data)[y * pitch];
      for (int x = 0; x < width; x++) {
	uint16 packed = line[x];
	uint8 b = (packed & 31) << 3;
	uint8 g = ((packed >> 5) & 63) << 2;
	uint8 r = ((packed >> 11) & 31) << 3;
	snes_img.rgb[idx++] = r;
	snes_img.rgb[idx++] = g;
	snes_img.rgb[idx++] = b;
      }
    }
  });
}

static int snes_frames = 0;
void SNES::Update(uint8 joy1, uint8 joy2) {
  snes_joy = joy1;
  snes_frames++;
  if (snes_frames % 2 == 0) {
    retro_run();
    MakePalette(PaletteMethod::MOST_COMMON, &snes_img, &rc, &screen);
    FillScreenSelective(&snes_img, &screen);
  }
}
