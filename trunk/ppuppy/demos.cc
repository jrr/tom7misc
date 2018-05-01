
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <thread>
#include <mutex>

#include "demos.h"
#include "convert.h"

#include "schedule.h"
#include "armsnes/libretro/libretro.h"
#include "threadutil.h"
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

static std::mutex snes_mutex;
static bool snes_do_frame = false;
static uint8 snes_joy = 0;
// PERF store directly as 565?
static ImageRGB snes_img{256, 240};
static int snes_frames = 0;
static int snes_client_frames = 0;
static Screen screens[2];
// Is 0 or 1. screens[double_buffer] is owned by the thread, and
// screens[double_buffer ^ 1] is owned by ppuppy. Only the main
// thread writes this, in Update.
static int double_buffer = 0;

// TODO: Should be some way to exit this thread, too
void SNES::Run() {
  ScheduleHighPriority(2);
  printf("SNES thread at pid %d\n", getpid());
  // Thread. We don't want to run out of control, so we are locked to
  // ppuppy's vblank detection. The way we do this is to block on a
  // mutex, which is released by Update.

  for (;;) {
    int buffer_target = 0;
    for (;;) {
      MutexLock ml(&snes_mutex);
      if (snes_do_frame) {
	// Consume the frame with the lock held.
	snes_do_frame = false;
	snes_client_frames++;
	buffer_target = double_buffer;
	break;
      }
    }
    
    retro_run();

    // Note: It's possible for a race (gotta be pretty unlucky since
    // it only takes about 2ms to render an SNES frame but we have
    // ~16ms) here. It happens when double_buffer has already switched
    // to the other value, and so we're writing to the screen that
    // ppuppy is currently reading from. Assuming that the undefined
    // behavior is limited to returning garbage in the data race
    // region, which is probably better than blocking on a mutex
    // (which would flash the whole screen from missing deadlines). I
    // cache the buffer target above with the lock held, because
    // if it doesn't have the value 0 or 1, we get much worse (crashy)
    // behavior here.
    MakePalette(PaletteMethod::GREYSCALE,
		&snes_img, &rc, &screens[buffer_target]);
    FillScreenSelective(&snes_img, &screens[buffer_target]);
  }
}

SNES::SNES(const string &cart) : rc("snes") {
  // Thread blocks until Update allows it to run.
  snes_mutex.lock();
  snes_do_frame = false;
  
  // OK to create this thread now.
  th.reset(new std::thread(&SNES::Run, this));
  
  // Initialize armsnes.
  // These callbacks all happen within the SNES thread,
  // even though we're running as the main thread right now
  // (we want to at least block until the game is loaded).
  
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
  snes_mutex.unlock();
}

void SNES::Update(uint8 joy1, uint8 joy2) {
  {
    MutexLock ml(&snes_mutex);
    // Pass joystick, signal that frame is ready
    snes_joy = joy1;
    snes_do_frame = true;
    double_buffer ^= 1;
    snes_frames++;
  }
  if (snes_frames % 60 == 0) {
    printf("SNES frame %d\n", snes_frames);
  }
}

Screen *SNES::GetScreen() {
  return &screens[double_buffer ^ 1];
}
