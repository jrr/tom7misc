
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <thread>
#include <mutex>
#include <atomic>

#include "snesdemo.h"
#include "convert.h"
#include "convert565.h"

#include "schedule.h"
#include "armsnes/libretro/libretro.h"
#include "threadutil.h"
#include "util.h"
#include "talk.h"
#include "base/logging.h"

// Note that due to all the global variables in armsnes, two instances
// of this cannot coexist!
//
// That's actually sort of bad because we can't show multiple
// games within a session (once we lose access to linux).
// But it'll suffice for the talk. (Could always reboot, too.)
// Last joystick value

// Mutexes seem to work better.
#if 0
// static std::mutex snes_mutex;
// We use spinlocks -- the critical sections should be short, we have
// nothing else to do, and each thread gets its own CPU core.
std::atomic_flag snes_lock = ATOMIC_FLAG_INIT;
#define CRITICAL_BEGIN \
  while (snes_lock.test_and_set(std::memory_order_acquire)) {}
#define CRITICAL_END \
  snes_lock.clear(std::memory_order_release)
#else
// Use mutexes like normal people.
std::mutex snes_mutex;
#define CRITICAL_BEGIN snes_mutex.lock()
#define CRITICAL_END snes_mutex.unlock()
#endif

static bool snes_do_frame = false;
static bool snes_frame_done = false;
static uint8 snes_joy = 0;
// PERF store directly as 565?
// static ImageRGB snes_img{256, 240};
static int snes_frames = 0;
static int snes_client_frames = 0;
// Triple buffered. Treated modularly, screens[buf] is the one
// currently being displayed. [buf+1] is the next frame. [buf+2] is
// being written by the SNES thread.
static Screen screens[3];
static int buf = 0;

// For speed, we only update part of the palette each frame.
// This is only accessed by the SNES thread.
static uint8 palette_cache[16] = {
  0x1d, 0x00, 0x2d, 0x30,
  0x00, 0x10, 0x2d, 0x3d,
  0x00, 0x00, 0x10, 0x3d,
  0x00, 0x2d, 0x3d, 0x30,
};

// Used only by thread.
static ArcFour rc("snes");

static Screen *retro_screen_target = &screens[0];

// TODO: Should be some way to exit this thread, too
void SNES::Run() {
  ScheduleHighPriority(2);
  printf("SNES thread at pid %d\n", getpid());
  // Thread. We don't want to run out of control, so we are locked to
  // ppuppy's vblank detection. The way we do this is to block on a
  // mutex, which is released by Update.

  for (;;) {
    for (;;) {
      // PERF better to use a spin-lock? We should have the whole
      // CPU to ourselves.
      CRITICAL_BEGIN;
      if (snes_do_frame) {
	// Consume the frame with the lock held.
	snes_do_frame = false;
	snes_client_frames++;
	int buffer_target = (buf + 2) % 3;
	retro_screen_target = &screens[buffer_target];
	CRITICAL_END;
	break;
      }
      CRITICAL_END;
    }

    // printf("client: %d\n", snes_client_frames);
    
    // Note: It's possible for a race (gotta be pretty unlucky since
    // it only takes about 2ms to render an SNES frame but we have
    // ~16ms) here. It happens when buf has already switched
    // to the next value, and so we're writing to the screen that
    // ppuppy is currently reading from. Assuming that the undefined
    // behavior is limited to returning garbage in the data race
    // region, which is probably better than blocking on a mutex
    // (which would flash the whole screen from missing deadlines). I
    // cache the buffer target above with the lock held, because
    // if it doesn't have an in-bound value, we get much worse (crashy)
    // behavior here.

    // The video callback herein populates the screen
    // natively from the 565 format.
    retro_run();
    NoDebugPalette(retro_screen_target);

    CRITICAL_BEGIN;
    snes_frame_done = true;
    CRITICAL_END;
  }
}

SNES::SNES(const string &cart) {
  // XXX replace with 'never initialized' 
  screens[0] = ScreenFromFile("images/marioboot.png");
  memcpy(&screens[1], &screens[0], sizeof (Screen));
  memcpy(&screens[2], &screens[0], sizeof (Screen));

  // Fill in tables before any convert565 routines.
  InitClosestColors565();
  
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
    MakePalette565(data, width, height, pitch, palette_cache, &rc,
		   retro_screen_target);
    FillScreenFast565(data, width, height, pitch, retro_screen_target);
    
#if 0
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
#endif
  });

  // OK to create this thread now.
  // Start in the state where we request a frame but
  // (of course) haven't computed one yet.
  snes_frame_done = false;
  snes_do_frame = true;
  th.reset(new std::thread(&SNES::Run, this));
}

static int update_calls = 0;
static int snes_frames_not_done = 0;
void SNES::Update(uint8 joy1, uint8 joy2) {
  {
    CRITICAL_BEGIN;

    // Always use latest joystick info.
    snes_joy = joy1;
    // If the snes completed a frame, then we advance
    // here.
    if (snes_frame_done) {
      // Consume the frame.
      snes_frame_done = false;
      snes_do_frame = true;
      // Set next buffer target, round robin.
      buf++; buf %= 3;
      snes_frames++;
    } else {
      // Otherwise, keep rendering the existing frame.
      // (this causes us to get the palette wrong, though,
      // urgh..)
      snes_frames_not_done++;
    }
    
    CRITICAL_END;
  }
  update_calls++;
  if (update_calls % 60 == 0) {
    printf("SNES frame %d [lag %d]\n",
	   snes_frames,
	   snes_frames_not_done);
  }
}

Screen *SNES::GetScreen() {
  return &screens[buf];
}

Screen *SNES::GetNextScreen() {
  return &screens[(buf + 1) % 3];
}
