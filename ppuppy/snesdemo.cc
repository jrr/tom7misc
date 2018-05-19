
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
#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"

// Note that due to all the global variables in armsnes, two instances
// of this cannot coexist!
//
// That's actually sort of bad because we can't show multiple
// games within a session (once we lose access to linux).
// But it'll suffice for the talk. (Could always reboot, too.)
// Last joystick value

// Mutexes seem to work better.
#if 0
// Use spinlocks -- the critical sections should be short, we have
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

enum DemoState {
  STATE_STARTSCREEN,
  STATE_SNES,
  STATE_NES,
};

static DemoState demo_state = STATE_STARTSCREEN;

static bool even_frame = true;
static bool snes_do_frame = false;
static bool snes_frame_done = false;
static uint8 snes_joy = 0;
static int snes_frames = 0;
static int snes_client_frames = 0;
static Screen start_screen;
// Triple buffered. Treated modularly, screens[buf] is the one
// currently being displayed. [buf+1] is the next frame. [buf+2] is
// being written by the SNES thread.
static Screen screens[3];
static int buf = 0;

static Emulator *nes = nullptr;

// Used only by thread.
static ArcFour rc("snes");

static Screen *retro_screen_target = &screens[0];

static inline uint8 ConvertJoy(uint8 native) {
  uint8 fceulib = 0;
  if (native & A_BUTTON) fceulib |= INPUT_A;
  if (native & B_BUTTON) fceulib |= INPUT_B;
  if (native & START) fceulib |= INPUT_T;
  if (native & SELECT) fceulib |= INPUT_S;
  if (native & UP) fceulib |= INPUT_U;
  if (native & DOWN) fceulib |= INPUT_D;
  if (native & LEFT) fceulib |= INPUT_L;
  if (native & RIGHT) fceulib |= INPUT_R;
  return fceulib;
}

// TODO: Should be some way to exit this thread, too
void SNES::Run() {
  ScheduleHighPriority(2);
  printf("SNES thread at pid %d\n", getpid());
  // Thread. We don't want to run out of control, so we are locked to
  // ppuppy's vblank detection. The way we do this is to block on a
  // mutex, which is released by Update.

  for (;;) {
    for (;;) {
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

    switch (demo_state) {
      // The video callback herein populates the screen
      // natively from the 565 format.
    case STATE_SNES:
      retro_run();
      NoDebugPalette(retro_screen_target);
      break;
    case STATE_NES:
      // XXX can easily support both controllers.
      nes->StepFull(ConvertJoy(snes_joy), 0);
      MakePaletteNES(nes->RawIndexedImage(), retro_screen_target);
      FillScreenFastNES(nes->RawIndexedImage(), retro_screen_target);
      break;
    case STATE_STARTSCREEN:
      // Should not get here, but if so, nothing to do.
      break;
    } 

    CRITICAL_BEGIN;
    snes_frame_done = true;
    CRITICAL_END;
  }
}

SNES::SNES(const string &cart) {
  start_screen = ScreenFromFile("images/titletest.png");
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
    even_frame = !even_frame;
    MakePalette565(data, width, height, pitch, even_frame, retro_screen_target);
    FillScreenFast565(data, width, height, pitch, even_frame, retro_screen_target);
  });

  nes = Emulator::Create("mario.nes");
  if (nes == nullptr) {
    printf("Failed to load NES?!\n");
  }
  
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
  switch (demo_state) {
  case STATE_STARTSCREEN:
    if (joy1 & START) {
      demo_state = STATE_SNES;
      return;
    }
    break;
  case STATE_NES:
  case STATE_SNES:
  {
    if ((joy1 & (SELECT | B_BUTTON)) == (SELECT | B_BUTTON)) {
      CRITICAL_BEGIN;
      if (demo_state == STATE_NES)
	demo_state = STATE_SNES;
      else if (demo_state == STATE_SNES)
	demo_state = STATE_NES;
      CRITICAL_END;
      break;
    }

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
  if (false && update_calls % 60 == 0) {
    printf("SNES frame %d [lag %d]\n",
	   snes_frames,
	   snes_frames_not_done);
  }
  }
}

Screen *SNES::GetScreen() {
  switch (demo_state) {
  case STATE_NES:
  case STATE_SNES:
  return &screens[buf];
  default:
  case STATE_STARTSCREEN:
    return &start_screen;
  }
}

Screen *SNES::GetNextScreen() {
  switch (demo_state) {
  case STATE_NES:
  case STATE_SNES:
  return &screens[(buf + 1) % 3];
  default:
  case STATE_STARTSCREEN:
    return &start_screen;
  }
}
