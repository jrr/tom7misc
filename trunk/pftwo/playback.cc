
#include <algorithm>
#include <vector>
#include <string>
#include <set>
#include <memory>
#include <list>

#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"

#include "SDL.h"
#include "graphics.h"

// Screen dimensions.
#define WIDTH 512
#define HEIGHT (512 + 64)

// Save state periodically so that rewind is fast.
#define SNAPSHOT_EVERY 1000

static bool should_die = false;
static mutex should_die_m;

std::mutex print_mutex;
#define Printf(fmt, ...) do {			\
    MutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);			\
  } while (0)

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

// Note: This is really the main thread. SDL really doesn't
// like being called outside the main thread, even if it's
// exclusive.
struct UIThread {
  vector<pair<uint8, uint8>> movie;
  // Snapshots enable fast rewinding. Sparse. The key is the index of
  // the frame that is next to be executed (so a key of 0 is the state
  // upon loading, without executing any frames).
  std::map<int64, vector<uint8>> snapshots;
  unique_ptr<Emulator> emu;
  int frameidx = 0;
  
  UIThread(const string &game,
	   const string &fm2) {
    // TODO: Get subtitles and other metadata from movie.
    movie = SimpleFM2::ReadInputs2P(fm2);
    CHECK(!movie.empty()) << "Couldn't read movie: " << fm2;
    emu.reset(Emulator::Create(game));
    CHECK(emu.get() != nullptr) << game;
  }

  enum class Mode {
    PLAY,
    PAUSE,
  };
  Mode mode = Mode::PLAY;
  // Speed (nes frames per SDL frame).
  int df = 1;
  
  void Loop() {
    SDL_Surface *surf = sdlutil::makesurface(256, 256, true);
    (void)surf;
    
    for (;;) {
      SDL_Event event;
      SDL_PollEvent(&event);
      switch (event.type) {
      case SDL_QUIT:
	return;
      case SDL_KEYDOWN:
	switch (event.key.keysym.sym) {

	case SDLK_ESCAPE:
	  return;

	case SDLK_RIGHT:
	  break;

	case SDLK_SPACE:
	  switch (mode) {
	  case Mode::PAUSE:
	    mode = Mode::PLAY;
	    break;
	  case Mode::PLAY:
	    mode = Mode::PAUSE;
	    break;

	  }
	  
	default:
	  break;
	}
	break;
      default:
	break;
      }

      if (mode == Mode::PLAY) {
	// Can we execute at least one frame?
	if (frameidx >= movie.size()) {
	  mode = Mode::PAUSE;
	} else {

	  // Execute some frames, and end with StepFull.
	  for (int i = 0; i < df; i++) {
	    if (frameidx < movie.size()) {
	      // There's a frame available to play.
	      // Do it. XXX: Don't always do full
	      // step if df > 1.
	      uint8 p1, p2;
	      std::tie(p1, p2) = movie[frameidx];
	      frameidx++;
	      emu->StepFull(p1, p2);
	      if (0 == frameidx % SNAPSHOT_EVERY) {
		snapshots[frameidx] = emu->SaveUncompressed();
	      }
	    }
	  }

	  // Now draw/play.
	  vector<uint8> argb = emu->GetImage();
	  BlitARGB2x(argb, 256, 256, 0, 16, screen);
	}
      } 
      
      // SDL_Delay(1000.0 / 30.0);

      SDL_Flip(screen);
    }

  }

  void Run() {
    screen = sdlutil::makescreen(WIDTH, HEIGHT);
    CHECK(screen);

    font = Font::create(screen,
			"font.png",
			FONTCHARS,
			FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
    CHECK(font != nullptr) << "Couldn't load font.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";

    maxfont = Font::create(screen,
			   "fontmax.png",
			   FONTCHARS,
			   MAXFONTWIDTH, MAXFONTHEIGHT, FONTSTYLES, 4, 3);
    CHECK(maxfont != nullptr) << "Couldn't load maxfont.";
    
    Loop();
    Printf("UI shutdown.\n");
    WriteWithLock(&should_die_m, &should_die, true);
  }

  ~UIThread() {
    // XXX free screen
    delete font;
  }

 private:
  static constexpr int FONTWIDTH = 9;
  static constexpr int FONTHEIGHT = 16;
  static constexpr int SMALLFONTWIDTH = 6;
  static constexpr int SMALLFONTHEIGHT = 6;
  static constexpr int MAXFONTHEIGHT = 48 * 2;
  static constexpr int MAXFONTWIDTH = 27 * 2;

  Font *font = nullptr, *smallfont = nullptr, *maxfont = nullptr;

  ArcFour rc{"ui_thread"};
  SDL_Surface *screen = nullptr;
};

// The main loop for SDL.
int main(int argc, char *argv[]) {
  (void)Rtos;
  
  if (argc < 3) {
    fprintf(stderr, "playback.exe game.nes movie.fm2\n");
    return -1;
  }
  string game = argv[1];
  string movie = argv[2];
  
  if (game != "contra.nes") {
    fprintf(stderr, "Sorry, contra.nes is hardcoded because of AOT.\n");
    return -1;
  }

  fprintf(stderr, "Init SDL\n");

  /* Initialize SDL and network, if we're using it. */
  CHECK(SDL_Init(SDL_INIT_VIDEO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_Surface *icon = SDL_LoadBMP("icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  UIThread ui{game, movie};
  ui.Run();
  
  SDL_Quit();
  return 0;
}
