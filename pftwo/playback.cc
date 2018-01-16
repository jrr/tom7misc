
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

      if (SDL_PollEvent(&event)) {
	switch (event.type) {
	case SDL_QUIT:
	  return;
	case SDL_KEYDOWN:
	  switch (event.key.keysym.sym) {

	  case SDLK_ESCAPE:
	    return;

	  case SDLK_RIGHT:
	    break;

	  case SDLK_0:
	    df = 1;
	    break;
	    
	  case SDLK_MINUS:
	    df--;
	    if (df < 0) df = 0;
	    break;

	  case SDLK_EQUALS:
	  case SDLK_PLUS:
	    df++;
	    break;

	  case SDLK_SPACE:
	    switch (mode) {
	    case Mode::PAUSE:
	      mode = Mode::PLAY;
	      if (df <= 0) df = 1;
	      break;
	    case Mode::PLAY:
	      mode = Mode::PAUSE;
	      break;
	    }
	    break;
	    
	  case SDLK_LEFT: {
	    mode = Mode::PAUSE;
	    df = 0;
	    auto it = snapshots.lower_bound(frameidx);
	    if (it == snapshots.begin()) break;
	    --it;
	    fprintf(stderr, "Seek to %d\n", it->first);
	    frameidx = it->first;
	    emu->LoadUncompressed(it->second);
	    // XXX need to execute a frame in order
	    // to have something to show..
	    break;
	  }
	    
	  default:
	    break;
	  }
	  break;
	default:
	  break;
	}
      }
	
      // PERF: Don't need to clear the game part.
      sdlutil::clearsurface(screen, 0x00000000);
      
      if (mode == Mode::PLAY) {
	// Can we execute at least one frame?
	if (frameidx >= movie.size() || df <= 0) {
	  mode = Mode::PAUSE;
	} else {

	  // Execute some frames, and end with StepFull.
	  for (int i = 0; i < df; i++) {
	    if (frameidx < movie.size()) {
	      // There's a frame available to play.
	      // Do it.
	      uint8 p1, p2;
	      std::tie(p1, p2) = movie[frameidx];
	      frameidx++;
	      // If we'll do more frames in this loop, then
	      // no need for a full step.
	      if (i < df - 1 && frameidx < movie.size()) {
		emu->Step(p1, p2);
	      } else {
		emu->StepFull(p1, p2);
	      }
	      if (0 == frameidx % SNAPSHOT_EVERY) {
		snapshots[frameidx] = emu->SaveUncompressed();
	      }
	    }
	  }

	}
      }

      // Above should leave the emulator in a state where
      // StepFull was just run.

      // Now draw/play.
      vector<uint8> rgba = emu->GetImage();
      BlitRGBA2x(rgba, 256, 256, 0, 16, screen);

      
      font->draw(0, 4, StringPrintf("%d^2/^<%d", frameidx, (int)movie.size()));
      
      // SDL_Delay(1000.0 / 30.0);

      if (mode == Mode::PLAY) {
	font->draw(0, 520, StringPrintf("PLAY ^2%d^3x", df));
      } else {
	font->draw(0, 520, "PAUSED");
      }
      SDL_Flip(screen);
    }

  }

  void Run() {
    screen = sdlutil::makescreen(WIDTH, HEIGHT);
    CHECK(screen);

    fprintf(stderr,
	    "BPP: %d\n"
	    "Screen shifts:\n"
	    "R %d\n"
	    "G %d\n"
	    "B %d\n"
	    "A %d\n"
	    "Screen masks:\n"
	    "R %08x\n"
	    "G %08x\n"
	    "B %08x\n"
	    "A %08x\n",
	    screen->format->BitsPerPixel,
	    screen->format->Rshift,
	    screen->format->Gshift,
	    screen->format->Bshift,
	    screen->format->Ashift,
	    screen->format->Rmask,
	    screen->format->Gmask,
	    screen->format->Bmask,
	    screen->format->Amask);
    
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

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);
  
  SDL_Surface *icon = SDL_LoadBMP("icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  UIThread ui{game, movie};
  ui.Run();
  
  SDL_Quit();
  return 0;
}
