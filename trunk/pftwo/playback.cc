
// TODO: Don't blank the screen when we fall behind.
// TODO: Read metadata like goals from the FM2 or some future format.

#include <algorithm>
#include <vector>
#include <string>
#include <set>
#include <memory>
#include <list>

#ifdef __MINGW32__
#define byte win_byte_override
#include <windows.h>
#undef byte
#undef ARRAYSIZE
#endif

#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/cart.h"
#include "../fceulib/ppu.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/re2/re2.h"

#include "SDL.h"
#include "graphics.h"

// Screen dimensions.
#define WIDTH 512
#define HEIGHT (512 + 128)

#define MAX_HEADROOM 24

// Save state periodically so that rewind is fast.
#define SNAPSHOT_EVERY 1000

// Show the rotated spritesheet, which is mostly useful
// for debugging sprite tracking stuff from autocamera
// v1, which we no longer use.
static constexpr bool DRAW_SPRITESHEET false;

static bool should_die = false;
static mutex should_die_m;

static std::mutex print_mutex;
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

// TODO: To emulator utilities?
// Because of (e.g.) tall sprite stuff, it's not often directly
// useful outside of diagnostics like this.
static vector<uint8> GetSpriteSheet(Emulator *emu, int rotate = 0) {
  // I don't understand why Palette::FCEUD_GetPalette isn't working,
  // but the NES palette is basically constant (emphasis aside), so
  // let's just inline it to save time. RGB triplets.
  // (I think FCEUD_GetPalette needs the color value to be OR'd with
  // 0xC0, since the ppu writes some flags into the high bits. -tom7)
  static constexpr uint8 ntsc_palette[] = {
    0x80,0x80,0x80, 0x00,0x3D,0xA6, 0x00,0x12,0xB0, 0x44,0x00,0x96,
    0xA1,0x00,0x5E, 0xC7,0x00,0x28, 0xBA,0x06,0x00, 0x8C,0x17,0x00,
    0x5C,0x2F,0x00, 0x10,0x45,0x00, 0x05,0x4A,0x00, 0x00,0x47,0x2E,
    0x00,0x41,0x66, 0x00,0x00,0x00, 0x05,0x05,0x05, 0x05,0x05,0x05,
    0xC7,0xC7,0xC7, 0x00,0x77,0xFF, 0x21,0x55,0xFF, 0x82,0x37,0xFA,
    0xEB,0x2F,0xB5, 0xFF,0x29,0x50, 0xFF,0x22,0x00, 0xD6,0x32,0x00,
    0xC4,0x62,0x00, 0x35,0x80,0x00, 0x05,0x8F,0x00, 0x00,0x8A,0x55,
    0x00,0x99,0xCC, 0x21,0x21,0x21, 0x09,0x09,0x09, 0x09,0x09,0x09,
    0xFF,0xFF,0xFF, 0x0F,0xD7,0xFF, 0x69,0xA2,0xFF, 0xD4,0x80,0xFF,
    0xFF,0x45,0xF3, 0xFF,0x61,0x8B, 0xFF,0x88,0x33, 0xFF,0x9C,0x12,
    0xFA,0xBC,0x20, 0x9F,0xE3,0x0E, 0x2B,0xF0,0x35, 0x0C,0xF0,0xA4,
    0x05,0xFB,0xFF, 0x5E,0x5E,0x5E, 0x0D,0x0D,0x0D, 0x0D,0x0D,0x0D,
    0xFF,0xFF,0xFF, 0xA6,0xFC,0xFF, 0xB3,0xEC,0xFF, 0xDA,0xAB,0xEB,
    0xFF,0xA8,0xF9, 0xFF,0xAB,0xB3, 0xFF,0xD2,0xB0, 0xFF,0xEF,0xA6,
    0xFF,0xF7,0x9C, 0xD7,0xE8,0x95, 0xA6,0xED,0xAF, 0xA2,0xF2,0xDA,
    0x99,0xFF,0xFC, 0xDD,0xDD,0xDD, 0x11,0x11,0x11, 0x11,0x11,0x11,
  };

  vector<uint8> rgba;
  // 64 sprites, which are 8 pixels wide but 8 or 16 tall, RGBA.
  static constexpr int SPRITESHEET_WIDTH = 64 * 8;
  rgba.resize(SPRITESHEET_WIDTH * 16 * 4);
  memset(rgba.data(), 0, rgba.size());

  const PPU *ppu = emu->GetFC()->ppu;

  const uint8 ppu_ctrl = ppu->PPU_values[0];
  const uint8 ppu_mask = ppu->PPU_values[1];
  // Are sprites 16 pixels tall?
  const bool tall_sprites = !!(ppu_ctrl & (1 << 5));
  // const int sprite_height = tall_sprites ? 16 : 8;
  const bool sprites_enabled = !!(ppu_mask && (1 << 4));  
    
  if (!sprites_enabled) return rgba;

  // Note: This is ignored if sprites are tall (and determined instead
  // from the tile's low bit).
  const bool spr_pat_high = !!(ppu_ctrl & (1 << 3));
    
  // Total 256 bytes.
  const uint8 *spram = ppu->SPRAM;

  // Normally sprites are drawn from 63 to 0, but these are not going
  // to overlap because they will each be in their own slots.
  for (int n = 0; n < 64; n++) {
    const uint8 tile_idx = spram[n * 4 + 1];
    const uint8 attr = spram[n * 4 + 2];
    const bool v_flip = !!(attr & (1 << 7));
    const bool h_flip = !!(attr & (1 << 6));
    const uint8 colorbits = attr & 3;
    // const uint8 ypos = spram[n * 4 + 0];
    // const uint8 xpos = spram[n * 4 + 3];
    int ypos = 0;
    int xpos = ((n + rotate) % 64) * 8;
    
    const uint8 *palette_table = emu->GetFC()->ppu->PALRAM;

    // Draw one 8x8 sprite tile into rgba, using pattern table $0000
    // if first arg is false, $1000 if true. The tile index is the
    // index into that pattern. xdest and ydest are the screen
    // destination, which will be adjusted to place at the appropriate
    // place in the texture.
    //
    // When drawing 8x8 sprites, this will be all the work. For tall
    // sprites, we'll make two calls to this.
    auto OneTile =
      [emu, &rgba, v_flip, h_flip, colorbits, palette_table](
	  bool patterntable_high, uint8 tile_idx, int x0, int y0) {
      const uint32 spr_pat_addr = patterntable_high ? 0x1000 : 0x0000;
      const uint8 *vram = emu->GetFC()->cart->VPagePointer(spr_pat_addr);
	
      const int addr = tile_idx * 16;
      for (int row = 0; row < 8; row++) {
	const uint8 row_low = vram[addr + row];
	const uint8 row_high = vram[addr + row + 8];

	// bit from msb to lsb.
	for (int bit = 0; bit < 8; bit++) {
	  const uint8 value =
	    ((row_low >> (7 - bit)) & 1) |
	    (((row_high >> (7 - bit)) & 1) << 1);

	  const int px = h_flip ? x0 + (7 - bit) : (x0 + bit);
	  const int py = v_flip ? y0 + (7 - row) : (y0 + row);
	  const int pixel = (py * SPRITESHEET_WIDTH + px) * 4;

	  // For sprites, transparent pixels need to be drawn with
	  // alpha 0. The palette doesn't matter; 0 means transparent
	  // in every palette.
	  if (value == 0) {
	    rgba[pixel + 0] = 0x00;
	    rgba[pixel + 1] = 0x00;
	    rgba[pixel + 2] = 0x00;
	    rgba[pixel + 3] = 0x00;
	  } else {
	    // Offset with palette table. Sprite palette entries come
	    // after the bg ones, so add 0x10.
	    const uint8 palette_idx = 0x10 + ((colorbits << 2) | value);
	    // ID of global NES color gamut.
	    const uint8 color_id = palette_table[palette_idx];
	      
	    // Put pixel in sprite texture:
	    rgba[pixel + 0] = ntsc_palette[color_id * 3 + 0];
	    rgba[pixel + 1] = ntsc_palette[color_id * 3 + 1];
	    rgba[pixel + 2] = ntsc_palette[color_id * 3 + 2];
	    rgba[pixel + 3] = 0xFF;
	  }
	}
      }
    };
      
    if (tall_sprites) {
      // Odd and even tile numbers are treated differently.
      if ((tile_idx & 1) == 0) {
	// This page:
	// http://noelberry.ca/nes
	// verifies that tiles t and t+1 are drawn top then bottom.
	if (v_flip) {
	  // in y-flip scenarios, we have to flip the
	  // y positions here so that the whole 8x16 sprite is flipping,
	  // rather than its two 8x8 components. So tile_idx actually goes
	  // on bottom.
	  OneTile(false, tile_idx, xpos, ypos + 8);
	  OneTile(false, tile_idx + 1, xpos, ypos);
	} else {
	  OneTile(false, tile_idx, xpos, ypos);
	  OneTile(false, tile_idx + 1, xpos, ypos + 8);
	}
      } else {
	// XXX I assume this drops the low bit? I don't see that
	// documented but it wouldn't really make sense otherwise
	// (unless tile 255 wraps to 0?)
	if (v_flip) {
	  OneTile(true, tile_idx - 1, xpos, ypos + 8);
	  OneTile(true, tile_idx, xpos, ypos);
	} else {
	  OneTile(true, tile_idx - 1, xpos, ypos);
	  OneTile(true, tile_idx, xpos, ypos + 8);
	}
      }
    } else {
      OneTile(spr_pat_high, tile_idx, xpos, ypos);
    }
  }
  return rgba;
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
  std::map<int64, string> subtitles;
  unique_ptr<Emulator> emu;
  int frameidx = 0;
  
  UIThread(const string &game,
	   const string &fm2) {
    vector<pair<int, string>> subs;
    movie = SimpleFM2::ReadInputsEx(fm2, &subs);
    CHECK(!movie.empty()) << "Couldn't read movie: " << fm2;
    for (const auto &p : subs)
      subtitles[p.first] = p.second;

    emu.reset(Emulator::Create(game));
    CHECK(emu.get() != nullptr) << game;

    SDL_AudioSpec spec;
    spec.freq = Emulator::AUDIO_SAMPLE_RATE;
    spec.samples = 1024;
    spec.format = AUDIO_S16;
    spec.channels = 1;
    spec.userdata = (void*)this;
    spec.callback = [](void *userdata, Uint8 *stream, int len) {
      ((UIThread *)userdata)->AudioCallback(stream, len);
    };

    CHECK(0 == SDL_OpenAudio(&spec, nullptr));
    
    SDL_PauseAudio(0);
  }

  // In order to get smooth audio/video playback, we render up to a
  // few frames ahead of what the user is actually seeing.
  struct Frame {
    int frameidx = 0;
    vector<uint8> rgba;
    vector<int16> samples;
    string subtitle;
    int goalx = -1, goaly = -1;
    int samples_used = 0;

    // XXX instead, conditionally draw this into rgba if enabled
    vector<uint8> spram;
    int rotate = 0;
    
    // 512x16 RGBA image, with sprites 0-63 drawn.
    vector<uint8> spritesheet;
  };
  std::mutex frames_m;
  std::list<Frame> frames;
  
  void AudioCallback(Uint8 *byte_stream, int bytes) {
    int16 *samples = (int16 *)byte_stream;
    int n = bytes >> 1;
    MutexLock ml(&frames_m);

    // While I need new frames...
    while (n--) {
      // No more frames? Copy silence.
      if (frames.empty()) {
	*samples = 0;
	samples++;
      } else {
	Frame *f = &*frames.begin();
	*samples = f->samples[f->samples_used];
	samples++;
	f->samples_used++;
	if (f->samples_used == f->samples.size()) {
	  frames.pop_front();
	}
      }
    }
  }

  enum class Mode {
    PLAY,
    PAUSE,
  };
  Mode mode = Mode::PLAY;
  // Speed (nes frames per SDL frame).
  int df = 1;
  
  void Loop() {
    // XXX contra
    int rotate = 45;
    
    RE2 goal_re{"g ([-0-9]+),([-0-9]+)"};
    
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

	  case SDLK_q:
	    rotate--;
	    if (rotate < 0) rotate += 64;
	    printf("Rotate %d\n", rotate);
	    break;
	  case SDLK_w:
	    rotate++;
	    rotate %= 64;
	    printf("Rotate %d\n", rotate);
	    break;
	    
	  case SDLK_0:
	    df = 1;
	    break;
	    
	  case SDLK_MINUS:
	    if (event.key.keysym.mod & KMOD_CTRL) {
	      df -= 100;
	    } else {
	      df--;
	    }
	    if (df < 1) df = 1;
	    break;

	  case SDLK_EQUALS:
	  case SDLK_PLUS:
	    if (event.key.keysym.mod & KMOD_CTRL) {
	      df += 100;
	    } else {
	      df++;
	    }
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
	    {
	      MutexLock ml(&frames_m);
	      frames.clear();
	    }
	    mode = Mode::PAUSE;
	    df = 0;
	    auto it = snapshots.lower_bound(frameidx);
	    if (it == snapshots.begin()) break;
	    --it;
	    fprintf(stderr, "Seek to %lld\n", it->first);
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

	  int headroom = 0;
	  {
	    MutexLock ml(&frames_m);
	    headroom = frames.size();
	  }

	  if (headroom < MAX_HEADROOM) {
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

	    // Now push onto the frames list.
	    {
	      MutexLock ml(&frames_m);
	      frames.push_back(Frame());
	      Frame *f = &frames.back();
	      f->frameidx = frameidx;
	      f->samples_used = 0;
	      emu->GetImage(&f->rgba);
	      emu->GetSound(&f->samples);
	      const int rot = (frameidx * rotate) % 64;
	      f->spritesheet = GetSpriteSheet(emu.get(), rot);

	      {
		// XXX
		uint8 *spram = emu->GetFC()->ppu->SPRAM;
		f->spram.resize(0x100);
		for (int i = 0; i < 0x100; i++) f->spram[i] = spram[i];
		f->rotate = rot;
	      }
	      
	      auto it = subtitles.lower_bound(frameidx);
	      if (it == subtitles.begin()) break;
	      --it;
	      f->subtitle = it->second;

	      (void)RE2::PartialMatch(f->subtitle, goal_re,
				      &f->goalx, &f->goaly);
	      
	      if (df > 100) {
		// Just get rid of the sound if we're going this fast!
		for (int i = 0; i < f->samples.size(); i++)
		  f->samples[i] = 0;
	      } else if (df > 2) {
		int shift = 1;
		if (df > 5) shift++;
		if (df > 10) shift++;
		if (df > 50) shift++;
		for (int i = 0; i < f->samples.size(); i++)
		  f->samples[i] >>= shift;
	      }
	    }
	  }
	    
	}
      }

      // Above should leave the emulator in a state where
      // StepFull was just run.

      int headroom = 0;
      {
	MutexLock ml(&frames_m);
	headroom = frames.size();
	if (frames.empty()) {
	  // XXX this happens when we have a very high df. Would be
	  // better to just leave the existing screen, or maybe the
	  // audio callback should not delete the last frame?
	  sdlutil::FillRectRGB(screen,
			       0, 16, 
			       512, 512 + 16,
			       0x77, 0x0, 0x0);
	} else {
	  Frame *f = &*frames.begin();
	  static constexpr int XOFF = 0;
	  static constexpr int YOFF = 0;
	  // Now draw/play.
	  BlitRGBA2x(f->rgba, 256, 256, XOFF, YOFF, screen);

	  // Draw spritesheets into future!
	  if (DRAW_SPRITESHEET) {
	    static constexpr int NUM_SPRITESHEETS = 8;
	    static constexpr int YSTART = YOFF + 512 + 8;
	    auto fit = frames.begin();
	    for (int ss = 0;
		 YSTART + 16 * ss + 16 < HEIGHT &&
		 ss < NUM_SPRITESHEETS &&
		 fit != frames.end();
		 ss++) {
	      const Frame &ff = *fit;
	      BlitRGBA(ff.spritesheet, 512, 16, 0, YSTART + 16 * ss, screen);
	      ++fit;
	    }
	  }

	  font->draw(0, 4, StringPrintf("%d^2/^<%d",
					f->frameidx, (int)movie.size()));
	  font->draw(0, 20, f->subtitle);

	  if (f->goalx >= 0 && f->goaly >= 0) {
	    font->draw(0, 500, StringPrintf("GOAL %d,%d", f->goalx, f->goaly));
	    sdlutil::drawbox(screen,
			     2 * f->goalx - 5, YOFF + 2 * f->goaly - 5, 10, 10,
	    0x00, 0x00, 0x00);
	    sdlutil::drawbox(screen,
			     2 * f->goalx - 4, YOFF + 2 * f->goaly - 4, 8, 8,
	    0xFF, 0x00, 0xFF);
	    sdlutil::drawbox(screen,
			     2 * f->goalx - 3, YOFF + 2 * f->goaly - 3, 6, 6,
	    0xFF, 0x00, 0xFF);
  	  }

	  // And sprites.
	  // XXX obviously, make this optional
	  for (int i = 0; i < 64; i++) {
	    int sx = f->spram[i * 4 + 3];
	    int sy = f->spram[i * 4 + 0];
	    // printf("[%d] %d,%d ", i, sx, sy);

	    int rotidx = (i + f->rotate) % 64;
	    
	    fontsmall->draw(XOFF + 2 * sx, YOFF + 2 * sy,
			    StringPrintf("%d", rotidx));
	  }
	  // printf("\n");
	}
      }

      // Draw 
      if (mode == Mode::PLAY) {
	font->draw(0, HEIGHT - FONTHEIGHT, StringPrintf("PLAY ^2%d^3x", df));
      } else {
	font->draw(0, HEIGHT - FONTHEIGHT, "PAUSED");
      }

      font->draw(480, HEIGHT - FONTHEIGHT, StringPrintf("[%d]", headroom));

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

    fontsmall = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(fontsmall != nullptr) << "Couldn't load smallfont.";
    
    Loop();
    Printf("UI shutdown.\n");
    SDL_PauseAudio(1);
    WriteWithLock(&should_die_m, &should_die, true);
  }

  ~UIThread() {
    // XXX free screen
    delete font;
    delete fontsmall;
  }

 private:
  static constexpr int FONTWIDTH = 9;
  static constexpr int FONTHEIGHT = 16;
  static constexpr int SMALLFONTWIDTH = 6;
  static constexpr int SMALLFONTHEIGHT = 6;

  Font *font = nullptr, *fontsmall = nullptr;

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
  
  /* Initialize SDL, video, and audio. */
  CHECK(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);
  
  SDL_Surface *icon = SDL_LoadBMP("playback-icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  UIThread ui{game, movie};
  ui.Run();

  SDL_CloseAudio();
  SDL_Quit();
  return 0;
}
