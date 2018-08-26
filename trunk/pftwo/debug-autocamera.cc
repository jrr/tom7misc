// This is a one-off (presumably) for developing/debugging autocamera.

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

#include <cmath>
#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/simplefm7.h"
#include "../fceulib/cart.h"
#include "../fceulib/ppu.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/re2/re2.h"
#include "autocamera.h"
#include "autocamera2.h"

#include "SDL.h"
#include "graphics.h"

// Screen dimensions.
#define WIDTH 1920
#define HEIGHT 1080

#define WARMUP_FRAMES 800

#define SNAPSHOT_EVERY 1000

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

// Super Mario Bros, from wiki
// static constexpr int XLOC = 0x0086;  // dec 134
// static constexpr int YLOC = 0x00CE;  // dec 206
// static int XLOC = 0x030e;
// static int YLOC = 0x030d;
// static int XLOC = 0x330, YLOC = 0x360;
static int XLOC = 0x14b, YLOC = 0x148;

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

struct UIThread {
  const string game;
  vector<pair<uint8, uint8>> movie;
  // Snapshots enable fast rewinding. Sparse. The key is the index of
  // the frame that is next to be executed (so a key of 0 is the state
  // upon loading, without executing any frames).
  std::map<int64, vector<uint8>> snapshots;
  std::map<int64, string> subtitles;
  unique_ptr<Emulator> emu;
  int frameidx = 0;

  UIThread(const string &game,
	   const string &moviefile) : game(game) {
    vector<pair<int, string>> subs;
    movie = Util::endswith(moviefile, ".fm2") ?
      SimpleFM2::ReadInputsEx(moviefile, &subs) :
      SimpleFM7::ReadInputs2P(moviefile);
    
    CHECK(!movie.empty()) << "Couldn't read movie: " << moviefile;
    for (const auto &p : subs)
      subtitles[p.first] = p.second;

    emu.reset(Emulator::Create(game));
    CHECK(emu.get() != nullptr) << game;

    for (int i = 0; i < WARMUP_FRAMES; i++) {
      emu->StepFull(movie[i].first, movie[i].second);
      frameidx++;
    }
    printf("Warmed up.\n");
  }
  
  enum class Mode {
    PLAY,
    PAUSE,
    ADVANCE,
    FFWD,
  };
  Mode mode = Mode::PLAY;

  void DrawEmulatorAt(Emulator *e, int rot, int startx, int starty) {
    const vector<uint8> rgba = emu->GetImage();
    const vector<uint8> spritesheet = GetSpriteSheet(emu.get(), rot);
    const vector<uint8> oam = AutoCamera::OAM(emu.get());

    BlitRGBA2x(rgba, 256, 256, startx, starty, screen);
    BlitRGBA(spritesheet, 512, 16, startx, starty + 512 + 8, screen);

    font->draw(startx, 4, StringPrintf("%d^2/^<%d",
				       frameidx, (int)movie.size()));

    const uint32 xscroll = e->GetXScroll();
    const uint32 yscroll = e->GetYScroll();
    font->draw(startx + 128, 4,
	       StringPrintf("^2scroll: ^1%d^2,^<%d",
			    (int)xscroll, (int)yscroll));
    // And sprites.
    for (int absolute_s = 0; absolute_s < 64; absolute_s++) {
      int s = (absolute_s + rot) % 64;
      int sx = oam[s * 4 + 3];
      int sy = oam[s * 4 + 0];

      fontsmall->draw(startx + 2 * sx, starty + 2 * sy,
		      StringPrintf("%d", s));
    }

    const PPU *ppu = emu->GetFC()->ppu;
    for (int sl = 0; sl < 256; sl++) {
      uint8 xs = ppu->interframe_x[sl];
      uint8 ys = ppu->interframe_y[sl];
      // Given x,y as NES screen coordinates
      auto DrawPixel =
	[this, startx, starty](int x, int y,
			       uint8 r, uint8 g, uint8 b) {
	  SetPixelRGB(startx + x * 2, starty + y * 2,
		      r, g, b, screen);
	  SetPixelRGB(startx + x * 2 + 1, starty + y * 2,
		      r, g, b, screen);
	  SetPixelRGB(startx + x * 2, starty + y * 2 + 1,
		      r, g, b, screen);
	  SetPixelRGB(startx + x * 2 + 1, starty + y * 2 + 1,
		      r, g, b, screen);
	};
      DrawPixel(ys, sl, 0, 0xFF, 0);
      DrawPixel(xs, sl, 0xFF, 0, 0);
    }
  }
  
  void Loop() {
    SDL_Surface *surf = sdlutil::makesurface(256, 256, true);
    (void)surf;

    // XXX contra
    const int rotate = 45;
    
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

	  case SDLK_SPACE:
	    switch (mode) {
	    case Mode::FFWD:
	    case Mode::ADVANCE:
	    case Mode::PAUSE:
	      mode = Mode::PLAY;
	      break;
	    case Mode::PLAY:
	      mode = Mode::PAUSE;
	      break;
	    }
	    break;

	  case SDLK_PERIOD:
	    mode = Mode::ADVANCE;
	    break;
	    
	  case SDLK_MINUS:
	    // XLOC--;
	    YLOC--;
	    printf("%d,%d = %04x,%04x\n", XLOC, YLOC, XLOC, YLOC);
	    break;
	  case SDLK_EQUALS:
	  case SDLK_PLUS:
	    // XLOC++;
	    YLOC++;
	    printf("%d,%d = %04x,%04x\n", XLOC, YLOC, XLOC, YLOC);
	    break;
	    
	  // Numpad cardinal directions modify the value at XLOC,YLOC.
	  // XXX: Should probably discard snapshots when this
	  // happens?
	  case SDLK_KP6:
	  case SDLK_KP4: {
	    uint8 *ram = emu->GetFC()->fceu->RAM;
	    ram[XLOC] += event.key.keysym.sym == SDLK_KP6 ? 5 : -5;
	    printf("now %d,%d\n", ram[XLOC], ram[YLOC]);
	    break;
	  }

	  case SDLK_KP8:
	  case SDLK_KP2: {
	    uint8 *ram = emu->GetFC()->fceu->RAM;
	    ram[YLOC] += event.key.keysym.sym == SDLK_KP2 ? 5 : -5;
	    printf("now %d,%d\n", ram[XLOC], ram[YLOC]);
	    break;
	  }

	  case SDLK_2: {
	    AutoCamera2 ac{game};
	    vector<AutoCamera2::Linkage> links =
	      ac.FindLinkages(emu->SaveUncompressed(),
			      [](const string &s) {
				printf("AC2: %s\n", s.c_str());
			      });
	    if (!links.empty()) {
	      AutoCamera2::Linkage best = links[0];
	      printf("Set loc to %d,%d = 0x%2x,0x%2x (score %.2f)\n",
		     best.xloc, best.yloc,
		     best.xloc, best.yloc,
		     best.score);
	      XLOC = best.xloc;
	      YLOC = best.yloc;
	    }
	    break;
	  }
	    
	  case SDLK_c: {
	    AutoCamera autocamera{game};
	    bool exited = false;
	    auto Callback = [this, &exited](int depth,
					    int total_displacement,
					    Emulator *lemu,
					    Emulator *nemu,
					    Emulator *remu) {
	      sdlutil::clearsurface(screen, 0x44000044);

	      DrawEmulatorAt(lemu, total_displacement, 0, 0);
	      DrawEmulatorAt(nemu, total_displacement, 512 + 4, 0);
	      DrawEmulatorAt(remu, total_displacement, 1024 + 8, 0);

	      SDL_Flip(screen);
	      
	      if (exited) return;
	      for (;;) {
		SDL_Event event;
		if (SDL_PollEvent(&event)) {
		  switch (event.type) {
		  case SDL_QUIT:
		    exited = true;
		    return;
		  case SDL_KEYDOWN:
		    switch (event.key.keysym.sym) {
		    case SDLK_ESCAPE:
		      exited = true;
		      return;
		    case SDLK_SPACE:
		      return;
		    default:
		      break;
		    }
		    break;
		  default:
		    break;
		  }
		}
	      }
	    };
	    // using XYSprite = AutoCamera::XYSprite;
	    using XSprites = AutoCamera::XSprites;
	    const vector<uint8> save = emu->SaveUncompressed();
	    const XSprites xcand = autocamera.GetXSprites(save, Callback);
	    printf("-----\n");
	    mode = Mode::PAUSE;
	  }
	    
	  case SDLK_LEFT: {
	    mode = Mode::PAUSE;

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

	  case SDLK_RIGHT: {
	    auto it = snapshots.lower_bound(frameidx + 1);
	    if (it == snapshots.end()) {
	      mode = Mode::FFWD;
	      break;
	    } else {
	      // Have snapshot; just seek.
	      fprintf(stderr, "Seek to %lld\n", it->first);
	      frameidx = it->first;
	      emu->LoadUncompressed(it->second);

	      mode = Mode::PAUSE;
	      break;
	    }
	  }
	  default:
	    break;
	  }
	  break;
	default:
	  break;
	}
      }
	      
      if (mode == Mode::PLAY || mode == Mode::ADVANCE ||
	  mode == Mode::FFWD) {
	// Can we execute at least one frame?
	if (frameidx >= movie.size()) {
	  mode = Mode::PAUSE;
	} else {

	  uint8 p1, p2;
	  std::tie(p1, p2) = movie[frameidx];
	  frameidx++;
	  emu->StepFull(p1, p2);

	  if (0 == frameidx % SNAPSHOT_EVERY) {
	    snapshots[frameidx] = emu->SaveUncompressed();
	    if (mode == Mode::FFWD) {
	      mode = Mode::PAUSE;
	    }
	  }
	}

	if (mode == Mode::ADVANCE) {
	  mode = Mode::PAUSE;
	}
      }

      if (mode == Mode::FFWD)
	continue;
      
      // PERF: Don't need to clear the game part.
      sdlutil::clearsurface(screen, 0x00000000);
      
      const int rot = (frameidx * rotate) % 64;
      DrawEmulatorAt(emu.get(), rot, 0, 0);
	
      // Draw 
      if (mode == Mode::PLAY) {
	font->draw(0, HEIGHT - FONTHEIGHT, "^2PLAY");
      } else {
	font->draw(0, HEIGHT - FONTHEIGHT, "PAUSED");
      }

      {
	uint8 *ram = emu->GetFC()->fceu->RAM;
	const PPU *ppu = emu->GetFC()->ppu;
	
	// end-of-frame scroll values.
	const uint32 xscroll = emu->GetXScroll();
	const uint32 yscroll = emu->GetYScroll();

	// Find the single most-common scroll value across all
	// scanlines; arg should be 256 bytes (like
	// ppu->interframe_x).
	auto GetMajorityScroll =
	  [&](const uint8 *sls) {
	    uint8 counts[256] = {};
	    for (int i = 0; i < 240; i++) {
	      counts[sls[i]]++;
	    }
	    // Find max...
	    int maxi = 0, maxv = 0;
	    for (int i = 0; i < 256; i++) {
	      if (counts[i] > maxv) {
		maxi = i;
		maxv = counts[i];
	      }
	    }
	    return maxi;
	  };
	
	// TODO: get the appropriate offset based on the scanline.
	// We could use ram[YLOC] to determine the scanline, but
	// if yscroll isn't zero, then this isn't the scanline that
	// the sprite is actually drawn on. Would we need to find
	// a memory location that tells us the "game area" y scroll?
	
	//  ... so as a heuristic, use the most common scroll position
	// on-screen. This assumes that more than half of the screen
	// is the game area, which seems like a good assumption,
	// except perhaps for in split-screen two-player.
	const uint8 xmaj = GetMajorityScroll(ppu->interframe_x);
	const uint8 ymaj = GetMajorityScroll(ppu->interframe_y);
	
	const uint8 xx = ram[XLOC], yy = ram[YLOC];
	auto DrawBox =
	  [this](int x, int y, uint8 r, uint8 g, uint8 b) {
	    // shadow first
	    sdlutil::drawbox(screen, x * 2 + 2, y * 2 + 2, 15, 15, 0, 0, 0);
	    sdlutil::drawbox(screen, x * 2, y * 2, 16, 16, r, g, b);
	    sdlutil::drawbox(screen, x * 2 + 1, y * 2 + 1, 14, 14, r, g, b);
	  };
	// RED is unmodified coordinates.
	DrawBox(xx, yy, 0xFF, 0, 0);
	// GREEN is offset by scroll.
	DrawBox((uint8)(xx - xscroll), (uint8)(yy - yscroll), 0, 0xFF, 0);
	// BLUE is offset by majority scroll.
	DrawBox((uint8)(xx - xmaj), (uint8)(yy - ymaj), 0x33, 0x33, 0xFF);

	font->draw(400, 4,
		   StringPrintf("^4maj: ^1%d^2,^<%d",
				(int)xmaj, (int)ymaj));
      }
	
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

    fontsmall = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(fontsmall != nullptr) << "Couldn't load smallfont.";

    fontmax = Font::create(screen,
			   "fontmax.png",
			   FONTCHARS,
			   MAXFONTWIDTH, MAXFONTHEIGHT, FONTSTYLES, 4, 3);
    CHECK(fontmax != nullptr) << "Couldn't load fontmax.";
    
    Loop();
    printf("UI shutdown.\n");
  }

  ~UIThread() {
    // XXX free screen
    delete font;
    delete fontsmall;
    delete fontmax;
  }

 private:
  static constexpr int FONTWIDTH = 9;
  static constexpr int FONTHEIGHT = 16;
  static constexpr int SMALLFONTWIDTH = 6;
  static constexpr int SMALLFONTHEIGHT = 6;
  static constexpr int MAXFONTHEIGHT = 48 * 2;
  static constexpr int MAXFONTWIDTH = 27 * 2;

  Font *font = nullptr, *fontsmall = nullptr, *fontmax = nullptr;

  SDL_Surface *screen = nullptr;
};

// The main loop for SDL.
int main(int argc, char *argv[]) {
  (void)Rtos;

  string game, movie;
  if (argc < 3) {
    fprintf(stderr, "debug-autocamera.exe game.nes movie.fm2\n");
    // return -1;
    game = "contra.nes";
    movie = "contra2pwaterfall.fm2";
  } else {
    game = argv[1];
    movie = argv[2];
  }

  if (argc >= 5) {
    XLOC = atoi(argv[3]);
    YLOC = atoi(argv[4]);
  }
  
  #ifdef ENABLE_AOT
  if (game != "contra.nes") {
    fprintf(stderr, "Sorry, contra.nes is hardcoded because of AOT.\n");
    return -1;
  }
  #endif
  
  fprintf(stderr, "Init SDL\n");  
  
  /* Initialize SDL, video, and audio. */
  CHECK(SDL_Init(SDL_INIT_VIDEO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);
  
  SDL_Surface *icon = SDL_LoadBMP("debug-autocamera-icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  UIThread ui{game, movie};
  ui.Run();

  SDL_CloseAudio();
  SDL_Quit();
  return 0;
}
