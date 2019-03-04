
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"

#include "../chess.h"
#include "../pgn.h"
#include "../bigchess.h"
#include "timer.h"

#include "unblinder.h"
#include "unblinder-mk0.h"

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr, *chessfont = nullptr;
#define SCREENW 1920
#define SCREENH 1280
static SDL_Surface *screen = nullptr;

enum class Mode {
  BITMAP,
  CHESSBOARD,
};

namespace {
struct UI {
  Mode mode = Mode::BITMAP;
  bool ui_dirty = true, output_dirty = true;
  
  std::unique_ptr<Unblinder> unblinder;

  UI();
  void Loop();
  void Draw();
  
  uint64 current_bitmap = 0xFFFF00000000FFFFULL;
  Position current_prediction;
  // Position current_position;

  static constexpr int BITX = 260, BITY = 64, BITSCALE = 32;
  static constexpr int OUTX = 560, OUTY = 64, OUTSCALE = 32;
};
}  // namespace

UI::UI() {
  Timer model_timer;
  unblinder.reset(UnblinderMk0::LoadFromFile("net.val"));
  CHECK(unblinder.get() != nullptr);
  fprintf(stderr, "Loaded model in %.2fs\n",
	  model_timer.MS() / 1000.0);
  fflush(stderr);
}

void UI::Loop() {
  Mode mode = Mode::BITMAP;
  
  int mousex = 0, mousey = 0;
  (void)mousex; (void)mousey;
  for (;;) {

    SDL_Event event;
    if (SDL_PollEvent(&event)) {
      switch (event.type) {
      case SDL_QUIT:
	printf("QUIT.\n");
	return;

      case SDL_MOUSEMOTION: {
	SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;

	mousex = e->x;
	mousey = e->y;

	// If dragging piece, do it...
	break;
      }

      case SDL_KEYDOWN: {
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  printf("ESCAPE.\n");
	  return;
	default:;
	}
      }

      case SDL_MOUSEBUTTONDOWN: {
	// LMB/RMB, drag, etc.
	if (mode == Mode::BITMAP) {
	  int c = (mousex - BITX) / BITSCALE;
	  int r = (mousey - BITY) / BITSCALE;
	  if (c >= 0 && r >= 0 && c < 8 && r < 8) {
	    current_bitmap ^= (1ULL << (63 - (r * 8 + c)));
	    ui_dirty = true;
	    output_dirty = true;
	    printf("Bitmap: %llx\n", current_bitmap);
	    fflush(stdout);
	  }
	}
	break;
      }

      default:;
      }
    }
    if (output_dirty) {
      current_prediction = unblinder->Unblind(true, current_bitmap);
      // Could avoid this if the prediction didn't actually
      // change, but why?
      ui_dirty = true;
      output_dirty = false;
    }

    if (ui_dirty) {
      sdlutil::clearsurface(screen, 0xFFFFFFFF);
      Draw();
      SDL_Flip(screen);
      ui_dirty = false;
    }
  }
  
}

void UI::Draw() {
  font->draw(2, 2,
	     StringPrintf("Info: ^1%s", unblinder->ModelInfo().c_str()));

  switch (mode) {
  case Mode::BITMAP:

    for (int r = 0; r < 8; r++) {
      int yy = BITY + r * BITSCALE;
      for (int c = 0; c < 8; c++) {
	int xx = BITX + c * BITSCALE;
		  
	bool has = !!(current_bitmap & (1ULL << (63 - (r * 8 + c))));
	bool black = (r + c) & 1;
	// uint8 rr = black ? 134 : 255;
	// uint8 gg = black ? 166 : 255;
	// uint8 bb = black ? 102 : 221;
	uint8 rr = black ? 194 : 255;
	uint8 gg = black ? 226 : 255;
	uint8 bb = black ? 162 : 231;

	sdlutil::FillRectRGB(screen, xx, yy, BITSCALE, BITSCALE, rr, gg, bb);
	if (has) {
	  sdlutil::FillRectRGB(screen, xx + 3, yy + 3,
			       BITSCALE - 6, BITSCALE - 6, 22, 22, 22);
	  printf("* ");
	} else {
	  printf(". ");
	}
	
      }
      printf("\n");
    }
    fflush(stdout);
    break;

  case Mode::CHESSBOARD:
    font->draw(10, 10, "[Unimplemented]");
    break;
  }

  // Always draw output...
  
  for (int r = 0; r < 8; r++) {
    int yy = OUTY + r * OUTSCALE;
    for (int c = 0; c < 8; c++) {
      int xx = OUTX + c * OUTSCALE;

      uint8 piece = current_prediction.PieceAt(r, c);
      
      bool black = (r + c) & 1;
      // Background
      uint8 rr = black ? 134 : 255;
      uint8 gg = black ? 166 : 255;
      uint8 bb = black ? 102 : 221;

      sdlutil::FillRectRGB(screen, xx, yy, OUTSCALE, OUTSCALE, rr, gg, bb);
      string str = " ";
      if ((piece & Position::TYPE_MASK) == Position::EMPTY) {
	// already space
      } else {
	uint8 typ = piece & Position::TYPE_MASK;
	if (typ == Position::C_ROOK) typ = Position::ROOK;
	
	if ((piece & Position::COLOR_MASK) == Position::BLACK) {
	  str[0] = "?pnbrqk"[typ];
	} else {
	  str[0] = "?PNBRQK"[typ];
	}
      }
      chessfont->draw(xx, yy, str);

      if (mode == Mode::CHESSBOARD) {
	// TODO: Highlight mistakes when we have input board, not bits.
      }      
    }
  }

  // TODO: Show predicted move, castling...
  #if 0
  // Castling flags.
  for (int c = 0; c < 4; c++) {
    const uint8 v = FloatByte(stim.values[l][64 * NUM_CONTENTS + c]);
    sdlutil::FillRectRGB(screen, xstart + c * 32 + (c >= 2 ? 130 : 0),
			 ystart + 32 * 8, 30, 8, v, v, v);
    if (v < 127)
      sdlutil::drawbox(screen, xstart + c * 32 + (c >= 2 ? 130 : 0),
		       ystart + 32 * 8, 30, 8, 0xFF, 0xFF, 0xFF);
		
  }
	      
  // Whose move is it? 1.0 means black, so subtract from 255.
  const uint8 v = 255 - FloatByte(stim.values[l][64 * NUM_CONTENTS + 4]);
  sdlutil::FillRectRGB(screen, xstart, ystart + 32 * 8 + 8, 32 * 8, 8, v, v, v);
  if (v < 127)
    sdlutil::drawbox(screen, xstart, ystart + 32 * 8 + 8, 32 * 8, 8,
		     0xFF, 0xFF, 0xFF);
#endif
}

      
int main(int argc, char **argv) {

  // XXX This is specific to my machine. You probably want to remove it.
  // Assumes that processors 0-16 are available.
  // CHECK(SetProcessAffinityMask(GetCurrentProcess(), 0xF));

  /*
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  */

  /* Initialize SDL and network, if we're using it. */
  CHECK(SDL_Init(SDL_INIT_VIDEO |
		 SDL_INIT_TIMER |
		 SDL_INIT_AUDIO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  SDL_Surface *icon = SDL_LoadBMP("unblind-icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }
  
  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);

  font = Font::create(screen,
		      "font.png",
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";

  chessfont = Font::create(screen,
			   "chessfont.png",
			   " PNBRQKpnbrqk",
			   32, 32, 1, 0, 1);
  CHECK(font != nullptr) << "Couldn't load font.";

  UI ui;
  ui.Loop();
  
  SDL_Quit();
  return 0;
}

