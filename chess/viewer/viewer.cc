
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

// #include "unblinder.h"
// #include "unblinder-mk0.h"

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
static Font *font = nullptr, *chessfont = nullptr, *chessfont3x = nullptr;
#define SCREENW 1920
#define SCREENH 1080
static SDL_Surface *screen = nullptr;

enum class Mode {
  BITMAP,
  CHESSBOARD,
};

namespace {
struct UI {
  // Mode mode = Mode::BITMAP;
  bool ui_dirty = true, output_dirty = true;
  
  UI();
  void Loop();
  void Draw();
  
  uint64 current_bitmap = 0xFFFF00000000FFFFULL;
  Position current_prediction;
  // Position current_position;

  static constexpr int BITX = 260, BITY = 64, BITSCALE = 32 * 3;
  static constexpr int OUTX = 560, OUTY = 64, OUTSCALE = 32 * 3;
};
}  // namespace

UI::UI() {
  Position::ParseFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		     &current_prediction);
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
	break;
      }

      default:;
      }
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
      chessfont3x->draw(xx, yy + 8, str);
    }
  }
}

      
int main(int argc, char **argv) {
  /* Initialize SDL and network, if we're using it. */
  CHECK(SDL_Init(SDL_INIT_VIDEO |
		 SDL_INIT_TIMER |
		 SDL_INIT_AUDIO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  SDL_Surface *icon = SDL_LoadBMP("../blind/unblind-icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }
  
  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);

  font = Font::create(screen,
		      "../blind/font.png",
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";

  chessfont = Font::create(screen,
			   "../blind/chessfont.png",
			   " PNBRQKpnbrqk",
			   32, 32, 1, 0, 1);
  CHECK(chessfont != nullptr) << "Couldn't load chessfont.";

  chessfont3x = Font::CreateX(3,
			      screen,
			      "../blind/chessfont.png",
			      " PNBRQKpnbrqk",
			      32, 32, 1, 0, 1);
  CHECK(chessfont3x != nullptr) << "Couldn't load chessfont3x.";

  UI ui;
  ui.Loop();
  
  SDL_Quit();
  return 0;
}

