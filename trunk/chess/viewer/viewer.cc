
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <deque>

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
#include "../cc-lib/lines.h"

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

// Mode basically controls what happens when we use the mouse.
enum class Mode {
  DRAWING,
  CHESS,
};

namespace {
struct UI {
  // Mode mode = Mode::BITMAP;
  bool ui_dirty = true;
  
  UI();
  void Loop();
  void Draw();
  
  uint64 current_bitmap = 0xFFFF00000000FFFFULL;
  Position position;

  uint32 current_color = 0x77AA0000;
  
  SDL_Surface *drawing = nullptr;
  bool dragging = false;
  static constexpr int CHESSX = 64, CHESSY = 64, CHESSSCALE = 32 * 3;

  static constexpr int MAX_UNDO = 32;
  deque<SDL_Surface *> undo_buffer;
  deque<SDL_Surface *> redo_buffer;
};
}  // namespace

UI::UI() {
  drawing = sdlutil::makesurface(SCREENW, SCREENH, true);
  sdlutil::ClearSurface(drawing, 0, 0, 0, 0);
  CHECK(drawing != nullptr);
  Position::ParseFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		     &position);
}

static void DrawThick(SDL_Surface *surf, int x0, int y0,
		      int x1, int y1, 
		      Uint32 color) {  
  Line<int> l{x0, y0, x1, y1};

  const int w = surf->w, h = surf->h;
  
  Uint32 *bufp = (Uint32 *)surf->pixels;
  int stride = surf->pitch >> 2;
  auto SetPixel = [color, w, h, bufp, stride](int x, int y) {
      if (x >= 0 && y >= 0 &&
	  x < w && y < h) {
	bufp[y * stride + x] = color;
      }
    };

  {
    fflush(stdout);
    SetPixel(x0, y0);
    SetPixel(x0 + 1, y0);
    SetPixel(x0, y0 + 1);
    SetPixel(x0 + 1, y0 + 1);
  }
  
  for (const std::pair<int, int> point : Line<int>{x0, y0, x1, y1}) {
    const int x = point.first, y = point.second;
    SetPixel(x, y);
    SetPixel(x + 1, y);
    SetPixel(x, y + 1);
    SetPixel(x + 1, y + 1);
  }
}


void UI::Loop() {
  Mode mode = Mode::DRAWING;
  
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

	const int oldx = mousex, oldy = mousey;
	
	mousex = e->x;
	mousey = e->y;

	if (dragging) {
	  if (mode == Mode::DRAWING) {
	    DrawThick(drawing, oldx, oldy, mousex, mousey, current_color);
	  }
	  ui_dirty = true;
	}
	break;
      }

      case SDL_KEYDOWN: {
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  printf("ESCAPE.\n");
	  return;
	  
	case SDLK_c:
	  sdlutil::clearsurface(drawing, 0x0);
	  ui_dirty = true;
	  break;

	case SDLK_z: {
	  // XXX check ctrl?
	  if (!undo_buffer.empty()) {
	    redo_buffer.push_front(drawing);
	    drawing = undo_buffer.back();
	    undo_buffer.pop_back();
	    ui_dirty = true;
	    printf("Undo size %d\n", undo_buffer.size());
	    fflush(stdout);
	  }
	  break;
	}

	case SDLK_y: {
	  // XXX check ctrl?
	  if (!redo_buffer.empty()) {
	    undo_buffer.push_back(drawing);
	    drawing = redo_buffer.front();
	    redo_buffer.pop_front();
	    ui_dirty = true;
	  }
	  break;
	}
	  
	default:;
	}
	break;
      }

      case SDL_MOUSEBUTTONDOWN: {
	// LMB/RMB, drag, etc.
	dragging = true;
	if (mode == Mode::DRAWING) {
	  undo_buffer.push_back(sdlutil::duplicate(drawing));
	  while (undo_buffer.size() > MAX_UNDO) {
	    SDL_FreeSurface(undo_buffer.front());
	    undo_buffer.pop_front();
	  }

	  while (!redo_buffer.empty()) {
	    SDL_FreeSurface(redo_buffer.front());
	    redo_buffer.pop_front();
	  }

	  fflush(stdout);
	}

	if (mode == Mode::DRAWING) {
	  // Make sure that a click also makes a pixel.
	  SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;
	  
	  mousex = e->x;
	  mousey = e->y;

	  DrawThick(drawing, mousex, mousey, mousex, mousey, current_color);
	  ui_dirty = true;
	}
	
	break;
      }

      case SDL_MOUSEBUTTONUP: {
	// LMB/RMB, drag, etc.
	dragging = false;	
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
    int yy = CHESSY + r * CHESSSCALE;
    for (int c = 0; c < 8; c++) {
      int xx = CHESSX + c * CHESSSCALE;

      uint8 piece = position.PieceAt(r, c);
      
      bool black = (r + c) & 1;
      // Background
      uint8 rr = black ? 134 : 255;
      uint8 gg = black ? 166 : 255;
      uint8 bb = black ? 102 : 221;

      sdlutil::FillRectRGB(screen, xx, yy, CHESSSCALE, CHESSSCALE, rr, gg, bb);
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

  sdlutil::blitall(drawing, screen, 0, 0);
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

