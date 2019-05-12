
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
#include "../cc-lib/sdl/cursor.h"
#include "../cc-lib/lines.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr, *font2x = nullptr;
static Font *chessfont = nullptr, *chessfont3x = nullptr;

static SDL_Cursor *cursor_arrow = nullptr, *cursor_bucket = nullptr;
#define VIDEOH 1080
#define STATUSH 128
#define SCREENW 1920
#define SCREENH (VIDEOH + STATUSH)

static SDL_Surface *screen = nullptr;

// Mode basically controls what happens when we use the mouse.
enum class Mode {
  DRAWING,
  FILLING,
  CHESS,
};

namespace {
struct UI {
  Mode mode = Mode::DRAWING;
  bool ui_dirty = true;
  
  UI();
  void Loop();
  void DrawStatus();
  void Draw();
  
  void SaveUndo() {
    undo_buffer.push_back(sdlutil::duplicate(drawing));
    while (undo_buffer.size() > MAX_UNDO) {
      SDL_FreeSurface(undo_buffer.front());
      undo_buffer.pop_front();
    }

    while (!redo_buffer.empty()) {
      SDL_FreeSurface(redo_buffer.front());
      redo_buffer.pop_front();
    }
  }

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

static void FloodFill(SDL_Surface *surf, int x, int y,
		      Uint32 color) {
  const int w = surf->w, h = surf->h;
  
  Uint32 *bufp = (Uint32 *)surf->pixels;
  int stride = surf->pitch >> 2;

  const Uint32 replace_color = bufp[y * stride + x];
  
  auto GetPixel = [w, h, bufp, stride, replace_color](int x, int y) {
      if (x >= 0 && y >= 0 &&
	  x < w && y < h) {
	return bufp[y * stride + x];
      } else {
	return ~replace_color;
      }
    };

  auto SetPixel = [color, w, h, bufp, stride](int x, int y) {
      if (x >= 0 && y >= 0 &&
	  x < w && y < h) {
	bufp[y * stride + x] = color;
      }
    };

  std::vector<std::pair<int, int>> todo;
  if (color != replace_color)
    todo.emplace_back(x, y);

  while (!todo.empty()) {
    int xx, yy;
    std::tie(xx, yy) = todo.back();
    todo.pop_back();

    Uint32 c = GetPixel(xx, yy);
    if (c == replace_color) {
      SetPixel(xx, yy);
      todo.emplace_back(xx - 1, yy);
      todo.emplace_back(xx + 1, yy);
      todo.emplace_back(xx, yy - 1);
      todo.emplace_back(xx, yy + 1);
    }
  }
}


void UI::Loop() {
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

	case SDLK_d: {
	  mode = Mode::DRAWING;
	  SDL_SetCursor(cursor_arrow);
	  ui_dirty = true;
	  break;
	}

	case SDLK_f: {
	  mode = Mode::FILLING;
	  SDL_SetCursor(cursor_bucket);
	  ui_dirty = true;
	  break;
	}
	  
	case SDLK_c: {
	  mode = Mode::CHESS;
	  SDL_SetCursor(cursor_arrow); // XXX hand
	  ui_dirty = true;
	  break;
	}

	case SDLK_q: {
	  SaveUndo();
	  sdlutil::clearsurface(drawing, 0x0);
	  ui_dirty = true;
	  break;
	}

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
	  SaveUndo();

	  // Make sure that a click also makes a pixel.
	  SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;
	  
	  mousex = e->x;
	  mousey = e->y;

	  DrawThick(drawing, mousex, mousey, mousex, mousey, current_color);
	  ui_dirty = true;
	} else if (mode == Mode::FILLING) {
	  SaveUndo();

	  // Make sure that a click also makes a pixel.
	  SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;
	  
	  mousex = e->x;
	  mousey = e->y;

	  FloodFill(drawing, mousex, mousey, current_color);
	  
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

void UI::DrawStatus() {
  int drawcolor = 1;
  int chesscolor = 1;
  int fillcolor = 1;
  
  switch (mode) {
  case Mode::DRAWING:
    drawcolor = 2;
    break;

  case Mode::FILLING:
    fillcolor = 2;
    break;
    
  case Mode::CHESS:
    chesscolor = 2;
    break;
    
  }
  string modestring =
    StringPrintf("[^3D^<]^%draw^<  [^3F^<]^%dill^<  [^3C^<]^%dhess^<  ...",
		 drawcolor, fillcolor, chesscolor);
  font2x->draw(5, SCREENH - (FONTHEIGHT * 2) - 1, modestring);


  
}

void UI::Draw() {
  // Status stuff
  DrawStatus();
  
  // On-screen stuff
  
  for (int r = 0; r < 8; r++) {
    const int yy = CHESSY + r * CHESSSCALE;
    for (int c = 0; c < 8; c++) {
      const int xx = CHESSX + c * CHESSSCALE;

      uint8 piece = position.PieceAt(r, c);
      
      bool black = (r + c) & 1;
      // Background
      uint8 rr = black ? 134 : 255;
      uint8 gg = black ? 166 : 255;
      uint8 bb = black ? 102 : 221;

      sdlutil::FillRectRGB(screen, xx, yy, CHESSSCALE, CHESSSCALE, rr, gg, bb);
      if ((piece & Position::TYPE_MASK) == Position::EMPTY) {
	// already space
      } else {
	string str = " ";

	uint8 typ = piece & Position::TYPE_MASK;
	if (typ == Position::C_ROOK) typ = Position::ROOK;
	
	if ((piece & Position::COLOR_MASK) == Position::BLACK) {
	  str[0] = "?pnbrqk"[typ];
	} else {
	  str[0] = "?PNBRQK"[typ];
	}
	chessfont3x->draw(xx, yy + 8, str);
      }
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

  font2x = Font::CreateX(2,
			 screen,
			 "../blind/font.png",
			 FONTCHARS,
			 FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font2x != nullptr) << "Couldn't load font.";
  
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

  CHECK((cursor_arrow = Cursor::MakeArrow()));
  CHECK((cursor_bucket = Cursor::MakeBucket()));

  SDL_SetCursor(cursor_arrow);
  SDL_ShowCursor(SDL_ENABLE);
  
  UI ui;
  ui.Loop();
  
  SDL_Quit();
  return 0;
}

