
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <deque>
#include <unordered_map>
#include <unistd.h>
#include <cmath>

#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/sdl/cursor.h"
#include "../cc-lib/lines.h"
#include "../cc-lib/util.h"

// Fork?
#define FONTFILE "../chess/blind/font.png"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?"

#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr, *font2x = nullptr, *font4x = nullptr;

static SDL_Cursor *cursor_arrow = nullptr, *cursor_bucket = nullptr;
static SDL_Cursor *cursor_hand = nullptr, *cursor_hand_closed = nullptr;
static SDL_Cursor *cursor_eraser = nullptr;
#define VIDEOH 1080
#define STATUSH 128
#define SCREENW 1920
#define SCREENH (VIDEOH + STATUSH)

static SDL_Surface *screen = nullptr;

// Mode basically controls what happens when we use the mouse.
enum class Mode {
  PANNING,
};

struct UI {
  Mode mode = Mode::PANNING;
  bool ui_dirty = true;

  UI();
  void Loop();
  void DrawStatus();
  void Draw();

  uint32 current_color = 0xFF0000FF;
  // Single nybble in [1, 15]. 0 is disallowed.
  int current_alpha = 15;
  inline uint32 GetColor() const {
    uint8 aa = current_alpha | (current_alpha << 4);
    return (current_color & 0xFFFFFF) | (aa << 24);
  }

  SDL_Surface *drawing = nullptr;
  int mousex = 0, mousey = 0;

  bool dragging = false;
};

UI::UI() {
  drawing = sdlutil::makesurface(SCREENW, SCREENH, true);
  sdlutil::ClearSurface(drawing, 0, 0, 0, 0);
  CHECK(drawing != nullptr);
}

static void DrawThick(SDL_Surface *surf, int x0, int y0,
                      int x1, int y1,
                      Uint32 color) {
  static constexpr int THICKNESS = 3;
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

  auto ThickPixel = [&SetPixel](int x, int y) {
      static constexpr int LO = THICKNESS >> 1;
      // static constexpr int RO = THICKNESS - LO;

      for (int xx = x - LO; xx < x - LO + THICKNESS; xx++) {
        for (int yy = y - LO; yy < y - LO + THICKNESS; yy++) {
          SetPixel(xx, yy);
        }
      }
    };

  ThickPixel(x0, y0);

  for (const std::pair<int, int> point : Line<int>{x0, y0, x1, y1}) {
    const int x = point.first, y = point.second;
    ThickPixel(x, y);
  }
}

void UI::Loop() {
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
          switch (mode) {
          case Mode::PANNING:
            break;
          default:;
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

        case SDLK_HOME: {
	  // XXX set to maximum extents
          break;
        }
	  
        case SDLK_KP_PLUS:
        case SDLK_EQUALS:
        case SDLK_PLUS:
	  // XXX increase zoom
          break;

  
        case SDLK_KP_MINUS:
        case SDLK_MINUS:
	  // XXX decrease zoom
          break;
	  
        case SDLK_s: {
	  if (event.key.keysym.mod & KMOD_CTRL) {
	    sdlutil::SavePNG("drawing.png", drawing);
	    printf("Wrote drawing.png\n");
	    fflush(stdout);
	  } 
	  break;
        }

        default:;
        }
        break;
      }

      case SDL_MOUSEBUTTONDOWN: {
        // LMB/RMB, drag, etc.
        SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;
        mousex = e->x;
        mousey = e->y;

        dragging = true;
	SDL_SetCursor(cursor_hand_closed);
	// XXX set drag source, etc.
	
        break;
      }

      case SDL_MOUSEBUTTONUP: {
        // LMB/RMB, drag, etc.
        dragging = false;
	SDL_SetCursor(cursor_hand);
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
  const string modestring =
    "tips for using the program go here";
  font2x->draw(5, SCREENH - (FONTHEIGHT * 2) - 1, modestring);
}

static void DrawArrow(SDL_Surface *surf,
		      int x0, int y0, int x1, int y1,
		      Uint32 color) {
  // Main stem.
  DrawThick(surf, x0, y0, x1, y1, color);
  sdlutil::DrawCircle32(surf, x1, y1, 6, color);
}

void UI::Draw() {
  // Status stuff, always outside the 1920x1080 window.
  DrawStatus();

  // On-screen stuff
  // sdlutil::blitall(drawing, screen, 0, 0);
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

  SDL_Surface *icon = SDL_LoadBMP("icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);
  
  font = Font::create(screen,
		      FONTFILE,
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";
  
  font2x = Font::CreateX(2,
			 screen,
			 FONTFILE,
			 FONTCHARS,
			 FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font2x != nullptr) << "Couldn't load font.";

  font4x = Font::CreateX(4,
			 screen,
			 FONTFILE,
			 FONTCHARS,
			 FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font4x != nullptr) << "Couldn't load font.";

  CHECK((cursor_arrow = Cursor::MakeArrow()));
  CHECK((cursor_bucket = Cursor::MakeBucket()));
  CHECK((cursor_hand = Cursor::MakeHand()));
  CHECK((cursor_hand_closed = Cursor::MakeHandClosed()));
  CHECK((cursor_eraser = Cursor::MakeEraser()));

  SDL_SetCursor(cursor_arrow);
  SDL_ShowCursor(SDL_ENABLE);

  UI ui;
  ui.Loop();

  SDL_Quit();
  return 0;
}

