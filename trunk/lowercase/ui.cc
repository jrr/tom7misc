
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>

#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "timer.h"

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/stb_truetype.h"
#include "../cc-lib/image.h"
#include "../cc-lib/util.h"
#include "../cc-lib/lines.h"

#include "ttf.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;

#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr;
#define SCREENW 1920
#define SCREENH 1280
static SDL_Surface *screen = nullptr;

enum class Mode {
  BITMAP,
};


namespace {
struct UI {
  Mode mode = Mode::BITMAP;
  bool ui_dirty = true;

  char current_char = 'a';
  
  UI();
  void Loop();
  void Draw();
  
  TTF times{"times.ttf"};
};
}  // namespace

UI::UI() {

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

	// Do dragging...
	break;
      }

      case SDL_KEYDOWN: {
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  printf("ESCAPE.\n");
	  return;
	case SDLK_LEFT:
	  if (current_char > ' ') current_char--;
	  ui_dirty = true;
	  break;
	case SDLK_RIGHT:
	  if (current_char < '~') current_char++;
	  ui_dirty = true;
	  break;
	default:;
	}
      }

      case SDL_MOUSEBUTTONDOWN: {
	// LMB/RMB, drag, etc.
	if (mode == Mode::BITMAP) {
	  // ...
	}
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
  font->draw(2, 2, "hi");

  switch (mode) {
  case Mode::BITMAP:

    vector<TTF::Contour> contours = times.GetContours(current_char);

    constexpr int XPOS = 128;
    constexpr int YPOS = 48;
    // Basically, the height of the font in pixels.
    constexpr int SCALE = 1000;
    auto Line = [&](float x1, float y1, float x2, float y2, uint32 color) {
	sdlutil::drawclipline(screen,
			      XPOS + x1 * SCALE, YPOS + y1 * SCALE,
			      XPOS + x2 * SCALE, YPOS + y2 * SCALE,
			      0xFF & (color >> 24),
			      0xFF & (color >> 16),
			      0xFF & (color >> 8));
      };

    // One screen pixel in normalized coordinates.
    double sqerr = 1.0f / (SCALE * SCALE);
    
    for (const auto &contour : contours) {
      auto [x, y] = times.Norm(contour.startx, contour.starty);
      printf("Contour:\n"
	     "start %d,%d (norm %.4f %.4f)\n", contour.startx, contour.starty,
	     x, y);
      for (const auto &p : contour.paths) {
	switch (p.type) {
	case TTF::PathType::LINE: {
	  // printf("  lineto %d,%d\n", p.x, p.y);
	  auto [px, py] = times.Norm(p.x, p.y);
	  Line(x, y, px, py, 0x000000FF);
	  x = px;
	  y = py;
	  break;
	}
	case TTF::PathType::BEZIER: {
	  // printf("  bezier %d,%d (%d, %d)\n", p.x, p.y, p.cx, p.cy);
	  auto [cx, cy] = times.Norm(p.cx, p.cy);
	  auto [px, py] = times.Norm(p.x, p.y);
	  // Line(x, y, cx, cy, 0x00FF00FF);
	  // Line(cx, cy, px, py, 0x0000FFFF);

	  for (const auto [xx, yy] : 
		 TesselateQuadraticBezier<double>(x, y, cx, cy, px, py, sqerr)) {
	    // times.Norm(1, 1.0f / 1000.0f).second)) {
	    Line(x, y, xx, yy, 0xFF0000FF);
	    x = xx;
	    y = yy;
	  }
	  break;
	}
	}
      }
    }
    
    break;
  }

  // Always draw output...
  
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

  SDL_Surface *icon = SDL_LoadBMP("lowercase-icon.bmp");
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

  UI ui;
  ui.Loop();
  
  SDL_Quit();
  return 0;
}

