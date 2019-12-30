
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <deque>
#include <unordered_map>
#include <unistd.h>
#include <cmath>

#include "../cc-lib/threadutil.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/sdl/cursor.h"
#include "../cc-lib/lines.h"
#include "../cc-lib/util.h"
#include "../cc-lib/re2/re2.h"
#include "../cc-lib/bounds.h"

// Fork?
#define FONTFILE "../chess/blind/font.png"
#define FONTSMALLFILE "fontsmall.png"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?"

#define FONTSTYLES 7

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

static constexpr int FONTWIDTH = 9;
static constexpr int FONTHEIGHT = 16;
static Font *font = nullptr, *font2x = nullptr, *font4x = nullptr;
static constexpr int FONTSMALLWIDTH = 6;
static constexpr int FONTSMALLHEIGHT = 6;
static Font *fontsmall = nullptr;

static SDL_Cursor *cursor_arrow = nullptr, *cursor_bucket = nullptr;
static SDL_Cursor *cursor_hand = nullptr, *cursor_hand_closed = nullptr;
static SDL_Cursor *cursor_eraser = nullptr;
#define VIDEOH 1080
#define STATUSH 128
#define SCREENW 1920
#define SCREENH (VIDEOH + STATUSH)

static constexpr double ZOOM_FACTOR = 0.15;

static SDL_Surface *screen = nullptr;

struct TraceRow {
  uint64 timestamp;
  bool on[6];
  int edges[6];
  int since[6];
};

static vector<TraceRow> ReadRows(const string &filename) {
  vector<string> lines = Util::ReadFileToLines(filename);

  RE2 number("([0-9]+),?");

  vector<TraceRow> rows;
  rows.reserve(lines.size());

  // skip first line, which is header.
  for (int r = 1; r < lines.size(); r++) {
    re2::StringPiece line(lines[r]);
    TraceRow row;
    if (!RE2::Consume(&line, number, &row.timestamp))
      break;

    for (int i = 0; i < 6; i++) {
      int on;
      CHECK(RE2::Consume(&line, number, &on)) << line;
      row.on[i] = on != 0;
    }

    for (int i = 0; i < 6; i++)
      CHECK(RE2::Consume(&line, number, &row.edges[i])) << line;

    for (int i = 0; i < 6; i++)
      CHECK(RE2::Consume(&line, number, &row.since[i])) << line;
    rows.push_back(row);
  }
  return rows;
}

static vector<TraceRow> the_rows;

double ExtractOn(const TraceRow &row, int i) {
  return row.on[i] ? 1.0 : 0.0;
}
double ExtractEdges(const TraceRow &row, int i) {
  return (double)row.edges[i];
}
double ExtractSince(const TraceRow &row, int i) {
  return (double)row.since[i];
}

struct Series {
  string name;
  double (*Extract)(const TraceRow &, int);
};

vector<Series> serieses;
Bounds max_bounds;
static int pan_x = 0, pan_y = 0;
static double zoom_x = 1.0, zoom_y = 1.0;

Bounds::Scaler GetScaler() {
  Bounds::Scaler scaler =
    max_bounds.
    Stretch(SCREENW, VIDEOH).
    FlipY().
    Zoom(zoom_x, zoom_y).
    PanScreen(pan_x, pan_y);

  return scaler;
}

static void InitBounds(const Series &series, Bounds *bounds) {
  for (const TraceRow &row : the_rows) {
    for (int i = 0; i < 6; i++) {
      bounds->Bound((double)row.timestamp, series.Extract(row, i));
    }
  }
}
static void InitSeries() {
#if 0
  Series edges;
  edges.name = "edges";
  edges.Extract = ExtractEdges;
  InitBounds(edges, &max_bounds);
  
  serieses = {edges};
#else
  Series since;
  since.name = "since";
  since.Extract = ExtractSince;
  InitBounds(since, &max_bounds);
  
  serieses = {since};
#endif
}

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
  int start_pan_x = 0, start_pan_y = 0;
  int drag_x = 0, drag_y = 0;
};

UI::UI() {
  drawing = sdlutil::makesurface(SCREENW, SCREENH, true);
  sdlutil::ClearSurface(drawing, 0, 0, 0, 0);
  CHECK(drawing != nullptr);
}

static void DrawThickLine(SDL_Surface *surf, int x0, int y0,
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

static void DrawLine(SDL_Surface *surf, int x0, int y0,
		     int x1, int y1,
		     Uint32 color) {
  // PERF: when zooming, many lines will be completely off screen.
  // When clipping, have to be careful about the case that both
  // endpoints are offscreen, but the line crosses a corner.

  Line<int> l{x0, y0, x1, y1};

  const int w = surf->w, h = surf->h;

  Uint32 *bufp = (Uint32 *)surf->pixels;
  int stride = surf->pitch >> 2;
#if 1
  auto SetPixel = [color, w, h, bufp, stride](int x, int y) {
      if (x >= 0 && y >= 0 &&
          x < w && y < h) {
        bufp[y * stride + x] = color;
      }
    };
#else
  // Always blends 50/50.
  auto SetPixel = [surf, color, w, h, bufp, stride](int x, int y) {
      if (x >= 0 && y >= 0 &&
          x < w && y < h) {
	uint32 prev = bufp[y * stride + x];
        bufp[y * stride + x] = sdlutil::Mix2(surf, color, prev);
      }
    };
#endif
  
  
  SetPixel(x0, y0);

  for (const std::pair<int, int> point : Line<int>{x0, y0, x1, y1}) {
    const int x = point.first, y = point.second;
    SetPixel(x, y);
  }
}

void UI::Loop() {
  for (;;) {

    // Zoom by the given amount, but then adjust the pan so that the
    // point under the cursor stays in the same place.
    auto ZoomBy = [this](double zx, double zy) {
	// Position in absolute coordinates where the mouse currently
	// points.
	Bounds::Scaler old_scaler = GetScaler();
	double amx = old_scaler.UnscaleX(mousex);
	double amy = old_scaler.UnscaleY(mousey);
	
	zoom_x += zx;
	zoom_y += zy;

	Bounds::Scaler new_scaler = GetScaler();
	double nsmx = new_scaler.ScaleX(amx);
	double nsmy = new_scaler.ScaleY(amy);

	/*
	printf("mouse %d,%d = abs %.2f, %.2f now %.2f,%.2f\n",
	       mousex, mousey, amx, amy, nsmx, nsmy);
	fflush(stdout);
	*/
	
	// So, the pixel that used to be at mousex,mousey is now
	// at nsmx,nsmy. Pan to put it at mousex,mousey by subtracting
	// the "error".
	pan_x -= (nsmx - mousex);
	pan_y -= (nsmy - mousey);
      };
    
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

        if (dragging) {
          switch (mode) {
          case Mode::PANNING:
	    pan_x = start_pan_x + (mousex - drag_x);
	    pan_y = start_pan_y + (mousey - drag_y);
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
	  pan_x = 0;
	  pan_y = 0;
	  zoom_x = 1.0;
	  zoom_y = 1.0;
          break;
        }
          
        case SDLK_KP_PLUS:
        case SDLK_EQUALS:
        case SDLK_PLUS:
	  ZoomBy(ZOOM_FACTOR, ZOOM_FACTOR);
          break;

  
        case SDLK_KP_MINUS:
        case SDLK_MINUS:
	  ZoomBy(-ZOOM_FACTOR, -ZOOM_FACTOR);
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
        SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;
        mousex = e->x;
        mousey = e->y;

	switch (e->button) {
	case SDL_BUTTON_WHEELUP:
	  ZoomBy(ZOOM_FACTOR, ZOOM_FACTOR);
	  break;
	case SDL_BUTTON_WHEELDOWN:
	  ZoomBy(-ZOOM_FACTOR, -ZOOM_FACTOR);
	  break;
	case SDL_BUTTON_LEFT:
	  dragging = true;
	  start_pan_x = pan_x;
	  start_pan_y = pan_y;
	  drag_x = mousex;
	  drag_y = mousey;
	  SDL_SetCursor(cursor_hand_closed);
	  break;
	}
        
        break;
      }

      case SDL_MOUSEBUTTONUP: {
        // LMB/RMB, drag, etc.
	SDL_MouseButtonEvent *e = (SDL_MouseButtonEvent*)&event;
        mousex = e->x;
        mousey = e->y;

	if (e->button == SDL_BUTTON_LEFT) {
	  dragging = false;
	  SDL_SetCursor(cursor_hand);
	  pan_x = start_pan_x + (mousex - drag_x);
	  pan_y = start_pan_y + (mousey - drag_y);
	}
	break;
      }

      default:;
      }
    }

    // if (ui_dirty) {
      sdlutil::clearsurface(screen, 0xFFFFFFFF);
      Draw();
      SDL_Flip(screen);
      // ui_dirty = false;
      // }
  }
}

void UI::DrawStatus() {
  Bounds::Scaler scaler = GetScaler();
  double ox, oy;
  std::tie(ox, oy) = scaler.Unscale({mousex, mousey});
  const string modestring =
    StringPrintf("mouse %.4f,%.4f", ox, oy);
  font2x->draw(5, SCREENH - (FONTHEIGHT * 2) - 1, modestring);
}

static void DrawArrow(SDL_Surface *surf,
                      int x0, int y0, int x1, int y1,
                      Uint32 color) {
  // Main stem.
  DrawThickLine(surf, x0, y0, x1, y1, color);
  sdlutil::DrawCircle32(surf, x1, y1, 6, color);
}

static constexpr uint32 COLORS[] = {
  /* 3 */ 0xFFbd3838, // R
  /* 4 */ 0xFF0ba112, // G
  /* 5 */ 0xFF1664CE, // B
  /* 6 */ 0xFFf943ea, // magenta
  /* 8 */ 0xFF29edef, // cyan
  /* 9 */ 0xFFc77e00, // orange/brown
  /* 7 */ 0xFFeef943, // yellow
  /* 0 */ 0xFF7500c7, // purple
};

void UI::Draw() {
  // Status stuff, always outside the 1920x1080 window.
  DrawStatus();

  static constexpr int GRID_LINES = 12;
  static constexpr uint32 GRID_COLOR = 0xFFDDDDDD;

  Bounds::Scaler scaler = GetScaler();
  // Horizontal grid lines
  for (int i = 0; i < GRID_LINES; i++) {
    int y = (VIDEOH / (double)GRID_LINES) * (i + 1);
    DrawLine(screen, 0, y, SCREENW - 1, y, GRID_COLOR);
    font->draw(6, y - FONTHEIGHT - 1,
	       StringPrintf("^6%.4f",
			    scaler.UnscaleY(y)));
  }

  // Vertical
  for (int i = 0; i < GRID_LINES; i++) {
    int x = (SCREENW / (double)GRID_LINES) * (i + 1);
    DrawLine(screen, x, 0, x, SCREENH - 1, GRID_COLOR);
    font->draw(x + 2, VIDEOH - FONTHEIGHT - 1,
	       StringPrintf("^6%.4f",
			    scaler.UnscaleX(x)));
  }
  
  // Draw lines.
  for (const Series &series : serieses) {
    for (int i = 0; i < 6; i++) {
      uint32 color = COLORS[i];

      double prev_x = scaler.ScaleX(the_rows[0].timestamp);
      double prev_y = scaler.ScaleY(series.Extract(the_rows[0], i));

      for (int r = 1; r < the_rows.size(); r++) {
	const TraceRow &row = the_rows[r];
	double x = scaler.ScaleX(row.timestamp);
	double y = scaler.ScaleY(series.Extract(row, i));

	DrawLine(screen, prev_x, prev_y, x, y, color);
	
	prev_x = x;
	prev_y = y;
      }
    }
  }
  
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

  fontsmall = Font::create(screen,
			   FONTSMALLFILE,
			   FONTCHARS,
			   FONTSMALLWIDTH,
			   FONTSMALLHEIGHT, FONTSTYLES, 0, 3);
  CHECK(fontsmall != nullptr) << "Couldn't load fontsmall.";
  
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

  the_rows = ReadRows("trace.csv");
  printf("Read %lld rows\n", (int64)the_rows.size());

  InitSeries();
  printf("Bounds %f,%f -> %f,%f\n",
	 max_bounds.MinX(), max_bounds.MinY(),
	 max_bounds.MaxX(), max_bounds.MaxY());
  fflush(stdout);
  
  UI ui;
  ui.Loop();

  SDL_Quit();
  return 0;
}

