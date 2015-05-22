
// Just for LoadLibrary. Maybe could use an extern decl.
#if 0
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <vector>
#include <utility>
#include <tuple>
#include <cstdint>

#include "SDL.h"
#include "sdl/sdlutil.h"
#include "math.h"

using namespace std;

using uint32 = uint32_t;
using uint64 = uint64_t;
using uint16 = uint16_t;
using uint8 = uint8_t;
using int64 = int64_t;
using int32 = int32_t;

// We use these interchangeably; ensure that they are consistent.
// The S* and U* versions come from SDL, the simple ones from base.h.
// (TODO: Is there a way to check that the types are literally
// the same?)
static_assert(sizeof(Sint64) == sizeof(int64), "int 64");
static_assert(sizeof(Uint64) == sizeof(uint64), "uint 64");
static_assert(sizeof(Sint32) == sizeof(int32), "int 32");
static_assert(sizeof(Uint32) == sizeof(uint32), "uint 32");
static_assert(sizeof(Uint16) == sizeof(uint16), "uint 16");
static_assert(sizeof(Uint8) == sizeof(uint8), "uint 8");

#define PI 3.14159265358979323846264338327950288419716939937510
#define TWOPI (2.0 * PI) // 6.28318530718


#define STARTW 1920
#define STARTH 1080

// TODO: Make into little library. Make constexpr mixcolor that's
// correct for the platform.
static constexpr uint32 RGBA(uint8 r, uint8 g, uint8 b, uint8 a) {
  return (a << 24) |
    (r << 16) |
    (g << 8) |
    b;
}
static constexpr uint32 RED = RGBA(0xFF, 0x0, 0x0, 0xFF);
static constexpr uint32 BLACK = RGBA(0x0, 0x0, 0x0, 0xFF);
static constexpr uint32 WHITE = RGBA(0xFF, 0xFF, 0xFF, 0xFF);
static constexpr uint32 GREEN = RGBA(0x0, 0xFF, 0x0, 0xFF);
static constexpr uint32 BLUE = RGBA(0x0, 0x0, 0xFF, 0xFF);
static constexpr uint32 BACKGROUND = RGBA(0x00, 0x20, 0x00, 0xFF);

// XXX whither?
static SDL_Surface *screen;

int main(int argc, char **argv) {

  if (SDL_Init (SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_TIMER) < 0) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
    abort();
  }

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, 
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);
  screen = sdlutil::makescreen(STARTW, STARTH);
  sdlutil::clearsurface(screen, BACKGROUND); 
  SDL_Flip(screen);

  // All angles in radians, distances in "units".

  // Diameter of big base gear.
  double sun_dia = 6.0;
  // Diameter of big nested gear.
  double earth_dia = 3.9;
  // Center of earth to center of sun.
  double orbit = 2.413;
  // Distance from earth's center to its wand's tip.
  double wand_length = orbit;

  // XXX make accurate
  double earth_gear_ratio = 4.0;
  const double earthdriver_dia = earth_dia / earth_gear_ratio;

  // Externally driven. Sun is actually driven by a little gear,
  // but we can pretend it's driven directly. The earth's driver
  // has an interaction with the sun, however.
  //
  // Both in mechanism-space.
       double sun_angle = 0.0;
  double earthdriver_angle = 0.0;

  struct Configuration {
    double earth_x = 0.0, earth_y = 0.0;
    double earth_angle = 0.0;
    double wand_x = 0.0, wand_y = 0.0;
  };

  auto Compute = [sun_dia, earth_dia, orbit, wand_length,
		  earth_gear_ratio, earthdriver_dia]
    (double sun_a, double ed_a) -> Configuration {

    Configuration c;

    // Earth's center.
    c.earth_x = sin(sun_a) * orbit;
    c.earth_y = cos(sun_a) * orbit;

    // The earth driver and earth would be in a normal gearing
    // relationship, but the earth orbits the earth driver as well
    // (because it is on the sun), causing a kind of phantom rotation.
    const double effective_angle = ed_a - sun_a;
    
    c.earth_angle = effective_angle / -earth_gear_ratio;

    c.wand_x = sin(c.earth_angle) * wand_length + c.earth_x;
    c.wand_y = cos(c.earth_angle) * wand_length + c.earth_y;

    return c;
  };


  auto ToScreen = [](double x, double y) {
    static constexpr int ORIGIN_X = STARTW >> 1;
    static constexpr int ORIGIN_Y = STARTH >> 1;
    static constexpr double PIXELS_PER_UNIT = STARTH / 12.0;
    return make_pair(x * PIXELS_PER_UNIT + ORIGIN_X,
		     y * PIXELS_PER_UNIT + ORIGIN_Y);
  };

  // All of these draw functions take mechanism-space coordinates.
  auto DrawLine = [&ToScreen](double x0, double y0, double x1, double y1,
			      uint32 color = WHITE) {
    int sx0, sy0, sx1, sy1;
    std::tie(sx0, sy0) = ToScreen(x0, y0);
    std::tie(sx1, sy1) = ToScreen(x1, y1);
    sdlutil::drawclipline(screen, sx0, sy0, sx1, sy1, 
			  (color >> 24) & 255,
			  (color >> 16) & 255,
			  (color >> 8) & 255);
  };

  auto DrawGear = [&DrawLine](double cx, double cy, double radius, 
			      double angle, int sides) {
    double radians_per_edge = TWOPI / (double)sides;

    double x = sin(angle) * radius + cx;
    double y = cos(angle) * radius + cy;
    for (int i = 1; i <= sides; i++) {
      double xx = sin(angle + radians_per_edge * i) * radius + cx;
      double yy = cos(angle + radians_per_edge * i) * radius + cy;

      DrawLine(x, y, xx, yy, WHITE);
      x = xx;
      y = yy;
    }
  };

  auto DrawPoint = [&DrawLine](double x, double y) {
    DrawLine(x, y, x, y);
  };

  vector<pair<double, double>> path;

  
  int tt = 0;
  auto Draw = [&sun_angle,
	       &earthdriver_dia, &earthdriver_angle,
	       &sun_dia, &earth_dia, &earth_gear_ratio,
	       &wand_length, &orbit,
	       &path,
	       &Compute, &tt,
	       &DrawGear, &DrawPoint, &DrawLine]() {
    sdlutil::clearsurface(screen, BACKGROUND);

    const double max_radius = wand_length + orbit;

    static constexpr int SIZE = 920;
    static constexpr int YOFF = (STARTH - SIZE) >> 1;
    for (int y = 0; y < SIZE; y++) {
      double sa = y * (TWOPI / SIZE);
      const uint8 b = 255 * (y / (double)SIZE);
      for (int x = 0; x < SIZE; x++) {
	if ((x ^ y) == tt) {
	  double ea = x * (TWOPI * earth_gear_ratio / SIZE);
	  const uint8 r = 255 * (x / (double)SIZE);
	  sdlutil::drawpixel(screen, x, YOFF + y, r, 128, b);

	  Configuration c = Compute(sa, ea);

	  int cx = ((c.wand_x / max_radius) + 1.0) * 0.5 * SIZE;
	  int cy = ((c.wand_y / max_radius) + 1.0) * 0.5 * SIZE;
	  sdlutil::drawpixel(screen, cx + SIZE + 20, YOFF + cy, r, 128, b);
	}
	
      }

    }

    tt++;
    tt %= SIZE;

#if 0
    // Draw path.
    if (!path.empty()) {
      pair<double, double> pt = path[0];
      for (int i = 1; i < path.size(); i++) {
	DrawLine(pt.first, pt.second, path[i].first, path[i].second,
		 BLUE);
	pt = path[i];
      }
    }

    Configuration c = Compute(sun_angle, earthdriver_angle);

    // Sun just depends on its angle.
    DrawGear(0, 0, sun_dia / 2.0, sun_angle, 22);
    // Same.
    DrawGear(0, 0, earthdriver_dia / 2.0, earthdriver_angle, 5);

    DrawPoint(c.earth_x, c.earth_y);

    DrawGear(c.earth_x, c.earth_y, earth_dia / 2, c.earth_angle, 20);

    // Draw the wand.
    DrawLine(c.earth_x, c.earth_y, c.wand_x, c.wand_y, GREEN);
    
    path.push_back({c.wand_x, c.wand_y});
#endif

  };

  bool sun_on = true, earth_on = true;
  double t = 0.0;
  while(1) {
    SDL_Event e;
    /* event loop */
    while (SDL_PollEvent(&e)) {
      switch(e.type) {
      case SDL_QUIT:
	return 0;

      case SDL_KEYDOWN: {
	int key = e.key.keysym.sym;
	switch(key) {
	case SDLK_s:
	  sun_on = !sun_on;
	  break;
	case SDLK_e:
	  earth_on = !earth_on;
	  break;
	case SDLK_z:
	  sun_angle -= 0.01;
	  break;
	case SDLK_x:
	  sun_angle += 0.01;
	  break;
	case SDLK_COMMA:
	  earthdriver_angle -= 0.01;
	  break;
	case SDLK_PERIOD:
	  earthdriver_angle += 0.01;
	  break;
	case SDLK_SPACE: {
	  if (sun_on || earth_on) {
	    sun_on = earth_on = false;
	  } else {
	    sun_on = earth_on = true;
	  }
	  break;
	}
	case SDLK_ESCAPE:
	  return 0;
	default:;
	}
      }
	
      default:;
      }
    }

    if (sun_on) sun_angle += -0.04 + 0.01 * sin(t/33);
    if (earth_on) earthdriver_angle += 0.02 * sin(t / 50) * sin(t / 21);

    t += 1.0;
    Draw();
    SDL_Flip(screen);

    SDL_Delay(0);
  }

  return 0;
}
