
#ifndef __ESCAPEX_H
#define __ESCAPEX_H

#include "SDL.h"
#include "SDL_net.h"
#include "SDL_thread.h"
#include <math.h>

#include <string>
#include <memory>

#include "base.h"
#include "font.h"
#include "drawable.h"

#include "../cc-lib/sdl/sdlutil.h"

#include "version.h"

/* some build environments might want to find static data
   (e.g. PNGs) in some other location */
#ifndef DATADIR
/* but normally we just look in the cwd */
#  define DATADIR ""
#else
#  ifndef MULTIUSER
#    error "Can only set DATADIR if MULTIUSER."
#  endif
#endif

#define STARTW 800
#define STARTH 600

/* build this many zoomed versions of
   the graphics, each at 1/2 the size
   of the previous (including the 1:1
   originals)

   careful setting this too high, or
   else shrink50 will eventually fail
   because the images are too small
   (could fix this and have it return
   an empty or near-empty surface) */
#define DRAW_NSIZES 4

/* Handle a video event: Exposure or resize. Returns true if
   it was such an event. On resize, makes a new screen surface
   of the appropriate size. Either way, call parent's draw method
   and flip the screen, if it's non-NULL. */
bool HandleVideoEvent(Drawable *parent, const SDL_Event &e);

/* is the network enabled? */
extern int network;
/* is the audio subsystem started? */
extern int audio;

extern SDL_Surface *screen;

#ifndef PLATFORM
#  ifdef WIN32
#    define PLATFORM "win32"
#  else
#    ifdef OSX
#      define PLATFORM "osx"
#    else /* assume linux */
#      define PLATFORM "linux"
#      define LINUX
#    endif
#  endif
#endif

#endif
