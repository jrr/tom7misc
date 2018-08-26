#ifndef __GRAPHICS_H
#define __GRAPHICS_H

#include <vector>
#include <string>

#include "SDL.h"
#include "pftwo.h"

#include "headless-graphics.h"

// XXX This code is confused because for a long time I didn't understand
// that SDL surfaces store their pixels in a platform-specific order. It's
// not an endianness thing. Fix this so that things called ARGB or RGBA
// are really in that byte order, and so that the blit functions that
// use SDL surfaces respect this.

// assumes ARGB, surfaces exactly the same size, etc.
void CopyARGB(const vector<uint8> &argb, SDL_Surface *surface);

void BlitARGB(const vector<uint8> &argb, int w, int h,
	      int x, int y, SDL_Surface *surface);

void HalveARGB(const vector<uint8> &argb, int width, int height,
	       SDL_Surface *surface);

void BlitARGBHalf(const vector<uint8> &argb, int width, int height,
		  int xpos, int ypos,
		  SDL_Surface *surface);

// The ones below this line treat RGBA correctly.

// Blit at 2X size. Treats alpha channel as being always 0xFF.
void BlitRGBA2x(const vector<uint8> &rgba, int w, int h,
		int x, int y, SDL_Surface *surface);

// Same as above, 1:1.
void BlitRGBA(const vector<uint8> &rgba, int w, int h,
	      int x, int y, SDL_Surface *surface);

// Implied alpha=0xFF.
void SetPixelRGB(int x, int y, uint8 r, uint8 g, uint8 b,
		 SDL_Surface *surface);

#endif
