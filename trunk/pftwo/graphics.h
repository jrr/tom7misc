#ifndef __GRAPHICS_H
#define __GRAPHICS_H

#include <vector>
#include <string>

#include "SDL.h"
#include "pftwo.h"

// assumes ARGB, surfaces exactly the same size, etc.
void CopyARGB(const vector<uint8> &argb, SDL_Surface *surface);

void BlitARGB(const vector<uint8> &argb, int w, int h,
	      int x, int y, SDL_Surface *surface);

void SaveARGB(const vector<uint8> &argb, int width, int height,
	      const string &filename);

void HalveARGB(const vector<uint8> &argb, int width, int height,
	       SDL_Surface *surface);

void BlitARGBHalf(const vector<uint8> &argb, int width, int height,
		  int xpos, int ypos,
		  SDL_Surface *surface);

inline constexpr uint8 Mix4(uint8 v1, uint8 v2, uint8 v3, uint8 v4) {
  return (uint8)(((uint32)v1 + (uint32)v2 + (uint32)v3 + (uint32)v4) >> 2);
}

#endif
