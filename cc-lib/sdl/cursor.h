
// A utility for creating (system) cursors, plus some standard ones.
// Note that if you want to do anything fancy (colors etc.) then you
// should SDL_ShowCursor(SDL_DISABLE) and just blit the cursor
// yourself.

#ifndef __SDLUTIL_CURSOR_H
#define __SDLUTIL_CURSOR_H

struct SDL_Cursor;

struct Cursor {
  // Returned cursor should be freed with SDL_FreeCursor.
  // Format of the ascii_img is width*height characters, where '#' indicates
  // black, '-' indicates white, and ' ' is transparent; see .cc file for
  // examples.
  // Cursor width must be a multiple of 8.
  static SDL_Cursor *MakeCursor(int width, int height, int hot_x, int hot_y,
				const char *ascii_img);

  // Built-in cursor data. These are all 32x32.
  // Standard arrow.
  static SDL_Cursor *MakeArrow();
  // "fill" bucket 
  static SDL_Cursor *MakeBucket();
  // Matching hand pointing, and hand gripping
  static SDL_Cursor *MakeHand();
  static SDL_Cursor *MakeHandClosed();
private:
  Cursor() = delete;
};

#endif
