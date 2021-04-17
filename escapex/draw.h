
#ifndef _ESCAPE_DRAW_H
#define _ESCAPE_DRAW_H

#include <math.h>
#include <string>
#include <cstdint>

#include "level.h"
#include "graphics.h"

#include "../cc-lib/sdl/font.h"

class SDL_Surface;
class Rating;
class RateStatus;

extern Font *fon;
extern Font *fonsmall;

struct Drawing {
  /* initialized by loadimages:
     there are DRAW_NSIZES of these
     in exponential backoff */
  static SDL_Surface **tiles;
  static SDL_Surface **guy;
  static SDL_Surface **tilesdim;
  static SDL_Surface **tileutil;

  /* call this once in the program. true on success */
  static bool LoadImages(const Graphics &graphics);

  static void DestroyImages();

  /* set these before using the functions
     below */

  /* space around the tiles in a level drawing */
  int margin = 0;

  /* in tiles. 0,0 means no scroll. */
  int scrollx = 0;
  int scrolly = 0;

  int posx = 0;
  int posy = 0;
  int width = 0;
  int height = 0;

  /* must be in range 0..(DRAW_NSIZES - 1) */
  int zoomfactor = 0;

  // Not owned!
  const Level *lev = nullptr;

  std::string message;

  /* must set at least width, height, lev */
  Drawing() {}

  /* Drawing functions */

  /* screen coordinates */
  static void DrawGuy(dir d,
                      int sx, int sy,
                      int zoomfactor,
                      SDL_Surface *surf = 0, bool dead = false);

  static void DrawBot(bot b,
                      dir d,
                      int sx, int sy,
                      int zoomfactor,
                      SDL_Surface *surf = 0,
                      int data = -1);

  /* if surface isn't supplied, then draw to screen. */
  static void DrawTile(int px, int py, int tl, int zfactor = 0,
                       SDL_Surface *surf = 0, bool dim = false);
  static void DrawTileU(int px, int py, int tl, int zf = 0,
                        SDL_Surface *surf = 0);

  void DrawLev(int layer = 0,
               SDL_Surface *surf = nullptr, bool dim = false);

  /* title, message, etc */
  void DrawExtra(SDL_Surface *surf = nullptr);

  /* debugging/editor/cheat */
  void DrawDests(SDL_Surface *surf = nullptr, bool shuffle = false);

  void DrawBotNums(SDL_Surface *surf = nullptr);

  /* make sure the scroll doesn't waste space by
     drawing nothing when it could be drawing
     something */
  void MakeScrollReasonable();

  /* scroll so that the guy is visible and somewhat
     centered */
  void SetScroll();

  /* given screen coordinates x,y, return a tile
     if it is inside one on the screen */
  bool InMap(int x, int y,
             int &tx, int &ty) const;

  /* given a tile x,y, return its
     screen coordinates if it is displayed currently. */
  bool OnScreen(int x, int y,
                int &tx, int &ty) const;

  /* XXX this should probably be elsewhere */
  /* XXX combine y and botmargin, which have to agree anyway
     ps. this function is much less complicated now */
  /* draw a small version of the level, with some info.
     used for the load screen and rating.

     solvemoves is the number of moves in the player's solution,
     or 0 if unsolved (all valid solutions have at least one move.)
  */
  static void DrawSmall(int y, int botmargin, uint32_t color,
                        const Level *l, int solvemoves,
                        const std::string &fname,
                        RateStatus *votes,
                        Rating *myrating, int date = 0,
                        int speedrecord = 0);

  /* height of small drawings */
  static int SmallHeight();
};

#endif
