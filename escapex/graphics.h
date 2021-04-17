
#ifndef _ESCAPE_GRAPHICS_H
#define _ESCAPE_GRAPHICS_H

#include <string>

#include "../cc-lib/image.h"

/* Size of zoom 0 tiles.
   We assume that these are
   evenly divisible by
   2^(DRAW_NSIZES - 1) */
#define TILEW 32
#define TILEH 32

#define GUY_OVERLAPY 5
#define DALEK_OVERLAPY 5
#define HUGBOT_OVERLAPY 5
#define BROKEN_OVERLAPY 0
#define BOMB_OVERLAPY 5
#define MAX_OVERLAPY 5

#define BGCOLOR 0

/* width of source graphic in tiles */
#define SRCTILESW 16

/* utility tiles */
enum {
  TU_TARGET, TU_DISABLED, TU_WARNING,
  TU_LAYERNORMAL, TU_LAYERALT,
  TU_TILESUD,
  TU_SAVE, TU_SAVEAS, TU_LOAD,
  TU_TITLE, TU_AUTHOR, TU_SIZE, TU_PLAYERSTART,
  TU_CLEAR, TU_PLAY, TU_RANDOM, TU_RANDTYPE,
  TU_CHANGED,

  /* menu items */
  TU_X, TU_N, TU_I,
  TU_T, TU_1, TU_2, TU_3, TU_4, TU_P,

  TU_EXITOPEN,

  TU_ERASE_BOT, TU_FIRST_BOT,
  TU_DALEK, TU_BROKEN, TU_HUGBOT,

  TU_PLAYBUTTON, TU_PAUSEBUTTON,
  TU_FREVBUTTON, TU_FFWDBUTTON,
  TU_FWDBUTTON, TU_REVBUTTON,

  TU_SLEEPWAKE, TU_PREFAB,
  TU_BOMB, TU_BOMBTIMER,

  TU_SAVESTATE, TU_RESTORESTATE,
  TU_BOOKMARKS,
  TU_RESTART,
  TU_UNDO,
  TU_REDO,
  TU_PLAYPAUSE,
  TU_PLAYPAUSE_PLAY,
  TU_FREDO,
  TU_FUNDO,
};

// All Escape graphics, headless.
struct Graphics {
  // Loads graphics when constructed.
  // Expects tiles.png, tileutil.png, animation.png etc.
  // in the supplied data dir. Returns nullptr upon failure.
  static Graphics *Create(const std::string &data_dir);
  virtual ~Graphics();
 
  const ImageRGBA tiles, tileutil;
  
  ImageRGBA
#include "animation_syms.h"
  ;

  // Get the pixel coordinates of tile t in the "tiles" image.
  // It has size TILEWxTILEH.
  static inline std::pair<int, int> TileXY(int t) {
    return make_pair((t % SRCTILESW) * TILEW,
                     (t / SRCTILESW) * TILEH);
  }
  
private:
  // Use Create.
  Graphics(const ImageRGBA &tiles_png,
           const ImageRGBA &tileutil_png,
           const ImageRGBA &animation_png);
};

#endif
