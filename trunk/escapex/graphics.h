
#ifndef _ESCAPE_GRAPHICS_H
#define _ESCAPE_GRAPHICS_H

#include <string>

#include "../cc-lib/image.h"

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
  
private:
  // Use Create.
  Graphics(const ImageRGBA &tiles_png,
           const ImageRGBA &tileutil_png,
           const ImageRGBA &animation_png);
};

#endif
