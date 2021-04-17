
#include "graphics.h"

#include <memory>

#include "../cc-lib/image.h"
#include "../cc-lib/util.h"

static constexpr char TILES_PNG[] = "tiles.png";
static constexpr char TILEUTIL_PNG[] = "tileutil.png";
static constexpr char ANIMATION_PNG[] = "animation.png";

Graphics::~Graphics() {}


Graphics *Graphics::Create(const std::string &data_dir) {
  std::unique_ptr<ImageRGBA> tiles_png(
      ImageRGBA::Load(Util::dirplus(data_dir, TILES_PNG)));
  if (tiles_png.get() == nullptr) return nullptr;

  std::unique_ptr<ImageRGBA> tileutil_png(
      ImageRGBA::Load(Util::dirplus(data_dir, TILEUTIL_PNG)));
  if (tileutil_png.get() == nullptr) return nullptr;

  std::unique_ptr<ImageRGBA> animation_png(
      ImageRGBA::Load(Util::dirplus(data_dir, ANIMATION_PNG)));
  if (animation_png.get() == nullptr) return nullptr;

  return new Graphics(*tiles_png, *tileutil_png, *animation_png);
}

Graphics::Graphics(
    const ImageRGBA &tiles_png,
    const ImageRGBA &tileutil_png,
    const ImageRGBA &animation_png) : tiles(tiles_png),
                                      tileutil(tileutil_png) {
  {
    // include expects this name
    const ImageRGBA &all = animation_png;
    #include "animation_init.h"
  }
}
