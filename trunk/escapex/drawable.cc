
#include "drawable.h"

namespace {
struct NoDraw : public Drawable {
  void Draw() override {}
  void ScreenResize() override {}
};
}

/* singleton no-draw object */
Drawable *nodraw = nullptr;

void Drawable::Init() {
  nodraw = new NoDraw();
}
