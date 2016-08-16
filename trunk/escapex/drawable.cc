
#include "drawable.h"

struct nodraw_t : public Drawable {
  virtual void draw() {}
  virtual void screenresize() {}
};

/* singleton no-draw object */
Drawable *nodraw;

void Drawable::init() {
  nodraw = new nodraw_t();
}
