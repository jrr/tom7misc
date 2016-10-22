
#ifndef __REGISTRATION_H
#define __REGISTRATION_H
/* n.b., register is a keyword */

#include "escapex.h"
#include "player.h"

struct Registration : public Drawable {
  static Registration *Create(Player *p);

  /* modifies p->webid to nonzero if successful */
  virtual void Registrate() = 0;

  virtual ~Registration() {}

  void Draw() override = 0;
  void ScreenResize() override = 0;
};

#endif
