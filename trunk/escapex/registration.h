
#ifndef __REGISTRATION_H
#define __REGISTRATION_H
/* n.b., register is a keyword */

#include "escapex.h"
#include "player.h"

struct Registration : public Drawable {
  static Registration *Create(Player *p);

  /* modifies p->webid to nonzero if successful */
  virtual void registrate() = 0;

  virtual ~Registration() {}

  void draw() override = 0;
  void screenresize() override = 0;
};

#endif
