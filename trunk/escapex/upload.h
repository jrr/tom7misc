
#ifndef __UPLOAD_H
#define __UPLOAD_H

#include "escapex.h"
#include "player.h"

/* Upload a level and its solution to the server. */

enum class UploadResult { OK, FAIL, };

struct Upload : public Drawable {
  static Upload *Create();
  virtual ~Upload();

  virtual UploadResult Up(Player *p, string file, string desc) = 0;

  void draw() override = 0;
  void screenresize() override = 0;
};

#endif
