
#ifndef _ESCAPE_UPLOAD_H
#define _ESCAPE_UPLOAD_H

#include "escapex.h"
#include "player.h"

/* Upload a level and its solution to the server. */

enum class UploadResult { OK, FAIL, };

struct Upload : public Drawable {
  static Upload *Create();
  virtual ~Upload();

  virtual UploadResult Up(Player *p, const string &file, const string &desc) = 0;

  void Draw() override = 0;
  void ScreenResize() override = 0;
};

#endif
