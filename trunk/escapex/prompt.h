#ifndef __PROMPT_H
#define __PROMPT_H

#include "escapex.h"
#include "drawable.h"

struct Prompt {
  string title;
  string input;
  string explanation;

  Drawable *below;

  /* XXX should probably be just this static method,
     and perhaps in util */
  static string ask(Drawable *b, string title, string def = "");

  static Prompt *Create();

  string select();

  virtual ~Prompt();
};

#endif
