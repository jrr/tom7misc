#ifndef _PROMPT_H
#define _PROMPT_H

#include <string>

#include "escapex.h"
#include "drawable.h"

struct Prompt {
  std::string title;
  std::string input;
  std::string explanation;

  Drawable *below;

  /* XXX should probably be just this static method,
     and perhaps in util */
  static std::string ask(Drawable *b,
                         std::string title, std::string def = "");

  static Prompt *Create();

  std::string select();

  virtual ~Prompt();
};

#endif
