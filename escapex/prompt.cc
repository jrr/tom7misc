
#include "prompt.h"

#include <string>

#include "../cc-lib/sdl/font.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"
#include "chars.h"
#include "menu.h"

/* XXX implement in terms of menu class?
   textinput is meant to be the same */

Prompt *Prompt::Create() {
  Prompt *pp = new Prompt();
  pp->below = 0;
  return pp;
}

Prompt::~Prompt() {}

string Prompt::ask(Drawable *b, string t, string d) {
  std::unique_ptr<Prompt> pp{Prompt::Create()};
  pp->title = t;
  pp->below = b;
  pp->input = d;

  return pp->select();
}

string Prompt::select() {
  TextInput inp;
  inp.question = title;
  inp.input = input;
  inp.explanation = explanation;

  VSpace spacer((int)(fon->height * 1.5f));

  Okay ok;
  ok.text = "Accept";

  Cancel can;

  std::unique_ptr<Menu> mm =
    Menu::Create(below, GREY "Input Required",
                 {&inp, &spacer, &ok, &can},
                 false);
  InputResultKind res = mm->menuize();

  if (res == InputResultKind::OK) {
    return inp.input;
  } else return ""; /* ?? */
}
