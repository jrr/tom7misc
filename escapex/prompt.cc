
#include "prompt.h"
#include "font.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"
#include "chars.h"
#include "extent.h"
#include "menu.h"
#include "ptrlist.h"

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
  textinput inp;
  inp.question = title;
  inp.input = input;
  inp.explanation = explanation;

  vspace spacer((int)(fon->height * 1.5f));

  okay ok;
  ok.text = "Accept";

  cancel can;
  can.text = "Cancel";

  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spacer);
  PtrList<MenuItem>::push(l, &inp);
  // PtrList<MenuItem>::push(l, &lab);

  menu *mm = menu::create(below, GREY "Input Required", l, false);
  resultkind res = mm->menuize();
  PtrList<MenuItem>::diminish(l);
  mm->destroy();
  
  if (res == MR_OK) {
    return inp.input;
  } else return ""; /* ?? */
}
