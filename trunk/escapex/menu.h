
#ifndef __MENU_H
#define __MENU_H

#include "escapex.h"
#include <SDL.h>
#include "util.h"
#include "chars.h"

/* menus are essentially what are often called
   "forms" in GUI lingo */

enum class InputResultKind {
  NOTHING,
  /* go to the next or previous item */
  NEXT, PREV,
  /* the key is rejected as illegal */
  REJECT,
  /* some action has changed the display
     (implied by next,prev) */
  UPDATED,
  /* confirm or cancel the entire menu */
  OK, CANCEL,
  /* quit the program */
  QUIT,
};

struct InputResult {
  InputResultKind k;
  virtual InputResultKind kind() { return k; }
  explicit InputResult(InputResultKind i) : k(i) {}
};

/* XXX add a way of getting defaults */
/* (mostly) abstract base class of menuitems,
   which are "controls" in GUI lingo */
struct MenuItem {
  /* perhaps multi-line explanation of what this
     control sets. */
  string explanation;

  /* how many pixels to indent by.
     this amount is interpreted as being automatically
     added to the width in size and the x value for
     draw. */
  int indent = 0;

  /* if the control is disabled, it should not
     be focusable and should draw differently */
  bool disabled = false;

  /* parent menu */
  struct Menu *container = nullptr;

  /* stuff that all menuitems have */

  /* some controls can be focused, some not */
  virtual bool focusable() { return !disabled; }

  virtual void size(int &width, int &height) = 0;
  virtual void draw(int x, int y, int focused) = 0;

  /* mouse click relative to menuitem */
  virtual InputResult click(int x, int y) {
    /* XXX send key 'enter' */
    return InputResult(InputResultKind::REJECT);
  }

  /* displayed at the bottom of the screen to
     explain how to use the control */
  virtual string helptext() = 0;

  /* some default behavior here */
  /* process a keypress.
     assume e.type = SDL_KEYDOWN */
  virtual InputResult key(SDL_Event e);

  MenuItem() {}
  virtual ~MenuItem() {}
};

/* unselectable labels */
struct Label : public MenuItem {
  string text;
  virtual bool focusable() { return false; }
  virtual string helptext() { return ""; }
  virtual void draw(int x, int y, int f);
  virtual void size(int &w, int &h);
  virtual ~Label() {}
};

/* empty space */
struct VSpace : public MenuItem {
  int height = 0;
  explicit VSpace(int n) : height(n) {}
  virtual bool focusable() { return false; }
  virtual string helptext() { return ""; }
  virtual void draw(int x, int y, int f) { }
  virtual void size(int &w, int &h) {
    w = 1;
    h = height;
  }
  virtual ~VSpace() {}
};

struct TextInput : public MenuItem {
  string question;
  string input;
  /* immediately accept when pressing 'enter'? */
  bool accept_on_enter = false;

  string helptext() override {
    return "Enter a single line of text.";
  }
  void draw(int x, int y, int) override;
  void size(int &w, int &h) override;
  InputResult key(SDL_Event e) override;

  TextInput() {}

 protected:
   virtual void draw_ch(int x, int y, int, char passwordchar = 0);
 private:
  /* cursor before nth character in input */
  /* XXX probably want to check that the cursor is
     still inside 'input', because input can be
     changed externally */
  int cursor = 0;
};

struct TextPassword : public TextInput {
  string helptext() override {
    return "Enter a password.";
  }
  void draw(int x, int y, int i) override {
    draw_ch(x, y, i, '*');
  }
};

struct Toggle : public MenuItem {
  string question;
  bool checked = false;

  string helptext() override {
    return "Press " BLUE "enter" POP " or "
      BLUE "space" POP " to toggle.";
  }
  void draw(int x, int y, int) override;
  void size(int &w, int &h) override;
  InputResult key(SDL_Event e) override;
  InputResult click(int, int) override;
};

struct Slider : public MenuItem {
  string question;
  /* labels over lowest and highest points in slider */
  string low;
  string high;

  /* inclusive */
  int lowest = 0;
  int highest = 0;

  /* for best accuracy, nsegs should be
     (highest - lowest + 1). This directly affects
     the width. Therefore nsegs should be at
     least length(low) + length(hi) + 1 */
  int nsegs = 0;

  string helptext() override {
    return "Press " BLUE "left" POP " or " BLUE "right" POP
           " to change the setting.";
  }

  Slider(int lows, int highs, int segs);
  void draw(int x, int y, int f) override;
  void size(int &w, int &h) override;
  InputResult key(SDL_Event e) override;
  InputResult click(int, int) override;

  /* selected position along line (value in the inclusive
     interval (lowest, highest)) */
  int pos = 0;

 private:
  /* scrollbar graphic as string. has nseg segments */
  string scrollbar;
};

struct Okay : public MenuItem {
  string text;
  int *ptr = nullptr;
  int myval = 0;

  string helptext() override {
    return "Press " BLUE "enter" POP " to confirm.";
  }

  Okay() {}

  Okay(string text, int *ptr = nullptr, int myval = 0)
    : text(text), ptr(ptr), myval(myval) {}

  void draw(int x, int y, int f) override;
  void size(int &w, int &h) override;

  InputResult key(SDL_Event e) override;

  InputResult click(int, int) override {
    activate();
    return InputResult(InputResultKind::OK);
  }

 protected:
  virtual void activate() {
    /* set pointer if applicable */
    if (ptr) *ptr = myval;
  }
};

struct Cancel : public MenuItem {
  string text = "Cancel";

  string helptext() override {
    return "Press " BLUE "enter" POP " to cancel. "
           GREY "(" BLUE "esc" POP " also cancels at any time.)";
  }

  void draw(int x, int y, int f) override;
  void size(int &w, int &h) override;
  InputResult key(SDL_Event e) override;

  InputResult click(int, int) override {
    return InputResult(InputResultKind::CANCEL);
  }
};


/* menus */

struct Menu : public Drawable {
  /* create a menu from some items. After this point,
     it's not possible to add or remove items. If
     'fullscreen' is true, the menu uses the whole screen.
     Otherwise, it stretches to accomodate its contents
     and is centered on the screen. */
  /* does not take ownership of the item pointers
     or the list cells */
  static std::unique_ptr<Menu> Create(Drawable *below,
				      string title,
				      PtrList<MenuItem> *items,
				      bool fullscreen);

  void draw() override;
  void screenresize() override;

  /* enter modal mode and return the final result of
     the menu. expect OK, CANCEL, or QUIT */
  InputResultKind menuize();

  /* all menuitems will still be available */
  virtual ~Menu();

  int alpha = 200;
  /* set negative to get centered behavior */
  int yoffset = -1;

 private:
  Menu() {}

  void Redraw();
  bool skip_ok(int);
  void fixup(int);
  void nextfocus(int dir);
  InputResult clickselect(int x, int y);

  Drawable *below = nullptr;
  string title;
  int nitems = 0;
  MenuItem **items = nullptr;
  bool fullscreen = false;
  int selected = 0;
  int skip = 0;

  int posx = 0, posy = 0;
  int w = 0, h = 0, stath = 0;
  SDL_Surface *alpharect = nullptr;
};

#endif
