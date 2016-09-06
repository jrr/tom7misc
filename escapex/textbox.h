#ifndef __TEXTBOX_H
#define __TEXTBOX_H

#include <string>
#include "menu.h"

struct TextBox : public MenuItem {
  virtual string helptext() {
    return "Use like a normal simple editor. " BLUE "Tab" POP " exits.";
  }
  void draw(int x, int y, int f) override;
  void size(int &w, int &h) override;
  ~TextBox() override { Clear(); }

  string get_text();
  /* puts cursor at the beginning, 0 scroll.
     might want to call goto_end after setting. */
  void set_text(string);

  inputresult key(SDL_Event e) override;

  TextBox(int cw, int ch) : charsw(cw), charsh(ch) {}

  string question;

  void goto_beginning();
  void goto_end();

  private:
  /* we use a "red sea" implementation, where the cursor
     is the cleaving, and we insert new chars onto the head
     of before (which appears in reverse order) */
  vallist<char> *before = nullptr;
  vallist<char> *after = nullptr;

  int charsw, charsh;

  /* destroys before, after */
  void Clear();

  string prevline(vallist<char> *&bb);
  string nextword(vallist<char> *&aa);
  string popword(vallist<char> *&bb, char &p);
  void addstring(string);
  void type(char);
  void left(bool erasing);
  void left_noflow(bool);
  void up();
  void down();
  void right(bool erasing);
  int  countprevline(vallist<char> *&bb);
};

#endif
