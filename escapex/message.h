#ifndef __MESSAGE_H
#define __MESSAGE_H

#include "escapex.h"
#include "drawable.h"
#include "chars.h"

struct Message : public Drawable {
  string title;
  string ok;
  string cancel;

  Drawable *below = nullptr;

  /* xpos, height and width will
     be computed automatically.
     If posy is below zero, center.
  */
  int posy = -1;

  static Message *Create();

  /* ok: true
     cancel: false */
  virtual bool Ask(char *actualchar = 0,
                   string charspec = "") = 0;

  void draw() override = 0;
  void screenresize() override = 0;
  virtual ~Message();

  static bool Quick(Drawable *bbelow, string ttitle,
                    string ook, string ccancel,
                    string icon = PICS EXCICON POP,
                    char *actualchar = 0,
                    string charspec = "") {
    std::unique_ptr<Message> m{Message::Create()};
    m->below = bbelow;
    m->posy = -1;
    m->title = icon + WHITE " " + ttitle;
    m->ok = ook;
    m->cancel = ccancel;

    return m->Ask(actualchar, charspec);
  }

  static bool Quickv(Drawable *bbelow, int posy,
                     string ttitle,
                     string ook, string ccancel,
                     string icon = PICS EXCICON POP,
                     char *actualchar = 0,
                     string charspec = "") {
    std::unique_ptr<Message> m{Message::Create()};
    m->below = bbelow;
    m->posy = posy;
    m->title = icon + WHITE " " + ttitle;
    m->ok = ook;
    m->cancel = ccancel;

    return m->Ask(actualchar, charspec);
  }

  static void DrawOnlyv(int posy,
                        string ttitle,
                        string ook, string ccancel,
                        string icon = PICS EXCICON POP);

  static bool Bug(Drawable *bbelow, string ttitle, char *actualchar = 0,
                  string charspec = "") {
    printf("Bug: %s\n", ttitle.c_str());
    std::unique_ptr<Message> m{Message::Create()};
    m->below = bbelow;
    m->posy = -1;
    m->title = PICS BUGICON POP RED " BUG: " POP WHITE + ttitle +
               "\n    (please leave a bug report at escape.spacebar.org)";
    m->ok = RED "Sorry" POP;
    m->cancel = "";

    return m->Ask(actualchar, charspec);
  }

  static bool No(Drawable *bbelow, string ttitle, char *actualchar = 0,
                 string charspec = "") {
    std::unique_ptr<Message> m{Message::Create()};
    m->below = bbelow;
    m->posy = -1;
    m->title = PICS XICON POP WHITE " " + ttitle;
    m->ok = "OK";
    m->cancel = "";

    return m->Ask(actualchar, charspec);
  }
};

#endif
