
#ifndef __MESSAGE_H
#define __MESSAGE_H

#include "escapex.h"
#include "drawable.h"
#include "chars.h"

struct Message : public Drawable {

  string title;
  string ok;
  string cancel;

  Drawable *below;

  /* xpos, height and width will
     be computed automatically.
     If posy is below zero, center.
  */
  int posy;

  static Message *create();
  virtual void destroy() = 0;

  /* ok: true
     cancel: false */
  virtual bool ask(char *actualchar = 0,
                   string charspec = "") = 0;

  virtual void draw() = 0;
  virtual void screenresize() = 0;
  virtual ~Message();

  static bool quick(Drawable *bbelow, string ttitle,
                    string ook, string ccancel,
                    string icon = PICS EXCICON POP,
                    char *actualchar = 0,
                    string charspec = "") {
    Message *m = Message::create();
    m->below = bbelow;
    m->posy = -1;
    m->title = icon + WHITE " " + ttitle;
    m->ok = ook;
    m->cancel = ccancel;

    const bool x = m->ask(actualchar, charspec);
    m->destroy();
    return x;
  }

  static bool quickv(Drawable *bbelow, int posy,
                     string ttitle,
                     string ook, string ccancel,
                     string icon = PICS EXCICON POP,
                     char *actualchar = 0,
                     string charspec = "") {
    Message *m = Message::create();
    m->below = bbelow;
    m->posy = posy;
    m->title = icon + WHITE " " + ttitle;
    m->ok = ook;
    m->cancel = ccancel;

    bool x = m->ask(actualchar, charspec);
    m->destroy();
    return x;
  }

  static void drawonlyv(int posy,
                        string ttitle,
                        string ook, string ccancel,
                        string icon = PICS EXCICON POP);

  static bool bug(Drawable *bbelow, string ttitle, char *actualchar = 0,
                  string charspec = "") {
    printf("Bug: %s\n", ttitle.c_str());
    Message *m = Message::create();
    m->below = bbelow;
    m->posy = -1;
    m->title = PICS BUGICON POP RED " BUG: " POP WHITE + ttitle +
               "\n    (please leave a bug report at escape.spacebar.org)";
    m->ok = RED "Sorry" POP;
    m->cancel = "";

    bool x = m->ask(actualchar, charspec);
    m->destroy();
    return x;
  }

  static bool no(Drawable *bbelow, string ttitle, char *actualchar = 0,
                 string charspec = "") {
    Message *m = Message::create();
    m->below = bbelow;
    m->posy = -1;
    m->title = PICS XICON POP WHITE " " + ttitle;
    m->ok = "OK";
    m->cancel = "";

    bool x = m->ask(actualchar, charspec);
    m->destroy();
    return x;
  }

};

#endif
