
#include "message.h"
#include "font.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"
#include "util.h"
#include "chars.h"

namespace {

struct Message_ : public Message {
  static Message_ *Create();
  ~Message_() override {
    if (alpharect) SDL_FreeSurface(alpharect);
  }
  
  /*  enter: true
     escape: false */
  bool Ask(char *actualchar = 0, string charspec = "") override;

  void Draw() override;
  void ScreenResize() override;

  SDL_Surface *alpharect = nullptr;
  bool loop(char *actualchar, string charspec);

  void init();
  void Redraw() {
    Draw();
    SDL_Flip(screen);
  }

  int nlines = 0;
  int posx = 0;
};


Message_ *Message_::Create() {
  Message_ *pp = new Message_{};
  pp->below = 0;
  pp->alpharect = 0;
  pp->posx = 0;
  pp->nlines = 0;
  return pp;
}

void Message_::init() {

  /* find longest line */
  int ll = 0;
  {
    string titlen = title + "\n";
    int cl = 0;
    for (unsigned int i = 0; i < titlen.length(); i++) {
      if (titlen[i] == '\n') {
        ll = std::max(fon->sizex(titlen.substr(cl, i - cl)), ll);
        cl = i;
      } else if (titlen[cl] == '\n') cl = i;
    }
  }

  int w =
    (2 * fon->width) +
    std::max(ll,
	     fon->sizex("ESCAPE: ") +
	     std::max(fon->sizex(ok),
		      fon->sizex(cancel)));

  nlines = Font::lines(title);

  const int h = cancel == "" ?
    ((3 + nlines) * fon->height) :
    ((4 + nlines) * fon->height);

  /* now center */
  posx = (screen->w - w) / 2;

  /* and y, if desired */
  if (posy < 0) {
    posy = (screen->h - h) / 2;
  }

  /* XXX make these values setable */
  /* create alpha rectangle */
  alpharect = sdlutil::makealpharect(w, h, 90, 90, 90, 200);

  sdlutil::outline(alpharect, 2, 36, 36, 36, 200);
}

bool Message_::Ask(char *actualchar, string charspec) {
  init();
  return loop(actualchar, charspec);
}

void Message_::ScreenResize() {
  if (below) below->ScreenResize();
}

void Message_::Draw() {
  /* clear back */
  if (!below) {
    sdlutil::clearsurface(screen, BGCOLOR);
  } else {
    below->Draw();
  }

  /* draw alpha-transparent box */
  SDL_Rect dest;

  dest.x = posx;
  dest.y = posy;

  SDL_BlitSurface(alpharect, 0, screen, &dest);

  /* draw text */
  fon->drawlines(posx + fon->width, posy + fon->height, title);
  fon->draw(posx + fon->width, posy + ((1 + nlines) * fon->height),
            (string)YELLOW "ENTER" POP ":  " + ok);
  if (cancel != "")
    fon->draw(posx + fon->width, posy + ((2 + nlines) * fon->height),
              (string)YELLOW "ESCAPE" POP ": " + cancel);
}

bool Message_::loop(char *actualchar, string charspec) {
  Redraw();

  SDL_Event e;
  while (SDL_WaitEvent(&e) >= 0) {
    if (HandleVideoEvent(this, e)) continue;
    int key;

    switch (e.type) {
    case SDL_QUIT:
      return false; /* XXX ? */
    case SDL_MOUSEBUTTONDOWN: {
      SDL_MouseButtonEvent *em = (SDL_MouseButtonEvent*)&e;

      if (em->button == SDL_BUTTON_LEFT) {
        /* allow a click within the entire box if
           there is no cancel; otherwise, require clicking
           on the text itself. */

         int x = em->x;
        int y = em->y;

        if (cancel == "") {
          if (x > posx &&
              x < (posx + alpharect->w) &&
              y >= posy &&
              y < (posy + alpharect->h)) return true;
        } else {
          if (x > posx &&
              x < (posx + alpharect->w)) {

            /* which did we click on? */
            if (y > posy + ((1 + nlines) * fon->height) &&
                y < posy + ((2 + nlines) * fon->height)) return true;

            if (y > posy + ((2 + nlines) * fon->height) &&
                y < posy + ((3 + nlines) * fon->height)) return false;

          }
        }
        /* out of area; flash screen? */

      }
    }
    case SDL_KEYDOWN:
      key = e.key.keysym.sym;
      if (actualchar) {
          int uc = e.key.keysym.unicode;
          if ((uc & ~0x7F) == 0 && uc >= ' ') {
            *actualchar = (char)uc;
            /* non-ascii */
          } else *actualchar = 0;
      }
      switch (key) {
      case SDLK_ESCAPE:
        return false;
      case SDLK_RETURN:
        return true;

      default:;
        /* is this a key in our range? */
        if (actualchar && util::matchspec(charspec, *actualchar)) {
          return false; /* wasn't 'enter' */
        }
        /* XXX else might flash screen or something */
      }
      break;
    default:;
    }
  }
  return false; /* XXX ??? */
}

}  // namespace

void Message::DrawOnlyv(int posy,
                        string ttitle,
                        string ook, string ccancel,
                        string icon) {
  std::unique_ptr<Message_> m{Message_::Create()};
  m->below = nodraw;
  m->posy = posy;
  m->title = icon + WHITE " " + ttitle;
  m->ok = ook;
  m->cancel = ccancel;

  m->init();
  m->Draw();
}

Message::~Message() {}
Message *Message::Create() {
  return Message_::Create();
}
