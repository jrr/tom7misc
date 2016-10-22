#include "registration.h"
#include "client.h"
#include "http.h"
#include "draw.h"

namespace {
struct Registration_ : public Registration {
  Registration_() {}

  void Registrate() override;

  void ScreenResize() override {
    /* XXX resize tx */
  }
  void Draw() override {
    sdlutil::clearsurface(screen, BGCOLOR);
    tx->Draw();
  }

  void Redraw() {
    Draw();
    SDL_Flip(screen);
  }

  void say(const string &s) {
    if (tx.get() != nullptr) {
      tx->Say(s);
      Redraw();
    }
  }

  std::unique_ptr<TextScroll> tx;
  Player *plr;
};

void Registration_::Registrate() {
  std::unique_ptr<HTTP> hh{Client::Connect(plr, tx.get(), this)};
  if (hh.get() == nullptr) {
    Message::Quick(this,
                   "Couldn't connect to server.",
                   "Sorry", "", PICS XICON POP);
    return;
  }

  /* XXX again, need a better way to detect this */
  if (plr->name == "Default") {
    Message::Quick(this,
                   "You can't register with the default player.",
                   "Sorry", "", PICS XICON POP);
    return;
  }

  int tries = 2;

  string res;
  while (tries--) {
    int seql = util::random();
    int seqh = util::random() ^ (((int)SDL_GetTicks() << 16) |
                                 ((int)SDL_GetTicks() >> 16));

    seql = abs(seql);
    seqh = abs(seqh);

    say("try " + itos(seql) + " " + itos(seqh) + (string)"...");

    if (Client::RPC(hh.get(), REGISTER_RPC,
                    (string) "seql=" + itos(seql) +
                    (string)"&seqh=" + itos(seqh) +
                    (string)"&name=" + HTTPUtil::URLEncode(plr->name),
                    res)) {

      int id = util::stoi(res);
      if (id) {
        plr->webid = id;
        plr->webseql = seql;
        plr->webseqh = seqh;

        if (plr->WriteFile()) {
          say((string)"success! " GREEN + res);
          Message::Quick(this,
                         (string)"You are registered as player "
                         YELLOW "#" + itos(id),
                         "OK!", "", PICS THUMBICON POP);
          return;
        } else {
          say(RED "can't write player file...?");
        }
      } else {
        say((string)"non-number response? [" RED + res + POP "]");
      }
    } else {
      say((string)"fail: [" RED + res + POP "]");
    }
  }

  /* give up ... */

  Message::No(this,
              "Unable to register:\n"
              "     " RED + res + POP "\n"
              "   Try again later!");

  return;
}

}  // namespace

Registration *Registration::Create(Player *p) {
  Registration_ *r = new Registration_();
  r->plr = p;
  r->tx.reset(TextScroll::Create(fon));
  return r;
}
