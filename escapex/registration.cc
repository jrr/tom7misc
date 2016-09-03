#include "registration.h"
#include "client.h"
#include "http.h"
#include "draw.h"

namespace {
struct Registration_ : public Registration {
  Registration_() {}

  void registrate() override;

  void screenresize() override;
  void draw() override;

  void redraw() {
    draw();
    SDL_Flip(screen);
  }

  void say(const string &s) {
    if (tx.get() != nullptr) {
      tx->say(s);
      redraw();
    }
  }

  std::unique_ptr<TextScroll> tx;
  Player *plr;
};

void Registration_::registrate() {
  std::unique_ptr<HTTP> hh{Client::connect(plr, tx.get(), this)};
  if (hh.get() == nullptr) { 
    Message::quick(this,
		   "Couldn't connect to server.",
		   "Sorry", "", PICS XICON POP);
    return;
  }

  /* XXX again, need a better way to detect this */
  if (plr->name == "Default") {
    Message::quick(this, 
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

    if (Client::rpc(hh.get(), REGISTER_RPC, 
		    (string) "seql=" + itos(seql) +
		    (string)"&seqh=" + itos(seqh) +
		    (string)"&name=" + httputil::urlencode(plr->name),
		    res)) {
      
      int id = util::stoi(res);
      if (id) {
	plr->webid = id;
	plr->webseql = seql;
	plr->webseqh = seqh;

	if (plr->writefile()) {
	  say((string)"success! " GREEN + res);
	  Message::quick(this,
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

  Message::no(this, 
	      "Unable to register:\n"
	      "     " RED + res + POP "\n"
	      "   Try again later!");
		 
  return;
}

void Registration_::screenresize() {
  /* XXX resize tx */
}

void Registration_::draw() {
  sdlutil::clearsurface(screen, BGCOLOR);
  tx->draw();
}

}  // namespace

Registration *Registration::Create(Player *p) {
  Registration_ *r = new Registration_();
  r->plr = p;
  r->tx.reset(TextScroll::Create(fon));
  return r;
}
