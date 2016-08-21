#include "registration.h"
#include "client.h"
#include "extent.h"
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

  ~Registration_() override {
    tx->destroy();
  }

  void say(string s) {
    if (tx) {
      tx->say(s);
      redraw();
    }
  }

  TextScroll *tx;
  Player *plr;
};

void Registration_::registrate() {

  HTTP *hh = Client::connect(plr, tx, this);
  if (!hh) { 
    Message::quick(this,
		   "Couldn't connect to server.",
		   "Sorry", "", PICS XICON POP);
    return;
  }

  Extent<HTTP> eh(hh);

  /* XXX again, need a better way to detect this */
  if (plr->name == "Default") {
    Message::quick(this, 
		   "You can't register with the default player.",
		   "Sorry", "", PICS XICON POP);
    return;
  }

  int tries=2;

  string res;
  while (tries--) {
    int seql = util::random();
    int seqh = util::random() ^ (((int)SDL_GetTicks() << 16) |
				 ((int)SDL_GetTicks() >> 16));

    seql = abs(seql);
    seqh = abs(seqh);

    say("try " + itos(seql) + " " + itos(seqh) + (string)"...");

    if (Client::rpc(hh, REGISTER_RPC, 
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
  r->tx = TextScroll::create(fon);
  return r;
}
