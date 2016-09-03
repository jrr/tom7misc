
#include "escapex.h"
#include "rating.h"

#include "player.h"
#include "extent.h"
#include "draw.h"
#include "chars.h"
#include "message.h"
#include "../cc-lib/md5.h"
#include "menu.h"

#include "client.h"
#include "commenting.h"
#include "httputil.h"

namespace {
struct cscreen : public Drawable {
  void draw() override {
    sdlutil::clearsurface(screen, BGCOLOR);

    fon->draw(2, 2, (string)(BLUE "Commenting on: " POP YELLOW) +
	      lev->title + POP);

    /* draw level thumbnail... */

    Uint32 color = 
      SDL_MapRGBA(screen->format, 0x22, 0x22, 0x44, 0xFF);
    int margin = Drawing::smallheight() + 16;
    int y = (screen->h - margin) + 4 ;
    Drawing::drawsmall(y,
		       margin,
		       color,
		       lev, nsolved, 
		       MD5::Ascii(levmd5), 0, 0);

    if (tx.get() != nullptr) tx->draw();
  }

  void screenresize() override {
    /* XXX */
  }

  void redraw() {
    draw();
    SDL_Flip(screen);
  }

  Level *lev;
  int nsolved;
  string levmd5;

  std::unique_ptr<TextScroll> tx;

  cscreen() {
    tx.reset(TextScroll::Create(fon));
    tx->posx = 2;
    tx->posy = fon->height + 2;
    tx->width = screen->w - 4;
    tx->height = screen->h - 
      (Drawing::smallheight() + fon->height + 24);
  }
};
}  // namespace

void CommentScreen::comment(Player *p, Level *lev, string md5,
			    bool cookmode) {
  cscreen cs;
  cs.lev = lev;
  cs.levmd5 = md5;
  
  Solution *sol = p->getsol(md5);
  cs.nsolved = sol?sol->length:0;


  cs.redraw();

  /* XXX if cookmode, then we have already just connected.. */

  /* make sure that we can access the server, since we will lose
     a comment that we compose if it can't be posted. */

  cs.tx->say(GREY "Making sure we're connected...");
  cs.redraw();
  std::unique_ptr<HTTP> hh{Client::connect(p, cs.tx.get(), &cs)};
  if (hh.get() == nullptr) {
    Message::quick(&cs, "Can't connect to internet!",
		   "OK", "", PICS XICON POP);
    return;
  }
  
  {
    string res;
    bool success = 
      Client::rpc(hh.get(), PING_RPC,
		  /* credentials */
		  (string)"id=" +
		  itos(p->webid) + 
		  (string)"&seql=" +
		  itos(p->webseql) +
		  (string)"&seqh=" +
		  itos(p->webseqh) +
		  (string)"&md=" +
		  MD5::Ascii(md5),
		  res);

    if (!success) {
      Message::quick(&cs, 
		     "Ping to server failed! "
		     "Perhaps this level is not uploaded.",
		     "OK", "", PICS XICON POP);
      return;
    }

  }

  cs.tx->say(GREY "ok.");
  cs.redraw();

  label levname;
  levname.text = lev->title;
  label author;
  author.text = (string)"  by " + lev->author;

  int IND = 2;

  textbox body(50, 13);
  body.indent = IND;
  body.question = 
    cookmode?"Please describe your cook.":"Your comment on this level:";
  body.explanation = 
    cookmode?"Describe your cook and how it differs from the solution\n"
             "you believe the author intended.":
             "Leave your comment on this level.\n";
  if (cookmode) {
    body.set_text("Cook: ");
    body.goto_end();
    /* XXX put cursor at end */
  }

  toggle spoiler;
  spoiler.indent = IND;
  spoiler.disabled = cookmode;
  spoiler.checked = cookmode;
  spoiler.question = cookmode?"Spoiler -- Cooked":"Spoiler";
  spoiler.explanation =
    "Does this comment give away any information towards a solution\n"
    "to the level, or some other surprise best left to be discovered\n"
    "by the player? If so, then it has spoilers, so mark that here.";

  okay ok;
  ok.text = "Submit Comment";

  cancel can;
  can.text = "Cancel";
  
  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spoiler);
  PtrList<MenuItem>::push(l, &body);

  menu *mm = menu::create(&cs, cookmode?"Explain your cook":"Leave a comment", l, false);
  Extent<menu> em(mm);
  PtrList<MenuItem>::diminish(l);

  mm->yoffset = fon->height + 4;
  mm->alpha = 230;


  /* XXX look for MR_QUIT too */
  if (MR_OK == mm->menuize()) {
    string com = body.get_text();

    string res;
    bool success = 
      Client::rpc(hh.get(), COMMENT_RPC,
		  /* credentials */
		  (string)"id=" + itos(p->webid) + 
		  (string)"&seql=" + itos(p->webseql) +
		  (string)"&seqh=" + itos(p->webseqh) +
		  (string)"&md=" + MD5::Ascii(md5) +
		  (string)"&comment=" + httputil::urlencode(com) +
		  (string)"&spoiler=" + itos(!!spoiler.checked),
		  res);

    if (success) {
      Message::quick(&cs, "posted: " + font::truncate(res, 60), 
		     "OK", "", PICS THUMBICON POP);
    } else {
      /* XXX copy to clipboard? */
      Message::quick(&cs, "failed: " + font::truncate(res, 60), 
		     "comment lost =(", "", PICS XICON POP);
    }
   
  }

}
