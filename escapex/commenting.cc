
#include "commenting.h"

#include <string>

#include "../cc-lib/crypt/md5.h"
#include "../cc-lib/util.h"
#include "../cc-lib/base/stringprintf.h"

#include "escapex.h"
#include "rating.h"

#include "player.h"
#include "draw.h"
#include "chars.h"
#include "message.h"
#include "menu.h"
#include "textbox.h"

#include "client.h"
#include "httputil.h"

namespace {
struct CScreen : public Drawable {
  void Draw() override {
    sdlutil::clearsurface(screen, BGCOLOR);

    fon->draw(2, 2, (string)(BLUE "Commenting on: " POP YELLOW) +
              lev->title + POP);

    /* draw level thumbnail... */

    Uint32 color =
      SDL_MapRGBA(screen->format, 0x22, 0x22, 0x44, 0xFF);
    int margin = Drawing::SmallHeight() + 16;
    int y = (screen->h - margin) + 4;
    Drawing::DrawSmall(y,
                       margin,
                       color,
                       lev, nsolved,
                       MD5::Ascii(levmd5), 0, 0);

    if (tx.get() != nullptr) tx->Draw();
  }

  void ScreenResize() override {
    /* XXX */
  }

  void Redraw() {
    Draw();
    SDL_Flip(screen);
  }

  const Level *lev = nullptr;
  int nsolved = 0;
  string levmd5;

  std::unique_ptr<TextScroll> tx;

  CScreen() {
    tx.reset(TextScroll::Create(fon));
    tx->posx = 2;
    tx->posy = fon->height + 2;
    tx->width = screen->w - 4;
    tx->height = screen->h -
      (Drawing::SmallHeight() + fon->height + 24);
  }
};
}  // namespace

void CommentScreen::Comment(Player *p, const Level *lev, const string &md5,
                            bool cookmode) {
  CScreen cs;
  cs.lev = lev;
  cs.levmd5 = md5;

  cs.nsolved = p->GetSolLength(md5);

  cs.Redraw();

  /* XXX if cookmode, then we have already just connected.. */

  /* make sure that we can access the server, since we will lose
     a comment that we compose if it can't be posted. */

  cs.tx->Say(GREY "Making sure we're connected...");
  cs.Redraw();
  std::unique_ptr<HTTP> hh{Client::Connect(p, cs.tx.get(), &cs)};
  if (hh.get() == nullptr) {
    Message::Quick(&cs, "Can't connect to internet!",
                   "OK", "", PICS XICON POP);
    return;
  }

  {
    string res;
    bool success =
      Client::RPC(hh.get(), PING_RPC,
                  StringPrintf("id=%d"
                               "&seql=%d"
                               "&seqh=%d"
                               "&md=%s",
                               p->webid,
                               p->webseql,
                               p->webseqh,
                               MD5::Ascii(md5).c_str()),
                  res);

    if (!success) {
      Message::Quick(&cs,
                     "Ping to server failed! "
                     "Perhaps this level is not uploaded.",
                     "OK", "", PICS XICON POP);
      return;
    }

  }

  cs.tx->Say(GREY "ok.");
  cs.Redraw();

  Label levname;
  levname.text = lev->title;
  Label author;
  author.text = (string)"  by " + lev->author;

  static constexpr int IND = 2;

  TextBox body(50, 13);
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

  Toggle spoiler;
  spoiler.indent = IND;
  spoiler.disabled = cookmode;
  spoiler.checked = cookmode;
  spoiler.question = cookmode?"Spoiler -- Cooked":"Spoiler";
  spoiler.explanation =
    "Does this comment give away any information towards a solution\n"
    "to the level, or some other surprise best left to be discovered\n"
    "by the player? If so, then it has spoilers, so mark that here.";

  Okay ok;
  ok.text = "Submit Comment";

  Cancel can;

  std::unique_ptr<Menu> mm =
    Menu::Create(&cs, cookmode ? "Explain your cook" : "Leave a comment",
                 {&body, &spoiler, &ok, &can},
		 false);

  mm->yoffset = fon->height + 4;
  mm->alpha = 230;

  /* XXX look for InputResultKind::QUIT too */
  if (InputResultKind::OK == mm->menuize()) {
    string com = body.get_text();

    string res;
    bool success =
      Client::RPC(hh.get(), COMMENT_RPC,
                  StringPrintf(
                      "id=%d"
                      "&seql=%d"
                      "&seqh=%d"
                      "&md=%s"
                      "&comment=%s"
                      "&spoiler=%d",
                      p->webid,
                      p->webseql,
                      p->webseqh,
                      MD5::Ascii(md5).c_str(),
                      HTTPUtil::URLEncode(com).c_str(),
                      (int)!!spoiler.checked),
                  res);

    if (success) {
      Message::Quick(&cs, "posted: " + Font::truncate(res, 60),
                     "OK", "", PICS THUMBICON POP);
    } else {
      /* XXX copy to clipboard? */
      Message::Quick(&cs, "failed: " + Font::truncate(res, 60),
                     "comment lost =(", "", PICS XICON POP);
    }

  }

}
