
#include "escapex.h"
#include "rating.h"

#include "player.h"
#include "draw.h"
#include "chars.h"
#include "message.h"
#include "../cc-lib/md5.h"
#include "menu.h"

#include "client.h"
#include "commenting.h"
#include "solutionuploading.h"

Rating *Rating::Create() {
  return new Rating();
}

string Rating::ToString() const {
  /* just use one hex digit for each. */
  string r = "aa    ";

  r[0] = ((difficulty & 0xF) << 4) | (style & 0xF);
  r[1] = ((rigidity & 0xF) << 4) | (cooked & 0xF);
  r[2] = r[3] = r[4] = r[5] = 0;

  return r;
}

Rating *Rating::FromString(const string &s) {
  if (s.length() != 6) return nullptr;

  Rating *rat = Create();

  if (!rat) return nullptr;

  rat->difficulty = (s[0] >> 4) & 0xF;
  rat->style = s[0] & 0xF;
  rat->rigidity = (s[1] >> 4) & 0xF;
  rat->cooked = s[1] & 0xF;

  return rat;
}

/* below is RateScreen */
namespace {

struct RateScreen_ : public RateScreen {
  static RateScreen_ *Create(Player *p, const Level *l, const string &levmd);

  void Rate() override;

  void Draw() override;
  void ScreenResize() override {}

  void SetMessage(const string &m) override { msg = m; }

 private:
  Player *plr;
  const Level *lev;

  string msg;

  std::unique_ptr<TextScroll> tx;
  string levmd5;
  /* number of moves solved in (0 = unsolved) */
  int nsolved;

  void Redraw() {
    Draw();
    SDL_Flip(screen);
  }

  /* current values for the rating. (or 0 if none yet) */
  Rating *rat;
};

RateScreen_ *RateScreen_::Create(
    Player *p, const Level *l, const string &levmd) {
  RateScreen_ *rr = new RateScreen_();
  if (!rr) return nullptr;

  rr->plr = p;
  rr->lev = l;
  rr->levmd5 = levmd;
  rr->nsolved = rr->plr->GetSolLength(levmd);

  /* might be 0, that's ok. */
  rr->rat = rr->plr->getrating(levmd);

  rr->below = 0;

  rr->tx.reset(TextScroll::Create(fon));
  rr->tx->posx = 2;
  rr->tx->posy = 2;
  rr->tx->width = screen->w - 4;
  rr->tx->height = screen->h - (Drawing::SmallHeight() + 24);

  return rr;
}

void RateScreen_::Draw() {
  if (below) {
    below->Draw();
  } else {
    /* clear back */
    sdlutil::clearsurface(screen, BGCOLOR);
  }

  fon->draw(2, 2, (string)(BLUE "Rating: " POP YELLOW) +
            lev->title + POP);

  fon->draw(2, 2 + 3 * fon->height, msg);

  /* draw level thumbnail... */

  const Uint32 color =
    SDL_MapRGBA(screen->format, 0x22, 0x22, 0x44, 0xFF);
  const int margin = Drawing::SmallHeight() + 16;
  const int y = (screen->h - margin) + 4;
  Drawing::DrawSmall(y,
                     margin,
                     color,
                     lev, nsolved,
                     MD5::Ascii(levmd5),
                     /* XX pass in current rating? */
                     0, 0);

  /* draw current rating? */

}

void RateScreen_::Rate() {
  /* XXX check that the player has registered */

  /* rat is the existing rating or 0 */

  Redraw();

  Label levname;
  levname.text = Font::pad(lev->title, 50);
  Label author;
  author.text = (string)"  by " + Font::pad(lev->author, 45);

  int IND = 2;

  Slider difficulty(0, 10, 22);
  difficulty.indent = IND;
  difficulty.pos = rat ? rat->difficulty : 5;
  difficulty.question = "Difficulty";
  difficulty.low = "easy";
  difficulty.high = "hard";
  difficulty.explanation =
    "Choose your rating for this level's difficulty.\n"
    "You can rate it even if you haven't solved the level yet.";

  Slider style(0, 10, 22);
  style.indent = IND;
  style.pos = rat ? rat->style : 5;
  style.question = "Style     ";
  style.low = "lame";
  style.high = "cool";
  style.explanation =
    "How do you rate the composition of this level?\n"
    "How interesting is it to play? How does it look?\n"
    "How different is it from previously created levels?\n";

  Slider rigidity(0, 10, 22);
  rigidity.indent = IND;
  rigidity.pos = rat ? rat->rigidity : 5;
  rigidity.question = "Rigidity  ";
  rigidity.low = "loose";
  rigidity.high = "tight";
  rigidity.explanation =
    "Did this level have many solutions (loose) or few (rigid)?\n"
    "Rigidity is not necessarily good or bad.";

  bool old_cooked = rat? (!! rat->cooked):false;

  Toggle cooked;
  cooked.indent = IND;
  cooked.disabled = nsolved==0;
  cooked.checked = old_cooked;
  cooked.question = "Cooked";
  cooked.explanation =
    "Do you think you solved this level the \"correct\" way?\n"
    "If you think you've found an alternate solution, then you've\n"
    "\"cooked\" it. Cooking really only applies to more rigid\n"
    "levels. (Please leave a comment describing the cook!)";

  Toggle solved;
  solved.indent = IND;
  solved.disabled = true;
  solved.checked = (nsolved>0);
  solved.question = "Solved " GREY "(set automatically)" POP;
  /* XXX can't ever see this.
     maybe disabled items should be selectable,
     but not modifiable? */
  solved.explanation =
    "Whether or not you've solved the level.\n"
    "This is set automatically.";

  Okay ok;
  ok.text = "Change Rating";

  Cancel can;

  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &solved);
  PtrList<MenuItem>::push(l, &cooked);
  PtrList<MenuItem>::push(l, &rigidity);
  PtrList<MenuItem>::push(l, &style);
  PtrList<MenuItem>::push(l, &difficulty);
  PtrList<MenuItem>::push(l, &author);
  PtrList<MenuItem>::push(l, &levname);

  std::unique_ptr<Menu> mm = Menu::Create(this, "Change Your Rating", l, false);

  /* XXX look for InputResultKind::QUIT too */
  if (InputResultKind::OK == mm->menuize()) {

    /* Send to the server. */

    /* rat, if nonzero, is owned by the player so we
       don't need to free it. putrating will overwrite
       it or create a new one, if necessary. */

    Rating *nr = Rating::Create();

    nr->difficulty = difficulty.pos;
    nr->style = style.pos;
    nr->rigidity = rigidity.pos;
    nr->cooked = cooked.checked;

    /* putrating takes ownership */
    plr->PutRating(levmd5, nr);
    plr->WriteFile();

    /* send message to server */
    std::unique_ptr<HTTP> hh{Client::Connect(plr, tx.get(), this)};

    string res;

    bool success =
      (hh.get() != nullptr) &&
      Client::RPC(hh.get(), RATE_RPC,
                  /* credentials */
                  (string)"id=" +
                  itos(plr->webid) +
                  (string)"&seql=" +
                  itos(plr->webseql) +
                  (string)"&seqh=" +
                  itos(plr->webseqh) +
                  /* rating */
                  (string)"&md=" + MD5::Ascii(levmd5) +
                  (string)"&diff=" + itos(nr->difficulty) +
                  (string)"&style=" + itos(nr->style) +
                  (string)"&rigid=" + itos(nr->rigidity) +
                  (string)"&cooked=" + itos((int)nr->cooked) +
                  (string)"&solved=" + itos((int)!!nsolved),
                  res);

    hh.reset();

    if (success) {
      int record = util::stoi(res);
      const Solution *ours = plr->GetSol(levmd5);
      if (plr->webid && (ours != nullptr) && ours->Length() < record) {
        /* beat the record! prompt to upload. */

        SolutionUploading::PromptUpload(
            nullptr, plr, levmd5, *ours,
            "You made a new speed record! " RED +
            itos(record) + POP " " LRARROW " " GREEN +
            itos(ours->Length()),
            "Speed Record",
            true);

      } else {

        /* XXX perhaps should prompt to upload solution
           (with comment) instead */
        /* first cook. prompt for comment. */
        if (nr->cooked && !old_cooked) {
          CommentScreen::Comment(plr, lev, levmd5, true);
        } else {
          /* only if no other pop-up */
          Message::Quick(this, "Rating sent!",
                         "OK", "", PICS THUMBICON POP);
        }
      }

    } else {
      Message::Quick(this, "Unable to send rating to server: " RED + res,
                     "Cancel", "", PICS XICON POP);
    }

  } /* otherwise do nothing */

  PtrList<MenuItem>::diminish(l);
}

}  // namespace

RateScreen *RateScreen::Create(Player *p, const Level *l, const string &levmd) {
  return RateScreen_::Create(p, l, levmd);
}
