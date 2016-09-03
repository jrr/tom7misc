
#include "upload.h"
#include "client.h"
#include "http.h"
#include "extent.h"
#include "message.h"
#include "draw.h"
#include "../cc-lib/md5.h"
#include "optimize.h"

namespace {

struct Upload_ : public Upload {
  static Upload_ *Create();

  UploadResult Up(Player *p, string file, string) override;

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

  void draw() override;
  void screenresize() override;
};

Upload_ *Upload_::Create() {
  std::unique_ptr<Upload_> ur{new Upload_};

  ur->tx.reset(TextScroll::Create(fon));
  if (ur->tx.get() == nullptr) return nullptr;

  ur->tx->posx = 5;
  ur->tx->posy = 5;
  ur->tx->width = screen->w - 10;
  ur->tx->height = screen->h - 10;

  return ur.release();
}

UploadResult Upload_::Up(Player *p, string f, string text) {
  redraw();

  string levcont = util::readfilemagic(f, LEVELMAGIC);

  plr = p;

  Level *lev = Level::fromstring(levcont);
  if (!lev) return UploadResult::FAIL;

  Extent<Level> el(lev);
  
  string md5c = MD5::Hash(levcont);


  /* don't free soln */
  Solution *slong;
  if (! ((slong = plr->getsol(md5c)) && 
      Level::verify(lev, slong)))
    return UploadResult::FAIL; /* no solution?? */

  say(GREEN "Level and solution ok." POP);
  say("Optimizing...");

  Solution *opt = Optimize::opt(lev, slong);
  if (!opt) {
    say(RED "optimization failed" POP);
    return UploadResult::FAIL;
  }
  Extent<Solution> es(opt);

  say(YELLOW + itos(slong->length) + GREY " " LRARROW " " POP +
      itos(opt->length) + POP);

  HTTP *hh = Client::connect(plr, tx.get(), this);

  if (!hh) return UploadResult::FAIL;

  string solcont = opt->tostring();

  formalist *fl = nullptr;

  /* XXX seems necessary! bug in aphasia cgi? */
  formalist::pusharg(fl, "dummy", "dummy");
  formalist::pusharg(fl, "id", itos(plr->webid));
  formalist::pusharg(fl, "seql", itos(plr->webseql));
  formalist::pusharg(fl, "seqh", itos(plr->webseqh));
  formalist::pusharg(fl, "text", text);
  formalist::pushfile(fl, "lev", "lev.esx", levcont);
  formalist::pushfile(fl, "sol", "sol.esx", solcont);

  redraw();

  string out;
  if (Client::rpcput(hh, UPLOAD_RPC, fl, out)) {
    Message::quick(this, GREEN "success!" POP,
		   "OK", "", PICS THUMBICON);
    formalist::diminish(fl);

    return UploadResult::OK;
  } else {
    Message::no(this, RED "Upload failed: " + 
		out + POP);
    formalist::diminish(fl);

    return UploadResult::FAIL;
  }

}

void Upload_::screenresize() {
  /* XXX resize */
}

void Upload_::draw() {
  sdlutil::clearsurface(screen, BGCOLOR);
  tx->draw();
}

}  // namespace

Upload::~Upload() {}

Upload *Upload::Create() {
  return Upload_::Create();
}
