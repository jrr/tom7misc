
#include "upload.h"

#include <string>
#include <memory>
#include <vector>

#include "../cc-lib/crypt/md5.h"
#include "../cc-lib/util.h"

#include "client.h"
#include "http.h"
#include "httputil.h"
#include "message.h"
#include "draw.h"
#include "optimize.h"
#include "escape-util.h"

namespace {

struct Upload_ : public Upload {
  static Upload_ *Create();

  UploadResult Up(Player *p, const string &file, const string &) override;

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
  Player *plr = nullptr;

  void Draw() override {
    sdlutil::clearsurface(screen, BGCOLOR);
    tx->Draw();
  }
  void ScreenResize() override {
    /* XXX resize */
  }
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

UploadResult Upload_::Up(Player *p, const string &f, const string &text) {
  Redraw();

  const string levcont = EscapeUtil::readfilemagic(f, LEVELMAGIC);

  plr = p;

  std::unique_ptr<Level> lev = Level::FromString(levcont);
  if (lev.get() == nullptr) return UploadResult::FAIL;

  string md5c = MD5::Hash(levcont);

  /* don't free soln */
  const Solution *slong = plr->GetSol(md5c);
  if (slong == nullptr || !Level::Verify(lev.get(), *slong))
    return UploadResult::FAIL; /* no solution?? */

  say(GREEN "Level and solution ok." POP);
  say("Optimizing...");

  Solution opt = Optimize::Opt(lev.get(), *slong);

  say(YELLOW + Util::itos(slong->Length()) + GREY " " LRARROW " " POP +
      Util::itos(opt.Length()) + POP);

  HTTP *hh = Client::Connect(plr, tx.get(), this);

  if (!hh) return UploadResult::FAIL;

  const string solcont = opt.ToString();

  vector<FormEntry> fl = {
    /* XXX seems necessary! bug in aphasia cgi? */
    FormEntry::Arg("dummy", "dummy"),
    FormEntry::Arg("id", Util::itos(plr->webid)),
    FormEntry::Arg("seql", Util::itos(plr->webseql)),
    FormEntry::Arg("seqh", Util::itos(plr->webseqh)),
    FormEntry::Arg("text", text),
    FormEntry::File("lev", "lev.esx", levcont),
    FormEntry::File("sol", "sol.esx", solcont),
  };
    
  Redraw();

  string out;
  if (Client::RPCPut(hh, UPLOAD_RPC, fl, out)) {
    Message::Quick(this, GREEN "success!" POP,
                   "OK", "", PICS THUMBICON);
    return UploadResult::OK;
  } else {
    Message::No(this, RED "Upload failed: " +
                out + POP);
    return UploadResult::FAIL;
  }

}

}  // namespace

Upload::~Upload() {}

Upload *Upload::Create() {
  return Upload_::Create();
}
