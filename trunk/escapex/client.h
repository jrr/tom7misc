
#ifndef _ESCAPE_CLIENT_H
#define _ESCAPE_CLIENT_H

#include "drawable.h"
#include "textscroll.h"
#include "http.h"
#include "chars.h"
#include "message.h"
#include "escape-util.h"
#include "player.h"
#include "prefs.h"
#include "draw.h"
#include "../cc-lib/sdl/sdlutil.h"

#define SUPERUSER (1)

#define PING_RPC "/f/a/escape/ping"
#define COMMENT_RPC "/f/a/escape/comment"
#define RATE_RPC "/f/a/escape/rate"
#define REGISTER_RPC "/f/a/escape/register"
#define UPLOAD_RPC "/f/a/escape/upload"
#define UPLOADSOL_RPC "/f/a/escape/upsol"
#define DELETE_RPC "/f/a/escape/deletefromgame"

#define HTTP_DEBUGFILE "netdebug.txt"

struct Client {
  static HTTP *Connect(Player *plr, TextScroll *tx, Drawable *that);

  /* XX add bool quiet=true; when false show progress */
  static bool QuickRPC(Player *, const string &path,
		       const string &query, string &ret);

  /* true on success */
  static bool RPC(HTTP *hh, const string &path, const string &query,
                  string &ret);

  static bool RPCPut(HTTP *hh, const string &path,
                     const std::vector<FormEntry> &fl,
                     string &ret);
  
  static void DebugLogMessage(const string &s);
  
  /* need the drawable to draw background, too */
  /* static */
  struct QuickTxDraw : public Drawable {
    std::unique_ptr<TextScroll> tx;
    QuickTxDraw() {
      tx.reset(TextScroll::Create(fon));
      tx->posx = 5;
      tx->posy = 5;
      tx->width = screen->w - 10;
      tx->height = screen->h - 10;
    }

    void say(const string &s) { tx->Say(s); }
    void Draw() override {
      sdlutil::clearsurface(screen, BGCOLOR);
      tx->Draw();
    }
    void ScreenResize() override {
      tx->ScreenResize();
    }
  };

};

#endif
