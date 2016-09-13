
#ifndef __CLIENT_H
#define __CLIENT_H

#include "drawable.h"
#include "textscroll.h"
#include "http.h"
#include "chars.h"
#include "message.h"
#include "util.h"
#include "player.h"
#include "prefs.h"
#include "draw.h"

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
  static HTTP *connect(Player *plr, TextScroll *tx, Drawable *that) {
    std::unique_ptr<HTTP> hh{HTTP::Create()};

    if (Prefs::getbool(plr, PREF_DEBUG_NET))
      hh->log_message = debug_log_message;

    string serveraddress = Prefs::getstring(plr, PREF_SERVER);
    int serverport =
      (Prefs::getbool(plr, PREF_ALTCONNECT))?8888:80;

    if (hh.get() == nullptr) {
      if (tx) tx->say(YELLOW "Couldn't create http object.");
      Message::Quick(that, "Upgrade failed!", "Cancel", "");
      return 0;
    }

    string ua = "Escape (" VERSION "; " PLATFORM ")";
    if (tx) tx->say((string)"This is: " + ua);

    hh->setua(ua);

    if (tx) tx->say((string)
                    "Connecting to " YELLOW + serveraddress +
                    WHITE ":" POP + itos(serverport) + POP "...");

    if (that) {
      that->draw();
      SDL_Flip(screen);
    }

    if (!hh->connect(serveraddress, serverport)) {
      if (tx) tx->say((string)RED "Couldn't connect to "
                      YELLOW + serveraddress + POP ".");
      Message::Quick(that, "Can't connect!", "Cancel", "");
      return 0;
    }

    return hh.release();
  }

  /* XX add bool quiet=true; when false show progress */
  static bool quick_rpc(Player *, string path, string query, string &ret);

  /* true on success */
  static bool rpc(HTTP *hh, string path, string query, string &ret) {
    string m;
    httpresult hr = hh->get(path + (string)"?" + query, m);

    if (hr == HT_OK) {

      if (m.length() >= 2 &&
          m[0] == 'o' &&
          m[1] == 'k') {

        /* drop first token */
        (void) util::chop(m);
        ret = util::losewhitel(m);
        return true;
      } else {
        ret = m;
        return false;
      }
    } else {
      ret = ("http request failed");
      return false;
    }
  }

  static bool rpcput(HTTP *hh, string path, formalist *fl, string &ret) {
    string m;
    httpresult hr = hh->put(path, fl, m);

    if (hr == HT_OK) {

      if (m.length() >= 2 &&
          m[0] == 'o' &&
          m[1] == 'k') {

        /* drop first token */
        (void) util::chop(m);
        ret = util::losewhitel(m);
        return true;
      } else {
        ret = m;
        return false;
      }

    } else if (hr == HT_404) {

      ret = "error code 404";
      return false;
    } else {
      ret = "error code (general)";
      return false;
    }
  }

  static void debug_log_message(const string &s) {
    FILE *f = fopen(HTTP_DEBUGFILE, "a");

    if (f) {
      fprintf(f, "%s", s.c_str());
      fclose(f);
    }
  }

  /* need the drawable to draw background, too */
  /* static */
  struct quick_txdraw : public Drawable {
    std::unique_ptr<TextScroll> tx;
    quick_txdraw() {
      tx.reset(TextScroll::Create(fon));
      tx->posx = 5;
      tx->posy = 5;
      tx->width = screen->w - 10;
      tx->height = screen->h - 10;
    }

    void say(string s) { tx->say(s); }
    void draw() override {
      sdlutil::clearsurface(screen, BGCOLOR);
      tx->draw();
    }
    void screenresize() override {
      tx->screenresize();
    }
  };

};

#endif
