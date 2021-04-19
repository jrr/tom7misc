
#include "client.h"

#include <string>
#include <memory>

#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"

#include "escapex.h"
#include "draw.h"
#include "http.h"
#include "player.h"
#include "drawable.h"

HTTP *Client::Connect(Player *plr, TextScroll *tx, Drawable *that) {
  std::unique_ptr<HTTP> hh{HTTP::Create()};

  if (Prefs::getbool(plr, PREF_DEBUG_NET))
    hh->log_message = DebugLogMessage;

  string serveraddress = Prefs::getstring(plr, PREF_SERVER);
  int serverport =
    Prefs::getbool(plr, PREF_ALTCONNECT) ? 8888 : 80;

  if (hh.get() == nullptr) {
    if (tx) tx->Say(YELLOW "Couldn't create http object.");
    Message::Quick(that, "Upgrade failed!", "Cancel", "");
    return 0;
  }

  string ua = "Escape (" VERSION "; " PLATFORM ")";
  if (tx) tx->Say((string)"This is: " + ua);

  hh->setua(ua);

  if (tx) tx->Say((string)
                  "Connecting to " YELLOW + serveraddress +
                  WHITE ":" POP + Util::itos(serverport) + POP "...");

  if (that) {
    that->Draw();
    SDL_Flip(screen);
  }

  if (!hh->connect(serveraddress, serverport)) {
    if (tx) tx->Say((string)RED "Couldn't connect to "
                    YELLOW + serveraddress + POP ".");
    Message::Quick(that, "Can't connect!", "Cancel", "");
    return 0;
  }

  return hh.release();
}

bool Client::RPC(HTTP *hh, const string &path, const string &query,
                 string &ret) {
  string m;
  HTTPResult hr = hh->get(path + (string)"?" + query, m);

  if (hr == HTTPResult::OK) {

    if (m.length() >= 2 &&
        m[0] == 'o' &&
        m[1] == 'k') {

      /* drop first token */
      (void) EscapeUtil::chop(m);
      ret = EscapeUtil::losewhitel(m);
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

bool Client::QuickRPC(Player *plr, const string &path, const string &query,
		      string &ret) {
  QuickTxDraw td;

  std::unique_ptr<HTTP> hh{Client::Connect(plr, td.tx.get(), &td)};

  td.say("Connecting..");
  td.Draw();

  if (hh.get() == nullptr) {
    Message::No(&td, "Couldn't connect!");
    ret = "Couldn't connect.";
    return false;
  }

  td.say("Sending command..");
  td.Draw();

  return RPC(hh.get(), path, query, ret);
}

bool Client::RPCPut(HTTP *hh, const string &path,
                    const std::vector<FormEntry> &fl,
                    string &ret) {
  string m;
  HTTPResult hr = hh->Put(path, fl, m);

  if (hr == HTTPResult::OK) {

    if (m.length() >= 2 &&
        m[0] == 'o' &&
        m[1] == 'k') {

      /* drop first token */
      (void) EscapeUtil::chop(m);
      ret = EscapeUtil::losewhitel(m);
      return true;
    } else {
      ret = m;
      return false;
    }

  } else if (hr == HTTPResult::ERROR_404) {

    ret = "error code 404";
    return false;
  } else {
    ret = "error code (general)";
    return false;
  }
}

void Client::DebugLogMessage(const string &s) {
  FILE *f = fopen(HTTP_DEBUGFILE, "a");
  
  if (f) {
    fprintf(f, "%s", s.c_str());
    fclose(f);
  }
}
