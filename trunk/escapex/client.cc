
#include "client.h"
#include "escapex.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"

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
