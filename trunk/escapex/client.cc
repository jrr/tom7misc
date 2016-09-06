
#include "client.h"
#include "escapex.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"

bool Client::quick_rpc(Player *plr, string path, string query, string &ret) {
  quick_txdraw td;

  std::unique_ptr<HTTP> hh{Client::connect(plr, td.tx.get(), &td)};

  td.say("Connecting..");
  td.draw();

  if (hh.get() == nullptr) {
    Message::no(&td, "Couldn't connect!");
    ret = "Couldn't connect.";
    return false;
  }

  td.say("Sending command..");
  td.draw();

  return rpc(hh.get(), path, query, ret);
}
