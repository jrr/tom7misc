
#include "client.h"
#include "escapex.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "draw.h"

bool Client::quick_rpc(Player *plr, string path, string query, string & ret) {
  quick_txdraw td;
  
  http * hh = Client::connect(plr, td.tx, &td);
  
  td.say("Connecting..");
  td.draw();

  if (!hh) { 
    Message::no(&td, "Couldn't connect!");
    ret = "Couldn't connect.";
    return false;
  }
  
  Extent<http> eh(hh);

  td.say("Sending command..");
  td.draw();
  
  return rpc(hh, path, query, ret);
}
