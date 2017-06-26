// [EPoCH]=-------------------------------------------------------------------
//                     GAMEREC - records for Game Requests
// [gamerec.cpp]=-------------------------------------------------------------

#include "gamerec.h"
#include "server.h"
#include "tom.h"
#include "epoch.h"

gamerec::gamerec() {
   cout << "Default Gamerec. Don't Call me!!\n";
}
gamerec::gamerec(short to_, short from_, char gametype_, short overwrite_, 
                long timeout_, epochgame * game_) {
   to = to_; from=from_; gametype=gametype_; overwrite=overwrite_;
   timeout=timeout_;game=game_;
}

gamerec::~gamerec() {
}

void sendrequest(short src,short dest,short ngame,char playagain,char gametype,string args){
   sendcommand(dest, "+"+binword(src)+binword(ngame)+pad(player[src]->name,10)
               +playagain+gametype+args);
}
