#ifndef __WM_SERVER_H
#define __WM_SERVER_H

#include "gamerec.h"

#define MAX_GAMES 1024
#define MAX_CLIENTS 1024
#define MAX_GAMEREC 512

void disconnect (int);
void sendcommand(int,string);
void findbroadcast(string);

extern eplayer * player[MAX_CLIENTS];
extern bool shuttingdown;
extern gamerec * grequest[MAX_GAMEREC];
extern epochgame * game[MAX_GAMES];
#endif
