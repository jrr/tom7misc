// [EPoCH]=-------------------------------------------------------------------
//                 TIC-TAC-TOE : Simple game to practice with.
// [tictactoe.cpp]=-----------------------------------------------------------

#include "game.h"
#include "tom.h"
#include "epoch.h"
#include "server.h"
#include "tictactoe.h"

//tictactoe::tictactoe() {}
tictactoe::tictactoe():epochgame() {}
tictactoe::~tictactoe() {}

void tictactoe::cleargame() {
   for (int y=0;y<3;y++)
    for (int x=0;x<3;x++)
      board[y][x] = 0;
   score[1] = score[2] = 0;
}

int tictactoe::validargs(string args) {
   // check if these arguments are valid
   cout << "Valid...? ok." << endl; return 1;
}
