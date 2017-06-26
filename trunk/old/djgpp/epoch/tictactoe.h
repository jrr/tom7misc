#ifndef __WM_GAME_TICTACTOE_H
#define __WM_GAME_TICTACTOE_H

#include "epoch.h"
#include "game.h"

class tictactoe : public epochgame {
public:
   tictactoe();
   ~tictactoe();

   virtual void cleargame();

   virtual int validargs(string);
   
   int board[3][3];
   int score[3];
};
#endif
