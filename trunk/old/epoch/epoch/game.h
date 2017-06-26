#ifndef __WM_GAME_H
#define __WM_GAME_H

#include "epoch.h"

struct egelement {
   eplayer * player; 
   egelement * next;
};

struct opponent {
   short id;
   short idx;
   eplayer * player;
};

class epochgame {
public:
   epochgame();
   ~epochgame();
   virtual void playeradded(int idx, string name);
   virtual void playerdropped(int idx, string name);

   virtual void spectatoradded(int idx, string name);
   virtual void spectatordropped(int idx, string name);  

   virtual void cleargame();

   virtual int addplayer(int idx);
   virtual int dropplayer(int idx);

   virtual void addspectator(int idx);
   virtual void dropspectator(int idx);

   virtual int isplayer(int id);
   virtual int isingame(int idx);

   virtual int validargs(string);

   virtual void parse(string);

//protected:
   void recursedelete(egelement *&, int);

   opponent    oppt[3];// players array (one on one)
   egelement * plhead; // players linked-list
   egelement * sphead; // spectators linked-list

};
int validgametype(unsigned short);
epochgame * newgame(const char);
#endif
