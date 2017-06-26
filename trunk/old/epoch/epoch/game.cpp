// [EPoCH]=-------------------------------------------------------------------
//                      GAME - The class and misc functions for Epoch Games
// [game.cpp]=----------------------------------------------------------------

#include "game.h"
#include "tom.h"
#include "epoch.h"
#include "server.h"
#include "games.h"

#define LASTGAMETYPE 1               // yeh

epochgame::epochgame() {
    sphead = plhead = NULL;
    oppt[1].id = oppt[1].idx = 0; oppt[1].player = NULL; 
    oppt[2].id = oppt[2].idx = 0; oppt[2].player = NULL;     
}
epochgame::~epochgame() {
    egelement * tmp;
    // Free our lists. This does not delete the actual players, just the
    // elements (64 bytes each).
    while (sphead) { tmp = sphead; sphead = sphead->next ; delete tmp; }
    while (plhead) { tmp = plhead; plhead = plhead->next ; delete tmp; }
}

void epochgame::spectatordropped(int idx, string name) {
;
}
void epochgame::spectatoradded(int idx, string name) {
;
}
   
void epochgame::playeradded(int idx, string name) {
   spectatoradded(idx,name);
}
void epochgame::playerdropped(int idx, string name){
   spectatordropped(idx,name);
}
void epochgame::cleargame() {
   cout << "[!!] Master cleargame() called...\n";
}

int epochgame::addplayer(int idx){
       if (!oppt[1].id) {
                oppt[1].player = player[idx];
                oppt[1].id = player[idx]->id;
                oppt[1].idx = idx;
       } else if (!oppt[2].id) {
                oppt[2].player = player[idx];
       		oppt[2].id = player[idx]->id;
                oppt[2].idx = idx;
       } else return 0;
       return 1; 
   }

int epochgame::dropplayer(int idx){
       ;
   }

void epochgame::addspectator(int idx) {
   // add to the Linked List (at head)
   egelement * newn = new egelement ((egelement) {player[idx], sphead});   
   sphead = newn;
}

void epochgame::dropspectator(int idx){

   if (!sphead) { info ("Delete when list empty..."); return; }
   recursedelete(sphead,  idx);   

}

void epochgame::recursedelete(egelement *& head, int idx) {
	if (!head) return;
	if (head->player == player[idx]) {
		delete head; head=NULL;
    }
    if (head->next && head->next->player == player[idx]) {
		egelement * tmp;
        tmp = head->next;
		head->next = head->next->next;
        delete tmp;
    } else recursedelete(head->next,idx);
}

int epochgame::isplayer(int id) {
    if (oppt[1].id == id || oppt[2].id == id) return 1;
}

int epochgame::isingame(int idx) {
    egelement * shead=sphead;
    if (oppt[1].player == player[idx] || oppt[2].player == player[idx]) return 1;
    while (shead) if (shead->player == player[idx]) return 1;
    return 0;
}

int epochgame::validargs(string arg) { // supposed to check if these are
				       // legitimate game request data
    return 1;       
}

int validgametype(unsigned short number) {
    // Checks if a gametype byte is in range. Right now just checks nonzero
    // and a Constant
    if (1 > number) return 0;
    if (number > LASTGAMETYPE) return 0; 
    return 1;
}

epochgame * newgame(const char gametype) {
     switch (gametype) { // can't really think of a slicker way to do this.
     case 1:
        return new tictactoe();
     break;
     default:
       fatal("Can't do that gametype. Didn't you check with validgametype() first?");
     break;
     }
}

void epochgame::parse (string msg) {
	cout << "["<<this<<"] I got :"<<esc(msg)<<endl;
}
