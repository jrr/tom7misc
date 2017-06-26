#include <string>
#include <iostream.h>

#include "tom.h"
#include "epoch.h"
#include "parse.h"
#include "server.h"
#include "game.h"
#include "gamerec.h"
#include "time.h"

#define PARSEDEVEL

extern eplayer * player[];

int login (int,string);
void sendinfo(int dest, int who);
inline string bindword(unsigned long);
inline string bytey(char x);

void parsecommand (string msg, int idx) {

unsigned int destnum,
             overwrite,
             poop;
unsigned char gametype;

string args;
#ifdef PARSEDEVEL
    cout << "[" << idx << "]: " << esc(msg) << endl; 
#endif

// verify their login status.

if (msg.length()==0) { disconnect(idx); return; }

// Note: check for packets shorter than their necessary length.

if (!(player[idx]->flags & FL_LOGGEDIN)) {
  switch (player[idx]->loginstage) {
  case LS_NOTHING:
  case LS_VERSION:
     if (msg[0] == 'v' && msg.length() == 5) {
       // sounds good to me.
       player[idx]->loginstage = LS_VERIFY;
       sendcommand(idx,"Ov");
     } else disconnect(idx);
     return;
  case LS_VERIFY:
  case LS_USER:
     if (msg[0] == 'L' && msg.length() != 1) {
       strncpy(player[idx]->name, (char *)(msg.c_str() + 1),10);
       player[idx]->name[10]=0;
       if (!player[idx]->name[0]) disconnect(idx);
       player[idx]->loginstage = LS_PASS;
       sendcommand(idx,"OL");
     } else disconnect(idx);
     return;
  case LS_PASS:
     if (msg[0] == 'P' && msg.length() != 1) {
       string password = (char *)(msg.c_str()+1);
       if (login(idx, password)) {       
          player[idx]->loginstage = LS_LOGGEDIN;
          player[idx]->flags |= FL_LOGGEDIN;
          sendcommand(idx,"OP");
       } else {
          sendcommand(idx,"NP\x01");
          disconnect(idx);
       } 
       return;
     } else disconnect(idx);
  }
return;
}

switch (msg[0]) {
  case 'G':
        if (msg.length() < 4) { disconnect(idx); return; }
	overwrite = wordbin(msg[1],msg[2]);
	if (overwrite > MAX_GAMES) { sendcommand(idx,"NG\x03"); disconnect(idx); return; }
	if (game[overwrite]->isingame(idx)) game[overwrite]->parse((string)(char *)(msg.c_str+3));
	break;
  case '-': break; // ignored
  case '&':
  	msg[0] = '#'; // send it Right Back.
	sendcommand(idx,msg);
  break;
  case ']':
        // someone joining Find Mode
        if (shuttingdown) {
          // can't join when in Shutdown mode.
          sendcommand(idx,"]\x01");return;
        } else {
	// broadcast everyone who's in here to the person.
      	for (int j=1;j<MAX_CLIENTS;j++)
 		if (player[j] && (player[j]->flags & FL_INGAME0))  {
                  sendcommand(idx,"]"+binword(j)+string(player[j]->name));
 		  sendinfo(idx,j);
		  sendcommand(j,"]"+binword(idx)+string(player[idx]->name));
		  sendinfo(j,idx);
		}
        player[idx]->flags |= FL_INGAME0;
        }
        // send attributes too.
  break;
  case '[':
        player[idx]->flags &= ~FL_INGAME0;
      	for (int j=1;j<MAX_CLIENTS;j++)
 		if (player[j] && (player[j]->flags & FL_INGAME0)) {
		  sendcommand(j,"["+binword(idx)+string(player[idx]->name));
                }
  break;
  case '#':
  break;
  case 'x':
	disconnect(idx); // thanks for telling us. =)
  break;
  case '?':
        if (msg.length() < 3) { disconnect(idx); return; }
	destnum = wordbin(msg[1],msg[2]);
        if (destnum>MAX_CLIENTS || !destnum) { sendcommand(idx,"N?\x02"); disconnect(idx); return; }
        if (player[destnum]) sendinfo(idx, destnum);
        else sendcommand(idx,"N?\x01");
  break;
  case 'J':
	// ok
	overwrite = wordbin(msg[1],msg[2]);
	if (overwrite > MAX_GAMES) { disconnect(idx); }
	if (!game[overwrite]) { sendcommand(idx,"NJ\x01"); return; }

	if (game[overwrite]->isplayer(player[idx]->id)) {
		game[overwrite]->addplayer(idx);
	} else {
		// No spectators check
		game[overwrite]->addspectator(idx);
	}
  break;
  case 'L':
  case 'v':
  case 'P':
       info (idx, "*** Login stuff out of Context");
       disconnect(idx);
  break;
  case '!': // accept gamerequest
        if (msg.length() < 3) { disconnect(idx); return; }
	overwrite = wordbin(msg[1],msg[2]);
	if (overwrite > MAX_GAMEREC) { sendcommand(idx,"N!\x02"); disconnect(idx); return; }
	if (!grequest[overwrite]) { sendcommand(idx,"N!\x01"); return; }
	if (idx != grequest[overwrite]->to) { sendcommand (idx, "N!\x04"); return; }
	if (!player[grequest[overwrite]->from]) { sendcommand (idx, "N!\x03"); return; }
	// ok.
	grequest[overwrite]->game->cleargame();
	grequest[overwrite]->game->addplayer(grequest[overwrite]->from);
	grequest[overwrite]->game->addplayer(idx);
	// make a game for it.
	for (int x=1;x<MAX_GAMES;x++)
		if (!game[x]) {
			game[x] = grequest[overwrite]->game;
			sendcommand(idx,"J"+bytey(grequest[overwrite]->gametype)
					+binword(x));
			sendcommand(grequest[overwrite]->from,"J"+bytey(grequest[overwrite]->gametype)+binword(x));
			delete grequest[overwrite];
			grequest[overwrite]=0;
			return;
		}
	info ("Ran out of games!");
	sendcommand(grequest[overwrite]->to,"N!\x05");
	sendcommand(grequest[overwrite]->from,"N!\x05");
	delete grequest[overwrite]->game;
	delete grequest[overwrite];
	grequest[overwrite]=0;
  break;
  case '@':
        if (msg.length() < 4) { disconnect(idx); return; }
	overwrite =wordbin(msg[1],msg[2]);
	poop = msg[3];
	if (overwrite > MAX_GAMEREC) { sendcommand(idx,"N@\x02"); disconnect(idx); return; }
        if (!grequest[overwrite]) { sendcommand(idx,"N@\x01"); disconnect(idx); return; }	
	if (grequest[overwrite]->to == idx) {
		if (poop) {
			if (!player[grequest[overwrite]->from]) 
	        sendcommand(idx,"N@\x04"); 
			else
		sendcommand(grequest[overwrite]->from,
			"@"+binword(overwrite)+string(player[idx]->name));
		delete grequest[overwrite]->game;
		delete grequest[overwrite];
		grequest[overwrite] = 0; return;
		} 
        } else { sendcommand(idx,"N@\x03"); return; }
  break;
  case '+':
        if (msg.length() < 7) { disconnect(idx); return; }
       destnum = wordbin(msg[1],msg[2]);
       gametype = msg[3];
       overwrite = wordbin(msg[4],msg[5]);
       args = msg.substr(6,msg.length()-6);
       if (!validgametype(gametype) || destnum > MAX_CLIENTS || destnum < 1) {
           sendcommand(idx,"N+\x04");
           disconnect(idx);
           return;
       }
       if (shuttingdown) {sendcommand(idx,"N+\x06"); return;}
       if (!player[destnum]) {sendcommand(idx,"N+\x05"); return;}
       if (!(player[destnum]->gameflags & (1<<gametype))) {
			    sendcommand(idx,"N+\x03"); return;}
       if ((player[idx]->flags & FL_GUEST) && (!(player[destnum]->flags & FL_ACCEPTG))) {
                            sendcommand(idx,"N+\x02"); return; }
       if (!(player[destnum]->flags & FL_ACCEPT)) {
			    sendcommand(idx,"N+\x01"); return; }
       if (overwrite) {
           // make sure they own this game
           if ((!game[overwrite]) || 
              (!(game[overwrite]->isplayer(player[idx]->id)
           && game[overwrite]->isplayer(player[destnum]->id))))
              { sendcommand(idx, "N+\x08"); return; }
       }
       // ############ CHECK FOR FLOODS ############
       
       // everything seems In Order... 
       int ngame;
       for (ngame=0;ngame<MAX_GAMEREC;ngame++)
           if (!grequest[ngame]) {
              // All set. Hook up!
              grequest[ngame] = new gamerec(destnum, idx, gametype,
                                overwrite, time(NULL)+GAMETIMEOUT,((overwrite)?
                                    game[overwrite]:newgame(gametype)));
              if (!grequest[ngame]->game->validargs(args)) { 
				sendcommand(idx,"N+\x09"); disconnect(idx);
				delete(grequest[ngame]->game);
				delete(grequest[ngame]);
				grequest[ngame]=0; return; }
	      // send it out:
              sendrequest(idx,destnum,ngame,(overwrite)?1:0,gametype,args);
           return;
           }
       info("GameRequests full!");
       sendcommand(idx,"N+\x07"); return;
  break;
  default:
#ifdef PARSEDEVEL
  cout << "[" << idx << "] sent unknown command " << msg[0] << ".\n";
#else // standard use
  info("Invalid command.", idx);
  disconnect(idx);  
#endif
  }
}

int login (int idx, string password) {
  cout << "[??] Username: " << player[idx]->name << "; password: " << password << "...\n";
  player[idx]->flags |= (FL_ACCEPT | FL_ACCEPTG);
  player[idx]->gameflags = 0xFFFFFFFF; // all on.
  // later actually check their flags from a file...
  return 1;
}

void sendinfo(int dest, int who) {
   // send 'A' packet
	sendcommand(dest,"AA"+bindword(player[who]->flags)
	+binword(player[who]->rank)+bindword(player[who]->gameflags)
        +bindword(player[who]->gameprefs));
}

inline string bindword(unsigned long in) {
   return (binword(in>>16)+binword(in&0x0000FFFF));
}

inline string bytey(char x) {
	string m = " ";
	m[0]=x;
	return m;
}
