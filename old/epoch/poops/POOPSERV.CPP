// POOPserver
// A Free 'Squares implementation
// UnCopyright (u) 1997 Tom Murphy 7, All Rights Released.
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
#include "poopserv.h"
// #include "Srvr.hpp"
//---------------------------------------------------------------------------
#pragma link "Sock"
#pragma resource "*.dfm"

#define USERFILE "d:\\builder\\projects\\poopserv\\users.txt"

#define MIN(c,d) ((c<d) ? c : d)

#define FL_GUEST (1)
#define FL_GAME0 (1<<1)
#define FL_ADMIN (1<<2)
#define FL_ACCEPT (1<<3)
#define FL_CONNECTED (1<<4)

#define clOkay ((TColor)0x0000A000)
#define clMild ((TColor)0x0000C0C0)
#define clDanger ((TColor)0x000000C0)

TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
//   TSock * sucks[MAXSOCKS];
   int i,x,y;
/*   char * xk;
   AnsiString loo;
   loo = binword(256);
   xk = loo.c_str();
   log((AnsiString) intval(xk[0],xk[1]));
*/
   Sock1->PortName = PortName->Text;
   for (i = MAXUSERS; i--;) {
     folks[i].userid = 0;
     folks[i].ip[0] = 0;
     folks[i].username[0] = 0;
     folks[i].socket = 0;
     folks[i].incomingidx = 0;
     folks[i].chatgame = 0;
     folks[i].chatcount = 0;
     folks[i].flags = 0;
     }
   for (i = MAXGAMES; i--;) {
     for (x=0;x<8;x++)
       for (y=0;y<8;y++)
         games[i].board[x][y] = 0;
     games[i].turn = 0;
     games[i].players[1] = 0;
     games[i].players[2] = 0;
     games[i].score[1] = 0;
     games[i].score[2] = 0;
     for (x=0;x<10;x++) games[i].members[x] = 0;
   }
   for (i= 254;i--;) {
     requests[i].to=0;
     requests[i].from=0;
     requests[i].flags = 0;
     requests[i].overwrite =0;
   }
   readuserstruct();
   Button1Click(Button1); // Poop Server Activated!
}
void __fastcall TForm1::initNewSock(TSock * foo) {
    // do whatever shit here.
	// nothing, actually... =)
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
	Sock1->Listen = true;
    Sock1->AutoAccept = true;
    Form1->Caption = "Poop server Activated!!";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
	Sock1->Listen = false;
    Sock1->AutoAccept = false;
    Form1->Caption = "Poop server deactivated. =(";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Sock1Read(TObject *Sender, int Count)
{
//    AnsiString poopz = Sock1->Receive().AsString;
//    Application->MessageBox ("Got: " + poopz, "Yo", MB_OK);
//    AnsiString textin;
    int i;
    TSock *aSockpoo = ((TSock*)Sender);

    for (i=0; i <= MAXUSERS; i++) {
       if (aSockpoo == folks[i].socket) {
          // this is the right socket.
//          textin = folks[i].socket->Receive();
          DataIn(i);
          return;
       }
    }

 log("ERROR -- SOCKET NOT FOUND");

  }
//---------------------------------------------------------------------------
void __fastcall TForm1::DataIn(int folksidx) {
char * inst;
int amount, z;
AnsiString instringy;
instringy = folks[folksidx].socket->Receive();
inst = instringy.c_str();
amount = strlen(instringy.c_str());
// add this on.
for (z=0;z<amount;z++) {
folks[folksidx].incomingmsg[
    folks[folksidx].incomingidx] = inst[z];
    folks[folksidx].incomingidx++;
    if (folks[folksidx].incomingidx == 10) {
        ParseCommand(folksidx);
        folks[folksidx].incomingidx = 0;
    }
}
    }

void __fastcall TForm1::Sock1AutoAccept(TObject *Sender, TSock *NewSock)
{
	// look for free socket handle
    AnsiString texto;
    int myID;
	calcmeter();
    for (myID = 1; myID <= MAXUSERS; myID++)
    	if (folks[myID].socket == 0) {
            folks[myID].socket = NewSock;
            return;
		}
    log(" -- out of sockets. (!!)");
	NewSock->Send("NL\x03xxxxxxx");
    NewSock->Close();
    return;

//    Application->MessageBox ( "FUCK, MAN!!!!!", "Connected!!!", MB_OK);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::disconnect(int idx) {
// leave games:
int u;
for (u=0;u<=MAXGAMES;u++) {
  if ((games[u].players[1] == idx) || (games[u].players[2] == idx))
    leavegame(u,idx);
}
folks[idx].socket->Close();
if (folks[idx].flags & FL_GAME0) {
 	folks[idx].flags = 0;
    broadcastZero("<" + binword(idx)
         + pad((char *)folks[idx].username,7));
}
//log(idx);
log(folks[idx].socket->HostName + " logoff.");
folks[idx].userid = 0;
folks[idx].ip[0] = 0;
folks[idx].username[0] = 0;
folks[idx].socket = 0;
folks[idx].incomingidx = 0;
folks[idx].chatcount = 0;
folks[idx].chatgame = 0;
folks[idx].incomingchat = "";
// It says I'm supposed to Free it. But
// when I do, it crashes. Not even on the
// Free statement; elsewhere!
// So screw that. Eat my memory up. I don't
// give a crap.

//Application->MessageBox("No poop for you", "Noee", MB_OK);
calcmeter();
}
//-------------------->
void __fastcall TForm1::log(AnsiString msg) {
LogBox->SelStart = LogBox->Text.Length();
LogBox->SelText = msg + "\r\n";
}
//-------------------->>
int __fastcall TForm1::indexfromsocket(TObject * Sender) {
int i;
TSock *aSockpoo = ((TSock*)Sender);

for (i=0; i <= MAXUSERS; i++) {
  if (aSockpoo == folks[i].socket)
   			return i;
  }
// Crap... Not found

log("indexfromsocket() failed!");
return 0;
}
void __fastcall TForm1::Sock1Disconnect(TObject *Sender)
{
//TSock * asockpoo = ((TSock*)Sender);
disconnect(indexfromsocket(Sender));
// Disconnect the one who dropped.
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ParseCommand(int idx) {
// The bulk of the program:
char * msg = (char *)folks[idx].incomingmsg;
folks[idx].incomingmsg[10] = 0; // zero-term jic.
int spo,x,y;
int v, remainder;
AnsiString poopab, fug;

if (! (folks[idx].flags & FL_CONNECTED)) {
	switch (msg[0]) {
    case 'L':
      // eventually check.
//      Application->MessageBox(msg,"Poop",MB_OK);
	log(msg);
      strncpy((char *)folks[idx].username, st((char *) (msg + 1)).c_str(), 9);
      folks[idx].username[8] = 0;
      for (v=0;v<MAXSTORED;v++) {
		if (!(strcmp(folks[idx].username, stored[v].name.c_str()))) {
			  folks[idx].userid = v;
		      sendcommand(idx, "OLxxxxxxxx");
              return;
        }
      }
      sendcommand(idx,"NL\x01xxxxxxx");
      disconnect(idx);
//	  log((char *) folks[idx].username);

    break;
    case 'P':
      if (folks[idx].userid != 0 && folks[idx].userid <= MAXSTORED) {
    if (!(strcmp(st((char *)msg +1).c_str(), stored[folks[idx].userid].password.c_str()))) {
	  sendcommand(idx, "OPxxxxxxxx");
      log ((AnsiString) ((char *) folks[idx].username) + " Connected from " + folks[idx].socket->HostName+".");
      folks[idx].flags |= FL_CONNECTED;
      // DELETE ME //
/*	  sendcommand (idx, "+\x01\x1DPoopyff"
      					"`*xxxxxxxx");
*/
      } else {
      log ("Bad Password from " + (AnsiString)((char *)folks[idx].username) +": "
            + (AnsiString)st((char *)(msg + 1)));
      sendcommand(idx, "NP\x01xackxxxx");
      disconnect(idx);
      }
       break;
      }
	/*FALLTHRU*/
    default:
      log("Login error...");
      sendcommand(idx, "Nxxxxxxxxxx\n\nWhat are you doing?\n");
	  disconnect(idx);
    }
} else {

//Application->MessageBox(msg,"Poop",MB_OK);
if (folks[idx].chatcount) {
  folks[idx].incomingchat += (AnsiString) (char *) folks[idx].incomingmsg;
  folks[idx].chatcount--;
  if (folks[idx].chatcount == 0) {
    // done.
  log("[" + (AnsiString) folks[idx].chatgame + "]" + folks[idx].incomingchat);
  v = folks[idx].incomingchat.Length();
  poopab = "h" + binword(folks[idx].chatgame) +
     byte(((v%10)?(v/10)+1:(v/10))) + "xxxxxx"
     + folks[idx].incomingchat;

  v = poopab.Length();
if (remainder = (v % 10)) { // Yes, I mean that.
fug = "           ";
poopab += fug.SubString(1,(10-remainder));
}
//  log(poopab);
  if(folks[idx].chatgame)
    broadcast(folks[idx].chatgame, poopab);
  else
    broadcastZero(poopab);
  folks[idx].incomingchat = ""; // save memory!
  }
} else {
  switch(msg[0]) {
    case '+':
    log(msg);
	v = intval(msg[1],msg[2]);
    if (v>MAXUSERS) { log ("Verybad from "+
    	(AnsiString)((char *)folks[idx].username) + " -- gamereq userid > MAXUSERS!");
        disconnect(idx); }
    for (spo=254;spo--;)
		if (requests[spo].from == 0) {
			requests[spo].from = idx;
			requests[spo].to = v;
			requests[spo].overwrite = intval(msg[3],msg[4]);
            requests[spo].flags = gbyte(msg[5]);
			// send it:
            log ("reqid: " + (AnsiString) spo) ;
			log ((AnsiString) requests[spo].to + " " + (AnsiString) requests[spo].from);
            sendcommand(v,"+" +
            byte(spo) +
            byte(requests[spo].flags)
            + pad((char *)folks[idx].username,7));
            sendcommand(v,"`" + byte(stored[folks[idx].userid].rank)
            + "xxxxxxxx");
            return;
        }
	log ("Out of GameRequests!");
    break;
	case '@':
	// a "no thank you".
    v = gbyte(msg[1]);
    if (v > 254) { disconnect(idx); return; }
    spo = gbyte(msg[2]);
    if (!spo) {
	sendcommand(requests[v].from, "@"
    	+ pad((AnsiString) ((char *)folks[idx].username),9));
        }
    break;
    case '!':
	  v = gbyte(msg[1]);
      if (v > 254) { disconnect(idx); return; }
// Create game
	  for(x=1;x<MAXGAMES;x++) {
		if (games[x].turn == 0) {
		switch (requests[v].flags) {
		case 0: games[x].turn =1; break;
        case 1: games[x].turn = 2; break;
        case 2:
        if (stored[requests[v].to].rank > stored[requests[v].from].rank)
		games[x].turn = 2;
        else if (stored[requests[v].to].rank < stored[requests[v].from].rank)
        games[x].turn = 1;
		/*FALLTHRU*/
        case 3:
		// #######FIXME######## Random always gives player1
		games[x].turn = 1;
        break;
        }
        games[x].players[1] = folks[requests[v].from].userid;
        games[x].players[2] = folks[requests[v].to].userid;
		for (y=0;y<10;y++) games[x].members[y]=0;
        games[x].score[1] = 0;
        games[x].score[2] = 0;
        // tell these fine folks that their game is All Set.
        log (
        "v: " + (AnsiString) v +
        "requests[v].from: " + (AnsiString) requests[v].from +
        "to: " + (AnsiString) requests[v].to);
        log (
        "x: " + (AnsiString) x
       );
        sendcommand(requests[v].from,"J" + binword(x) + byte(1) + "xxxxxx");
        sendcommand(requests[v].to  ,"J" + binword(x) + byte(2) + "xxxxxx");
        break;
        }
      }
// Send out gamestarts to players.

    break;
    case ']':
	broadcastjoin(idx);
	folks[idx].flags |= FL_GAME0;
    broadcastZero("z" + binword(idx)
         + pad((char *)folks[idx].username,7));
//log ((AnsiString) idx);
    broadcastZero("Z" + binword(stored[folks[idx].userid].rank)
         + "xxxxxxx");
    break;
    case '[':
	folks[idx].flags &= ~FL_GAME0;
    broadcastZero("<" + binword(idx)
         + pad((char *)folks[idx].username,7));
    break;
    case 'h':
    // incoming Chat!
      folks[idx].incomingchat = (AnsiString)
      		(char *)folks[idx].username + ": ";
      folks[idx].chatgame = intval(msg[1], msg[2]);
      folks[idx].chatcount = gbyte(msg[3]);
    break;
    case 'l': // leavegame
    spo = intval(folks[idx].incomingmsg[1],folks[idx].incomingmsg[2]);
    leavegame (spo,idx);
    break;
    case 'M':
      makemove(idx, msg);
    break;
    case 'G':
      // Listing games.
      listgames(idx);
    break;
    case 'J':
      // Join a game:
      spo = intval(msg[1],msg[2]);
      // attempt to join.
      if (!games[spo].players[2]) {
  		// looks empty, let's join it!
/* #################### FIXME ######################### */
        if (games[spo].players[1] != 0 && games[spo].players[1] != idx) { // creator.
        log(((AnsiString) (char *)folks[idx].username) +
        " joined game #" + (AnsiString) spo +".");
        broadcast(spo, ":" +
        binword(spo) + pad((AnsiString) ((char *)folks[idx].username),7));
		games[spo].players[2]=idx;
        sendcommand(idx,"J" + binword(spo) + byte(2) + "xxxxxx");
      }

//      }
        // send Gamestart Info.
//        log ("Starting Gamestart...");
sendcommand(idx, "IT" + binword(spo) + byte(games[spo].turn)+"xxxxxxx");
		for(x=0;x<8;x++)
          for(y=0;y<8;y++)
            if (games[spo].board[x][y])
//              log ("IC" + (AnsiString) x + (AnsiString) y);
              sendcommand(idx, "IC" + byte((x*8) + y)
              			+ byte(games[spo].board[x][y]) + "zzzzzz");
        sendcommand(idx, "IS" + binword(games[spo].score[1]) +
        						binword(games[spo].score[2]) +
                                "xxxx");

if (games[spo].players[1]){
 sendcommand(idx, "IP" + pad((char *)folks[games[spo].players[1]].username,8));
// log(pad((char *)folks[games[spo].players[1]].username,8));
 }
if (games[spo].players[2]) sendcommand(idx, "Ip" + pad((char *)folks[games[spo].players[2]].username,8));
        sendcommand(idx, "IDxxxxpoop");
		} else {
			sendcommand(idx,"NJxxxxxxxx");
        }

      break;
    case 'C':
      // Create a game:
      for (spo=1;spo<=MAXGAMES;spo++) {
        if (!games[spo].turn) {
           // free game...
           goto foundgame;
        }
      }
    log ("Not enough free games!");
    sendcommand(idx,"NC\x01xxxxxxx");
    break;
    foundgame:
    games[spo].turn = 1;
	for (x=0;x<8;x++)
      for (y=0;y<8;y++)
        games[spo].board[x][y] = 0;
    games[spo].players[1] = idx;
	games[spo].players[2] = 0;
    games[spo].score[1] = 0;
	games[spo].score[2] = 0;
    for (x=0;x<10;x++) games[spo].members[x] = 0;
    poopab = "C" + binword((short) spo) + "xxxxxxx"   ;
//    log (poopab);
    sendcommand (idx,poopab);
    log ("Game "+ (AnsiString) spo +" created by "+ ((AnsiString) (char *)folks[idx].username)+".");
    break;
	default:
	break;
  }
 } // chatcount
 } // connected
}
//-------------------------->
void __fastcall TForm1::sendcommand(int idx, AnsiString msg) {
  if (! folks[idx].socket->Send(msg)) disconnect(idx);
}
AnsiString __fastcall TForm1::st(AnsiString msg) {
unsigned char * i = (unsigned char *) msg.c_str();
int m;
for (m=0; m< (int)strlen((char *)i); m++) {
  if (i[m] == 255) { i[m]=0; break;}
 }
return (char *)i;
}
void __fastcall TForm1::listgames(int idx) {
// sends every game to user[idx].
// this looks like:
// GwwPoopy..
// ww = game number (word), .. == 255
//log("Got gamelist command.");
int u;
 for (u=0;u<MAXGAMES;u++){
  if (games[u].turn) {
    // Game exists, list it.
    sendcommand(idx, "G" + binword((short) u) +
       pad(
         ((folks[games[u].players[1]].socket) ?
         (char *) folks[games[u].players[1]].username
         : "[Empty]")
         ,7));
    sendcommand(idx, "g" + binword((short) u) +
       pad(
       ((folks[games[u].players[2]].socket) ?
         (char *) folks[games[u].players[2]].username
         : "[Empty]")
         ,7));
 }
 }
 sendcommand(idx,"Exxxxpoopx");
}

AnsiString __fastcall TForm1::binword(short number) {
// Stupid-ass socket->Send won't let me send nulls!
// Mother fuckers!!! So we use base 255.
AnsiString poo = "xx";
char * roo = poo.c_str();
roo[0] = (char) (number / 255);
roo[1] = (char) (number % 255);
if (roo[0]==0) roo[0]=255;
if (roo[1]==0) roo[1]=255;
return poo;
}
AnsiString __fastcall TForm1::pad(AnsiString st, int lent) {
char * dvop;
char poopz[128];
int u;
for (u=0;u<lent;u++)
poopz[u] = 255;
poopz[lent] = 0;
dvop = st.c_str();
for (u=0;MIN(u<st.Length(),lent);u++)
  poopz[u] = dvop[u];

return (AnsiString) poopz;
}
short __fastcall TForm1::intval(unsigned char high,unsigned char low) {
   return (short) (((high==255)?0:high)*255) + ((low==255)?0:low);
}
void __fastcall TForm1::leavegame(int gamenum,int idx) {
// remove player idx from gamenum.
log ((AnsiString) ((char *) folks[idx].username) + " left game #" +(AnsiString) gamenum+".");
int send1=0;
int send2=0; // to avoid broadcast loop mishaps... ugly.
if (games[gamenum].players[1] == idx) {
    games[gamenum].players[1] = 0;
	send1=1;
    }
if (games[gamenum].players[2] == idx) {
	games[gamenum].players[2] = 0;
    send2=1;
    }
if (!(games[gamenum].players[1] || games[gamenum].players[2])) {
	closegame(gamenum);
 	log ("Game "+ (AnsiString) gamenum + " closed.");
  } else {
  broadcast(gamenum, "/"
      + binword(gamenum) + pad((AnsiString)(char *)folks[idx].username,7));
  }
}
void __fastcall TForm1::closegame(int i) {
 int x,y;
 for (x=0;x<8;x++)
   for (y=0;y<8;y++)
     games[i].board[x][y] = 0;
 games[i].turn = 0;
 games[i].players[1] = 0;
 games[i].players[2] = 0;
 games[i].score[1] = 0;
 games[i].score[2] = 0;
}
AnsiString __fastcall TForm1::byte(char xyz) {
AnsiString poo = "x";
char * roo = poo.c_str();
roo[0] = (char) (xyz % 255);
if (roo[0]==0) roo[0]=255;
return poo;
}

void __fastcall TForm1::makemove(int idx, char * move) {
int gameno, x, y, turn;
gameno = intval(move[1],move[2]);
x = gbyte(move[3]);
y = gbyte(move[4]);
turn = gbyte(move[5]);
// validate:
if (gameno > MAXGAMES) {
	log ("Verybad from " + (AnsiString) (char *)folks[idx].username+
     " game was greater than MAXGAMES");
    disconnect(idx);
 	return;
}

if (games[gameno].turn == 0 || games[gameno].turn == 3) {
   // This game does not exist or is finished!
//   log("Game over, or nonexistant game!");
   return;
   }
if (turn != games[gameno].turn) {
   // Not your turn.
//	log("Not turn!");
   return;
   }
if (games[gameno].players[turn] != idx) {
   // falsifying their move??
//   log("Not in game, wtf?");
   return;
   }
if (x > 7 || x < 0 || y > 7 || y < 0) {
   // out of bounds.
//   log("Out of Bounds");
   return;
   }
if (games[gameno].board[x][y] != 0) {
   // occupied
//   log("Occupied!");
   return;
   }
// looks good to me, let's broadcast.
games[gameno].board[x][y] = turn; // (originally forgot this, duh...)
// Score it...
Checksquares(x,y,gameno);
broadcast(gameno,
  "V" + binword(gameno) +
     byte(turn) + byte(x) + byte(y) + binword(games[gameno].score[1])
     + binword(games[gameno].score[2]));
games[gameno].turn = ((turn == 1) ? 2 : 1);
if (games[gameno].score[1] > 150 && ((games[gameno].score[1] -
 				games[gameno].score[2]) >= 15)) {
// player 1 won!
games[gameno].turn = 3;
broadcast(gameno, "W" + binword(gameno) + "\x01xxxxxx");
}
if (games[gameno].score[2] > 150 && ((games[gameno].score[2] -
 				games[gameno].score[1]) >= 15)) {
// player 1 won!
games[gameno].turn = 3;
broadcast(gameno, "W" + binword(gameno) + "\x02xxxxxx");
}

/*
log ("Move: <" + (AnsiString) gameno + "> (" +
    (AnsiString) x + "," + (AnsiString) y + ") - " +
    (AnsiString) turn + "-> " + (AnsiString) games[gameno].score[1]
    +":"+ (AnsiString) games[gameno].score[2]);
*/
}
int __fastcall TForm1::gbyte(unsigned char x) {
return (x == 255) ? 0 : (int) x;
}
void __fastcall TForm1::broadcast(int gameno, AnsiString message) {
if (games[gameno].players[1])
  sendcommand(games[gameno].players[1],message);
if (games[gameno].players[2])
  sendcommand(games[gameno].players[2],message);

}

void __fastcall TForm1::broadcastjoin(int idx) {
// tells someone all the people in game0
int x;
for (x=0;x<MAXUSERS;x++) {
  if ((folks[x].flags & FL_GAME0) && folks[x].socket) {
    sendcommand(idx,"z" + binword(x)
         + pad((char *)folks[x].username,7));
//log ((AnsiString) idx);
    sendcommand(idx,"Z" + binword(stored[folks[x].userid].rank)
         + "xxxxxxx");
  }
}}

void __fastcall TForm1::broadcastZero(AnsiString message) {
int i;
for (i=0;i<MAXUSERS;i++) {
  if (folks[i].socket && (folks[i].flags & FL_GAME0)) {
//	They want to hear it.
  sendcommand(i,message);
  }
}
}

void __fastcall TForm1::calcmeter() {
int i,c=0;
for (i=0;i<MAXUSERS;i++) {
    if (folks[i].socket) c++;
}
if (c > (MAXUSERS * 0.6))
    Meter->Color = clDanger;
else if (c > (MAXUSERS * 0.2))
    Meter->Color = clMild;
else
	Meter->Color = clOkay;
}
void __fastcall TForm1::readuserstruct() {
FILE * poop;
int g, idx;
int k,a;
char dats[100];
char name[14];
char pass[14];
if (!(poop = fopen(USERFILE,"r"))) {
  log("Unable to open User file for Reading!");
return;
}
// char outtemp[256];
while (1) {
   a = fscanf (poop, "%d %s %s %d %d %s", &idx, name, pass, &k, &g, dats);
   if ((a == EOF || a == 0)) break;
   if (idx>MAXSTORED) {
     log ("Verybad: users.txt has more users than MAXSTORED.");
     break;
   }
   stored[idx].rank = k;
   stored[idx].name = name;
   stored[idx].password = pass;
   stored[idx].flags = g;
}
fclose(poop);
return;
}
void __fastcall TForm1::writeuserstruct() {
	;
}

void __fastcall TForm1::Button3Click(TObject *Sender) {
	LogBox->Text = "";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action) {
int x;
  for (x=0;x<MAXUSERS;x++) {
      if (folks[x].socket) sendcommand(x,"Xxxxxxxxxx");
  }
}
//---------------------------------------------------------------------------
