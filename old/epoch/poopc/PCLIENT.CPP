//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#define BRIGHT1 ((TColor)(OptionsF->hi1)) //clLime
#define DIM1    ((TColor)(OptionsF->dim1)) // clGreen
#define BRIGHT2 ((TColor)(OptionsF->hi2)) //clRed
#define DIM2    ((TColor)(OptionsF->dim2)) //clMaroon

#define DEBUGIO
/*
  3 = One Wins
  4 = Two Wins
  5 = Tie
*/
#include "PClient.h"
#include "gamelist.h"
#include "options.h"
#include "aboutbox.h"
#include "gamerq.h"
//---------------------------------------------------------------------------
#pragma link "Sock"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PaintLine(int x1,
int y1, int x2, int y2, int color) {
  TCanvas *canvas = GameBox->Canvas;
  TPoint points[2];

  points[0] = Point(x1*42 + 21, y1*42 + 21);
  points[1] = Point(x2*42 + 21, y2*42 + 21);
  canvas->Pen->Mode = pmCopy;
  canvas->Pen->Width = 3;

  switch(color) {
  case 1: canvas->Pen->Color = DIM1; break;
  case 2: canvas->Pen->Color = DIM2; break;
  }
  canvas->Polyline(EXISTINGARRAY(points));
  canvas->Pen->Width = 2;
/*
  switch(color) {
  case 1: canvas->Pen->Color = BRIGHT1; break;
  case 2: canvas->Pen->Color = BRIGHT2; break;
  }
  canvas->Polyline(EXISTINGARRAY(points));
*/
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{

BGraphicBMP = new Graphics::TBitmap;
 BGraphicBMP->LoadFromFile("grid.bmp");
 // kill the socket.
 incomingidx = 0;
 waiting = 0;
 gameboxopen=0;
 StatusBox->Caption = "Not Connected";
 lastx = -1;
 cleargames();
 //Glist->LBox->Clear();
//Players->Clear();
 }
//---------------------------------------------------------------------------
void __fastcall TForm1::GameBoxPaint(TObject *Sender)
{
  TCanvas *canvas = GameBox->Canvas;
/* Players->Enabled = false;
Players->Visible = false; */     // But why??
canvas->Draw (0,0,BGraphicBMP);
int x,y;
ReChecksquares(currentgame);
for (x=0;x<8;x++) for (y=0;y<8;y++) if (games[currentgame].board[x][y])
	Putpiece(x,y,games[currentgame].board[x][y]);
puttoken(games[currentgame].lastx, games[currentgame].lasty);
//canvas-Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Putpiece(int x, int y, int color)
{
Drawpiece(GameBox->Canvas, x,y,color);
}
void __fastcall TForm1::Drawpiece(TCanvas *canvas,
									int x, int y, int color) {
  TPoint points[5];
  int xc, yc;
  xc = (42 * x) + 5;
  yc = (42 * y) + 5;

  points[0] = Point(xc, yc + 16);
  points[1] = Point(xc + 16, yc);
  points[2] = Point(xc + 32, yc + 16);
  points[3] = Point(xc + 16, yc + 32);
  points[4] = points[0];

  canvas->Pen->Mode = pmCopy;
  canvas->Pen->Width = 1.8;
switch (color) {
case 1:
  canvas->Brush->Color = BRIGHT1;
  canvas->Pen->Color = DIM1;
break;
case 2:
  canvas->Brush->Color = BRIGHT2;
  canvas->Pen->Color = DIM2;
break;
default:
break;
}
  canvas->Polygon(EXISTINGARRAY(points));
  canvas->Polyline(EXISTINGARRAY(points));


}

//-------------------------------------->
void __fastcall TForm1::GameBoxMouseDown(TObject *Sender, TMouseButton Button,
	TShiftState Shift, int X, int Y)
{
// Deduct absolute coordinates:

int movex, movey;
movex = (int) ((X) / 42);
movey = (int) ((Y) / 42);
//drawsquare(1,1,6,6,1);
if (! games[currentgame].board[movex][movey])
   Sendcommand("M" + binword(games[currentgame].gamenumber) + byte(movex) + byte(movey) +
      byte((short) games[currentgame].me) + "xxxx");

}
//---------------------------------------------------------------------------
void __fastcall TForm1::SockeyRead(TObject *Sender, int Count)
{
// Read data from the socket. If we've completed a message,
// parse it with ParseCommand(). Most of these will come
// through as blocks, but we want to be safe.
char * inst;
int amount, z;
AnsiString instringy;
//while (getbusy); // wait until we are Clear.

// Problem is that two are conflicting... hmm.

//getbusy = 1;
instringy = Sockey->Receive();
inst = instringy.c_str();
amount = strlen(inst);
// add this on.
for (z=0;z<amount;z++) {
    incomingmsg[incomingidx] = inst[z];
    incomingidx++;

    if (incomingidx == 10) {
        ParseCommand();
        incomingidx = 0;
    }
}
//getbusy = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ParseCommand(void) {
// take incomingmsg[] and parse it.
int xyz, gamenum,x,y;
AnsiString tempo;
incomingmsg[10] = 0;
#ifdef DEBUGIO
printthis("<< " + (AnsiString)(char *)incomingmsg);
#endif

if (chatcount) {
//printthis("Chat message" + (AnsiString) chatcount);
incomingchat += (AnsiString) (char *) incomingmsg;
chatcount--;
if (chatcount==0) {
   printthis(incomingchat);
   incomingchat="";
}
} else {
switch (incomingmsg[0]) {
 case 'V':
// Textdisp->Text = Textdisp->Text + "Poop";
gamenum = findgame(intval(incomingmsg[1],incomingmsg[2]));
// discard
xyz = gbyte(incomingmsg[3]);
x = gbyte(incomingmsg[4]);
y = gbyte(incomingmsg[5]);

Play1score->Caption = (AnsiString) intval(incomingmsg[6], incomingmsg[7]);
Play2score->Caption = (AnsiString) intval(incomingmsg[8], incomingmsg[9]);

if (xyz == 1) {
Turn1->Checked = false;
Turn2->Checked = true;
} else {
Turn1->Checked = true;
Turn2->Checked = false;
}

games[gamenum].board[x][y] = xyz;
games[gamenum].lastx = x;
games[gamenum].lasty = y;
if (gamenum == currentgame) { Checksquares( x, y,gamenum);
for (x=0;x<8;x++) for (y=0;y<8;y++) if (games[gamenum].board[x][y])
						Putpiece(x,y,games[gamenum].board[x][y]);
puttoken(games[gamenum].lastx,games[gamenum].lasty);
}
break;
case 'z':
// someone joined the players room.
// printthis((AnsiString) (char *)incomingmsg);
Players->Items->Add(
(AnsiString) intval(incomingmsg[1], incomingmsg[2]) + " "
+ (AnsiString) st((AnsiString)(char *)(incomingmsg + 3)));
break;
case '<':
xyz = intval(incomingmsg[1],incomingmsg[2]);
// printthis((AnsiString) xyz);
for (x=0;x<Players->Items->Count;x++) {
if (xyz == bytem(Players->Items->Strings[x])) {
	Players->Items->Delete(x);
    x--; // rewind
   }
}
break;
case '+': // Game Request (part 1)
printthis((char*)incomingmsg);
xyz = gbyte(incomingmsg[2]);
GameReq->reqid = gbyte(incomingmsg[1]);
GameReq->name = st((AnsiString)(char *)(incomingmsg + 3));
GameReq->origranked = (xyz & 1);
GameReq->origfirst = (xyz >> 1) & 3;
GameReq->origtime = (xyz >> 3) & 7;
GameReq->ichangedit = 0;
break;
case '`':
GameReq->rank = intval(incomingmsg[1], incomingmsg[2]);
GameReq->initiate = 0;
GameReq->setup();
GameReq->ShowModal();
break;
 case 'O':
 // an OK message; very important!
   switch (incomingmsg[1]) {
	 case 'L':
       if (waiting == okforlogin) {
	   // we got an OK for login; move on to the next step.
       waiting = okforpass;
       Sendcommand("P" + pad(OptionsF->Password->Text,9));
       StatusBox->Caption = "Sending Password...";
       } // otherwise just ignore it, I guess.
     break;
     case 'P':
       if (waiting == okforpass) {
       StatusBox->Caption = "Logged in.";
//       GetGameList();
	   // ########### FIXME ### - only if they want to:
       Players->Enabled = true;
       Players->Visible = true;
       Players->Clear();
       Sendcommand("]xxxxxxxxx");
       }
     default: break;
	}
break;
case 'X':
// server going down
Disconnected(dsNomsg);
Application->MessageBox("The server is going down immediately. Sorry, "
"I run this off my own machine for the time being and sometimes "
"the computer needs to go down.", "Server going down.", MB_OK);
break;
case 'W':
gamenum = findgame(intval(incomingmsg[1],incomingmsg[2]));

  x = gbyte(incomingmsg[3]);
  switch (x) {
  case 1: games[gamenum].turn = 3; break;
  case 2: games[gamenum].turn = 4; break;
  case 0: games[gamenum].turn = 5; break;
  }
break;
case 'I':
 switch(incomingmsg[1]) {
  case 'T':
    x = (intval(incomingmsg[2],incomingmsg[3]));
    cinfstream = findgame(x);
    games[cinfstream].turn = gbyte(incomingmsg[4]);
  break;
  case 'C':
    x = ((gbyte(incomingmsg[2])) / 8);
    y = ((gbyte(incomingmsg[2])) % 8);
    games[cinfstream].board[x][y] = gbyte(incomingmsg[3]);
//  StatusBox->Caption = "(" + (AnsiString) x + "," + (AnsiString) y+ ")";
    break;
  case 'P':
    printthis(st((char *) incomingmsg + 2) + " is here.");
  Play1name->Caption = st((char *) incomingmsg + 2);
  break;
  case 'p':
    printthis(st((char *) incomingmsg + 2) + " is here.");
  Play2name->Caption = st((char *) incomingmsg + 2);
  break;
  case 'D':
// Display the New Game?
//  GameBoxPaint((TObject *) NULL);
  StatusBox->Caption = "Joined game " +
  		(AnsiString) games[cinfstream].gamenumber;
  cinfstream = 0;
  break;
 }
break;
case ':':
  xyz = gbyte(incomingmsg[1]);
  tempo = (AnsiString) xyz;
//  Application->MessageBox(tempo.c_str(), incomingmsg, MB_OK);
    printthis(st((char *) incomingmsg + 2) + " has entered.");
/*  if (xyz == 1)
  Play1name->Caption = st((char *) incomingmsg + 2);
  if (xyz == 2)
  Play2name->Caption = st((char *) incomingmsg + 2);
  */
break;
case '/':
//************FIXME******************//

  xyz = gbyte(incomingmsg[1]);
  printthis(st((char *) incomingmsg + 2) + " has left.");
/*
  if (xyz == 1)
  Play1name->Caption = "[Empty]";
  if (xyz == 2)
  Play2name->Caption = "[Empty]";
*/
break;
case 'C':
// A game was created for us, so join it.
//Application->MessageBox((char *) incomingmsg, "poo", MB_OK);
xyz = (short) intval(incomingmsg[1],incomingmsg[2]);
Sendcommand("J"+binword((short) xyz)+"xxxxxxx");
//tempo = "Joined game #" + (AnsiString) xyz;
//printthis(tempo);
//joingame(xyz);  // ?
//games[findgame(xyz)].me = 1;
break;
case 'J':
xyz = intval(incomingmsg[1], incomingmsg[2]);
joingame(xyz);
games[findgame(xyz)].me = gbyte(incomingmsg[3]);
// usplayer = gbyte(incomingmsg[3]);
break;
case 'G':
// game list
  if (! gameboxopen) {
    Glist->Show();
	glistfirst = "";
    Glist->Clearb();
    gameboxopen = 1;
  }
glistfirst = st((char *)(incomingmsg+3));
break;
case 'g':
gamenum = intval(incomingmsg[1],incomingmsg[2]);
Glist->LBox->Items->Add((AnsiString) gamenum + " " +glistfirst+"  |  "+st((char *)(incomingmsg+3)));
break;
case '.': /// DEBUGGING ##FIXME##
Application->MessageBox("Message", "Message", MB_OK);
break;
case 'h':
// Incoming Chat.
chatcount = gbyte(incomingmsg[3]); // num of packets
incomingchat = "";
break;
case 'E':
gameboxopen = 0;
break;
case 'N':
// also very important, a NOT OK message.
   switch (incomingmsg[1]) {
      case 'P':
		switch (incomingmsg[2]) {
        case 1:
        Application->MessageBox("Incorrect Password!", "Password Failed!", MB_OK);
        break;
        default:
		Application->MessageBox("Unknown Error code", "Password Failed!", MB_OK);
        break;
        }
      break;
      case 'L':
        switch (incomingmsg[2]) {
		case 1:
          Application->MessageBox("Username unknown!", "Login failed", MB_OK);
          break;
        case 2:
          Application->MessageBox("Logins not permitted now.", "Login failed", MB_OK);
          break;
		case 3:
          Application->MessageBox("Server is full. (!)", "Login Failed", MB_OK);
        break;
	    case 4:
        case 5:
          Application->MessageBox("You are banned or suspended.", "Login failed", MB_OK);
		break;
        case 6:
          Application->MessageBox("Your version is out of date and "
          "unsupported. You'll need a new version; try "
          "http://www.andrew.cmu.edu/~twm/squares/", "Login failed", MB_OK);
        break;
        default:
          Application->MessageBox("Unknown error code.", "Login failed!!", MB_OK);
          break;
        }
      Disconnected(dsNomsg);
      break;
      default: break;
	}

 break;
 default: break;
 }
} // if chatcount
}
//------------------------>

void __fastcall TForm1::SockeyConnect(TObject *Sender)
{
// Log in.
StatusBox->Caption = "Logging in...";
waiting = okforlogin;
Players->Clear();
Glist->Close();
Sendcommand("L" + pad(OptionsF->UserName->Text,9));

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

void __fastcall TForm1::Sendcommand (AnsiString sendo) {

//Application->MessageBox ( "Poop", sendo.c_str(), MB_OK);

// Might want to queue this at some point.
#ifdef DEBUGIO
 printthis(">> " + sendo);
#endif
if (! Sockey->Send(sendo)) Disconnected(dsMsg);

}
void __fastcall TForm1::Disconnected(int msgornot) {
// disconnected
if (msgornot == dsMsg) {
Application->MessageBox("Disconnected!", "Disconnected!!", MB_OK);
}
Sockey->Close();
//OptionsF->LoginButt->Enabled = true;
StatusBox->Caption = "Disconnected.";
GamePan->Caption = "No Game.";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GetGameList() {
	waiting = notwait;
}
//---------------------------------------------------------------------------
short __fastcall TForm1::intval(unsigned char high,unsigned char low) {
	return (short) (((high==255)?0:high)*255) + ((low==255)?0:low);
}
void __fastcall TForm1::joingame(short num) {
    int x;
	if ((x = addgame()) != -1) {
    	Cleargame(x);
	    games[x].gamenumber = num;
		GameTab->Tabs->Add((AnsiString)x);
    } else {
		// can't join that many games.
        Sendcommand("l" + binword(num) + "xxxxxxx");
		Application->MessageBox("This is too many games", "Error", MB_OK);
    }
	//ourgame=num;
//	GamePan->Caption = "Game " + (AnsiString) ourgame;

}

AnsiString __fastcall TForm1::binword(short number) {

AnsiString poo = "xx";
char * roo = poo.c_str();
roo[0] = (char) (number / 255);
roo[1] = (char) (number % 255);
if (roo[0]==0) roo[0]=255;
if (roo[1]==0) roo[1]=255;
return poo;
}
AnsiString __fastcall TForm1::st(AnsiString msg) {
unsigned char * i = (unsigned char *) msg.c_str();
int m;
for (m=0; m< (int)strlen((char *)i); m++) {
  if (i[m] == 255) { i[m]=0; break;}
 }
return (char *)i;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Quit1Click(TObject *Sender) {
	// Actually "login"...
	Sockey->HostName = OptionsF->Host->Text;
	Sockey->PortName = "4242";
	Sockey->Open();
	StatusBox->Caption = "Connecting...";
	//LoginButt->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::N1Click(TObject *Sender) {
	// List games
	Sendcommand("Gxxxxxxxxx");
	gameboxopen =0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateGame1Click(TObject *Sender) {
//	Sendcommand("l" + binword(ourgame) + "xxxxxxx");
    Sendcommand("[xxxxxxxx]");
//	Cleargame();
	Sendcommand("Cxxxxxxxxx");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Logout1Click(TObject *Sender) {
	Sendcommand("Q123456789");
	Sockey->Close();
	StatusBox->Caption = "Disconnected.";
	//LoginButt->Enabled = true;
	cleargames();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Cleargame(int num) {
int x,y;
for (y=0;y<8;y++) for (x=0;x<8;x++) games[num].board[x][y] = 0;
games[num].turn = 0;
games[num].me = 0;
games[num].chatbox = "";
/*	Play1score->Color = clBtnFace;
	Play1name->Color = clBtnFace;
	Play2name->Color = clBtnFace;
	Play2score->Color = clBtnFace;
	Play1score->Caption = "";
	Play2score->Caption = "";   // set playernames too?
*/
	games[num].lastx = -1;
//	GameBoxPaint((TObject *) NULL);
/*	Players->Enabled = false;
    Players->Visible = false;
    */
}
AnsiString __fastcall TForm1::byte(short xyz) {
	AnsiString poo = "x";
	char * roo = poo.c_str();
	roo[0] = (char) (xyz % 255);
	if (roo[0]==0) roo[0]=255;
	return poo;
}
int __fastcall TForm1::gbyte(unsigned char x) {
	return (x == 255) ? 0 : (int) x;
}
void __fastcall TForm1::Options1Click(TObject *Sender) {
	OptionsF->ShowModal();
   	OptionsF->Calculate(); // calc new colours.
    GameBoxPaint((TObject *) NULL);
}
void __fastcall TForm1::printthis(AnsiString msg) {
LogBox->SelStart = LogBox->Text.Length();
LogBox->SelText = msg + "\r\n";
}

//---------------------------------------------------------------------------
void __fastcall TForm1::About1Click(TObject *Sender)
{
Form2->ShowModal();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Exit1Click(TObject *Sender)
{
exit(0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::InputKeyPress(TObject *Sender, char &Key)
{
int v, remainder;
AnsiString fug;
if (Key == 13) {
// send Input->text as chat.
v = Input->Text.Length();
Sendcommand("h" + binword(games[currentgame].gamenumber) +
     byte(((v%10)?(v/10)+1:(v/10))) +
     "xxxxxx");
Sendcommand(Input->Text);
//printthis("Sentchars: " + (AnsiString) v);
//printthis("Sentmsg: " + (AnsiString) Input->Text);

if (remainder = (v % 10)) // Yes, I mean that.
fug = "           ";
fug = fug.SubString(1,(10-remainder));
	Sendcommand(fug);
//    printthis("Sentpad: " + fug);
Input->Text = "";
}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::puttoken(int x, int y) {
  TPoint points[5];
  int xc, yc;
  TCanvas *canvas = GameBox->Canvas;

  if (lastx == -1) return;
  xc = (42 * x) + 15;
  yc = (42 * y) + 15;

  points[0] = Point(xc, yc);
  points[1] = Point(xc+12, yc);
  points[2] = Point(xc+12, yc+12);
  points[3] = Point(xc, yc+12);
  points[4] = points[0];

  canvas->Pen->Mode = pmCopy;
  canvas->Pen->Width = 1;

  canvas->Brush->Color = clWhite;
  canvas->Pen->Color = clBlack;

  canvas->Polygon(EXISTINGARRAY(points));
  canvas->Polyline(EXISTINGARRAY(points));

}
int __fastcall TForm1::bytem(AnsiString ms) {
char * poo;
int m,x;
poo = ms.c_str();
for (m=0;m<=ms.Length();m++) {
  if (poo[m] == ' ') {
  poo[m] = 0;
  x = (short) atoi(poo);
  poo[m] = ' ';
  return x;
  }
}
return 0;
}

int __fastcall TForm1::addgame() {
// returns an index for a new game.
int i;
for (i=0;i<MAXCGAMES;i++) {
	if (!games[i].gamenumber)
    	return i;
}
return -1;
}
void __fastcall TForm1::GameTabClick(TObject *Sender)
{
    int poo;
    poo = atoi(
          GameTab->Tabs->Strings[GameTab->TabIndex].c_str()
          );
	currentgame = poo;
    GameBoxPaint((TObject *) NULL);
}
//---------------------------------------------------------------------------
int __fastcall TForm1::findgame(int gamenum) {
int i;
  for(i=0;i<MAXCGAMES;i++) {
	if (games[i].gamenumber == gamenum) return i;
  }
  return 0;
}

void __fastcall TForm1::drawgame(int gamenum) {
switch(games[gamenum].turn) {
  case 3:
    StatusBox->Caption = "Player 1 wins!";
    Play1score->Color = clTeal;
    Play1name->Color = clTeal;
    Play2name->Color = clGray;
    Play2score->Color = clGray;
  break;
  case 4:
    StatusBox->Caption = "Player 2 wins!";
    Play2score->Color = clTeal;
    Play2name->Color = clTeal;
    Play1name->Color = clGray;
    Play1score->Color = clGray;
  break;
  case 5:
    StatusBox->Caption = "It's a Tie!";
    Play1score->Color = clGray;
    Play1name->Color = clGray;
    Play2name->Color = clGray;
    Play2score->Color = clGray;
  break;
}

}
void __fastcall TForm1::cleargames() {
int poop;
 for (poop=0;poop<MAXCGAMES;poop++) {
  Cleargame(poop);
  }
}
void __fastcall TForm1::Poop1Click(TObject *Sender) {
//#####################FIXME##################//
 GameReq->ShowModal();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PlayersDblClick(TObject *Sender) {
int fred,x;
AnsiString poop;
   poop = Players->Items->Strings[Players->ItemIndex];
   fred = bytem(poop);
GameReq->usernum = fred;
// #########FIXME###### Change it so they can set the defaults.
GameReq->first = 2;
GameReq->ScrollBar1->Position = 0;
GameReq->ranked = 1;
for (x=0;x<poop.Length();x++) {
    if (poop.c_str()[x] == ' ') break;
}
GameReq->name = (AnsiString)((char *)(poop.c_str() + x + 1));
GameReq->initiate = 1;
GameReq->ichangedit = 0;
GameReq->setup();
GameReq->ShowModal();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
Sockey->Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
; // null	
}
//---------------------------------------------------------------------------
