//---------------------------------------------------------------------------
#ifndef poopservH
#define poopservH
#define MAXUSERS 2048
#define MAXGAMES 1024
#define MAXSTORED 2048
#define DEBUG
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "Sock.hpp"
#include <vcl\ExtCtrls.hpp>
    typedef struct {
unsigned    char ip[16];
unsigned    char username[9];
            int  userid;
TSock		* 	 socket;
			int incomingidx;
unsigned    char incomingmsg[11];
			int chatcount;
            int chatgame;
AnsiString       incomingchat;
unsigned    char flags;
	} user;

typedef struct {
unsigned char board[8][8];
int           score[3]; // we use 1 and 2
int           players[3]; // ditto
short 		  members[10];
unsigned char turn;     // 1 or 2, 0 for Not Open.
} gamey;
typedef struct {
	AnsiString name;
    AnsiString password;
    int rank;
unsigned char flags;
} playerdef;
typedef struct {
 	int from;
    int to;
	int flags;
    int overwrite;
} reqdef;

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TSock *Sock1;
	TButton *Button1;
	TButton *Button2;
	TEdit *PortName;
	TMemo *LogBox;
	TListBox *Players;
	TEdit *Chat;
	TPanel *Meter;
	TButton *Button3;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
    void __fastcall initNewSock(TSock * foo);

	void __fastcall Sock1Read(TObject *Sender, int Count);

	void __fastcall Sock1AutoAccept(TObject *Sender, TSock *NewSock);
	void __fastcall Sock1Disconnect(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
public:		// User declarations
//      unsigned short int socks[MAXSOCKS];
short __fastcall intval(unsigned char high,unsigned char low);

void __fastcall disconnect(int idx);
void __fastcall log(AnsiString msg);
AnsiString __fastcall st(AnsiString msg);
AnsiString __fastcall binword(short number);
int __fastcall indexfromsocket(TObject * Sender);
	__fastcall TForm1(TComponent* Owner);
  void __fastcall DataIn(int folksidx);
//  void __fastcall debugy(AnsiString& texty);
void __fastcall  sendcommand(int idx, AnsiString msg);
int __fastcall gbyte(unsigned char x);
void __fastcall makemove(char * move);

int __fastcall hassquare(int x1, int y1, int x2, int y2, int idx, int game);
void __fastcall Checksquares(int x, int y, int game);
void __fastcall score(int player, int points, int game);
void __fastcall broadcast(int gameno, AnsiString message);
void __fastcall broadcastjoin(int idx);
void __fastcall ParseCommand(int);
void __fastcall broadcastZero(AnsiString message);
void __fastcall makemove(int, char *);
void __fastcall listgames(int idx);
void __fastcall readuserstruct();
void __fastcall writeuserstruct();
void __fastcall calcmeter();
void __fastcall leavegame(int, int);
void __fastcall closegame(int num);
AnsiString __fastcall byte(char);
AnsiString __fastcall pad(AnsiString st, int lent);
  user folks[MAXUSERS];                  // grand structures
  gamey games[MAXGAMES];
  playerdef stored[MAXSTORED];
  reqdef requests[254];
 };
   //---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
