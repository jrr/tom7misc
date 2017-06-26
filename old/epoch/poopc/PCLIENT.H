//---------------------------------------------------------------------------
#ifndef PClientH
#define PClientH
#define STRN(x) (((AnsiString) x).c_str())
#define _WINSOCKAPI_ // ha ha!!
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include "Sock.hpp"
#include <vcl\Menus.hpp>
#include <vcl\ComCtrls.hpp>
#include <vcl\Tabs.hpp>
#define dsNomsg 0
#define dsMsg 1
#define MAX(c1,c2) ((c1 < c2) ? c2 : c1)
#define MIN(c1,c2) ((c1 < c2) ? c1 : c2)
// #include <vcl\ComCtrls.hpp> // <- incompatible with
                              /// Beachdog!
#define MAXCGAMES 16
typedef struct {
       int gamenumber;
       int board[8][8];
       int turn;
       int me;
       int lastx;
       int lasty;
       AnsiString chatbox;
} cgame;
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPaintBox *GameBox;
	TSock *Sockey;
	TMemo *LogBox;
	TPanel *StatusBox;
	TMainMenu *Menu;
	TMenuItem *Verbs1;
	TMenuItem *Quit1;
	TMenuItem *N1;
	TMenuItem *Logout1;
	TMenuItem *Exit1;
	TMenuItem *Nouns1;
	TMenuItem *Options1;
	TMenuItem *Poop1;
	TMenuItem *N2;
	TMenuItem *CreateGame1;
	TMenuItem *Utterances1;
	TMenuItem *Help1;
	TMenuItem *CrapIlostagain1;
	TPanel *GamePan;
	TPanel *Play1name;
	TPanel *Play2name;
	TPanel *Play1score;
	TPanel *Play2score;
	TEdit *Input;
	TMenuItem *N3;
	TMenuItem *About1;
	TRadioButton *Turn1;
	TRadioButton *Turn2;
	TListBox *Players;
	TTabSet *GameTab;
	TButton *Button1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GameBoxPaint(TObject *Sender);
	void __fastcall GameBoxMouseDown(TObject *Sender, TMouseButton Button,
	TShiftState Shift, int X, int Y);

	void __fastcall SockeyRead(TObject *Sender, int Count);

	void __fastcall SockeyConnect(TObject *Sender);

	void __fastcall Quit1Click(TObject *Sender);
	void __fastcall N1Click(TObject *Sender);
	void __fastcall CreateGame1Click(TObject *Sender);
	void __fastcall Logout1Click(TObject *Sender);
	void __fastcall Options1Click(TObject *Sender);

	void __fastcall About1Click(TObject *Sender);
	void __fastcall Exit1Click(TObject *Sender);
	void __fastcall InputKeyPress(TObject *Sender, char &Key);
	void __fastcall GameTabClick(TObject *Sender);
	void __fastcall Poop1Click(TObject *Sender);
	void __fastcall PlayersDblClick(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
     __fastcall TForm1(TComponent* Owner);
void __fastcall puttoken(int, int);
void __fastcall Putpiece(int x, int y, int color);
void __fastcall PaintLine(int x1, int y1, int x2, int y2, int color);
int __fastcall gbyte(unsigned char x);
void __fastcall Checksquares(int x, int y,int);
int __fastcall bytem(AnsiString ms);
void __fastcall score(int player, int points,int);
int __fastcall hassquare(int x1, int y1, int x2, int y2, int idx, int);
void __fastcall drawsquare(int x1, int y1, int x2, int y2, int colorb);
  Graphics::TBitmap * BGraphicBMP ;
void __fastcall Drawpiece(TCanvas *canvas,int,int,int);
  void __fastcall ParseCommand(void);
AnsiString __fastcall pad(AnsiString st, int lent);
void __fastcall Sendcommand (AnsiString sendo);
void __fastcall Disconnected(int messageornot);
int __fastcall findgame(int gamenum);
void __fastcall cleargames();
void __fastcall drawgame(int);
void __fastcall GetGameList();
AnsiString __fastcall st(AnsiString msg);
short __fastcall intval(unsigned char high,unsigned char low);
void __fastcall ReChecksquares(int);
AnsiString __fastcall byte(short xyz);
void __fastcall Cleargame(int gameno);
int __fastcall addgame();
void __fastcall joingame(short num);
void __fastcall printthis(AnsiString msg);
AnsiString __fastcall binword(short number);
//  unsigned char board[8][8];
  unsigned char incomingmsg[11];
  cgame games[MAXCGAMES];
  unsigned char incomingidx;
enum { notwait, okforlogin, okforpass, okforjoin, okformove };
int waiting;
int gameboxopen;
AnsiString glistfirst;
//int ourgame, usplayer;
int currentgame;
int getbusy;
int chatcount;
AnsiString incomingchat;
int lastx, lasty;
int cinfstream;
 };
//---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
