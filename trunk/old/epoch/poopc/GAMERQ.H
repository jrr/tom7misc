//---------------------------------------------------------------------------
#ifndef gamerqH
#define gamerqH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
/*#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>*/
//---------------------------------------------------------------------------
class TGameReq : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TButton *Button1;
	TButton *Button2;
	TScrollBar *ScrollBar1;
	TLabel *Limit;
	TRadioButton *MeFirst;
	TRadioButton *YouFirst;
	TRadioButton *Random;
	TRadioButton *LowerFirst;
	TCheckBox *Ranked;
	TLabel *Label1;
	TPanel *Panel1;
	TPanel *Panel2;
	TPanel *Panel3;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall ScrollBar1Change(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	
	void __fastcall RankedClick(TObject *Sender);

	void __fastcall radios(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TGameReq(TComponent* Owner);
void __fastcall changedit();
void __fastcall radios();
void __fastcall setup();
    int timelim;
    int first;
    int ichangedit;
    int reqid;
    int initiate;
    int origtime;
    int origfirst;
    int origranked;
    int ranked;
    int rank;
  short usernum;
    int obliterate;
AnsiString name;
};
//---------------------------------------------------------------------------
extern TGameReq *GameReq;
//---------------------------------------------------------------------------
#endif
