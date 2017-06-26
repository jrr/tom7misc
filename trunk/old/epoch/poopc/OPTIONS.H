//---------------------------------------------------------------------------
#ifndef optionsH
#define optionsH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TOptionsF : public TForm
{
__published:	// IDE-managed Components
	TEdit *Host;
	TLabel *Label1;
	TEdit *UserName;
	TLabel *Label2;
	TEdit *Password;
	TLabel *Label3;
	TCheckBox *CheckBox1;
	TButton *Button1;
	TImage *Pieces;
	TScrollBar *Red1;
	TScrollBar *Green1;
	TScrollBar *Blue1;
	TScrollBar *KRed1;
	TScrollBar *Red2;
	TScrollBar *Green2;
	TScrollBar *Blue2;
	TScrollBar *KRed2;
	TPanel *Panel1;
	TPanel *Panel2;
	TPanel *Panel3;
	TPanel *Panel4;
	TPanel *Panel5;
	TPanel *Panel6;
	TScrollBar *KGreen2;
	TScrollBar *KBlue2;
	TScrollBar *KGreen1;
	TScrollBar *KBlue1;
	TPanel *Panel8;
	TPanel *Panel9;
	TPanel *Panel10;
	TPanel *Panel7;
	TPanel *Panel11;
	TPanel *Panel12;
	TLabel *Label4;
	void __fastcall Button1Click(TObject *Sender);
	
	void __fastcall Update(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall PasswordKeyPress(TObject *Sender, char &Key);
	void __fastcall PasswordKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall FormKeyPress(TObject *Sender, char &Key);
private:	// User declarations
public:		// User declarations
	__fastcall TOptionsF(TComponent* Owner);
    void __fastcall Update();
int dim1,
    hi1,
    dim2,
    hi2;
void __fastcall Calculate();
};
//---------------------------------------------------------------------------
extern TOptionsF *OptionsF;
//---------------------------------------------------------------------------
#endif
