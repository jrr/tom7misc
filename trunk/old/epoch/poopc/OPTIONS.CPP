//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
#include "PClient.h"
#include "options.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TOptionsF *OptionsF;
//---------------------------------------------------------------------------
__fastcall TOptionsF::TOptionsF(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::Button1Click(TObject *Sender)
{
Close();
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::Calculate() {
hi1 = Red1->Position + (Green1->Position * 256) + (Blue1->Position * 65536);
hi2 = Red2->Position + (Green2->Position * 256) + (Blue2->Position * 65536);
dim1 = KRed1->Position + (KGreen1->Position * 256) + (KBlue1->Position * 65536);
dim2 = KRed2->Position + (KGreen2->Position * 256) + (KBlue2->Position * 65536);
/*
temp = K1->Position/100.0;
dim1 = ((Red1->Position + (temp*Red1->Position))
     + (256 * (Green1->Position + (temp*Green1->Position)))
     + (65536 * (Blue1->Position + (temp*Blue1->Position))));
temp = K2->Position/100.0;
dim2 = ((Red2->Position + (temp*Red2->Position))
     + (256 * (Green2->Position + (temp*Green2->Position)))
     + (65536 * (Blue2->Position + (temp*Blue2->Position))));
     */
//Application->MessageBox(dim1,hi1,MB_OK);
}
void __fastcall TOptionsF::Update() {
Calculate();
Form1->Drawpiece(Pieces->Canvas,0,0,1);
Form1->Drawpiece(Pieces->Canvas,1,1,2);
}
void __fastcall TOptionsF::Update(TObject *Sender)
{
  Update();
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::FormCreate(TObject *Sender)
{
Update();
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::PasswordKeyPress(TObject *Sender, char &Key)
{
// if (Key == 13) { Close(); }
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::PasswordKeyDown(TObject *Sender, WORD &Key,
	TShiftState Shift)
{
if (Key == 13) Close();
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::FormKeyDown(TObject *Sender, WORD &Key,
	TShiftState Shift)
{
; // null	
}
//---------------------------------------------------------------------------
void __fastcall TOptionsF::FormKeyPress(TObject *Sender, char &Key)
{
; // null	
}
//---------------------------------------------------------------------------
