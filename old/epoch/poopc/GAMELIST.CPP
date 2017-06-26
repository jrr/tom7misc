//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "gamelist.h"
#include "PClient.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TGlist *Glist;
//---------------------------------------------------------------------------
__fastcall TGlist::TGlist(TComponent* Owner)
	: TForm(Owner)
{
}
void __fastcall TGlist::Clearb(void) {
// clear the listbox.
LBox->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TGlist::FormHide(TObject *Sender)
{
Form1->gameboxopen = 0;
}
//---------------------------------------------------------------------------
void __fastcall TGlist::LBoxDblClick(TObject *Sender) {
int x;
short m;
char * poo;
AnsiString fred;
x = LBox->ItemIndex;
fred = LBox->Items->Strings[x];
//Application->MessageBox("Poop",fred.c_str(),MB_OK);
poo = fred.c_str();
for (m=0;m<=6;m++) {
  if (poo[m] == ' ') poo[m]=0;
}
m = (short) atoi(poo);
/* AnsiString temm = m;
Application->MessageBox(temm.c_str(), (char *)poo, MB_OK); */
/*Form1->Sendcommand("l" + Form1->binword(Form1->ourgame) +
				"xxxxxxx"); */
Form1->Sendcommand("[xxxxxxxx]");
//Form1->Cleargame();
Form1->Sendcommand("J"+ Form1->binword(m)+"xxxxxxx");
Close();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
