//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "gamerq.h"
#include "PClient.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TGameReq *GameReq;
//---------------------------------------------------------------------------
__fastcall TGameReq::TGameReq(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TGameReq::Button1Click(TObject *Sender)
{
if (initiate) {
Form1->Sendcommand("+" + Form1->binword(usernum)
//#########################//
   + Form1->binword(0)
//#########FIXME##########: Game-obliteration (always sends 00)//
   + Form1->byte(
     ranked       |
     (first << 1) |
     ((ScrollBar1->Position) << 3))
   + "xxxx");
} else {
	if (ichangedit) {
    Form1->Sendcommand("@" + Form1->byte(reqid)
    			+ "\x01xxxxxxx");
    Form1->Sendcommand("+" + Form1->binword(usernum)
//#########################//
   + Form1->binword(0)
//#########FIXME##########: Game-obliteration (always sends 00)//
   + Form1->byte(
     ranked       |
     (first << 1) |
     ((ScrollBar1->Position) << 3)
     )
   + "xxxx");
	} else Form1->Sendcommand("!" + Form1->byte(reqid) + "xxxxxxxx");

}
Close();
}
//---------------------------------------------------------------------------
void __fastcall TGameReq::ScrollBar1Change(TObject *Sender)
{
switch(ScrollBar1->Position) {
case 0: timelim= 0; break;
case 1: timelim= 4; break;
case 2: timelim= 6; break;
case 3: timelim=10; break;
case 4: timelim=14; break;
case 5: timelim=18; break;
case 6: timelim=24; break;
case 7: timelim=30; break;
}
Limit->Caption = ((AnsiString)
  ((timelim==0)?(AnsiString)"No Time Limit":(AnsiString)timelim+" minutes"));
ScrollBar1->Hint = ((AnsiString)
  ((timelim==0)?(AnsiString)"No Time Limit":(AnsiString)timelim+" minutes"));
changedit();
}
//---------------------------------------------------------------------------
void __fastcall TGameReq::Button2Click(TObject *Sender)
{
if (initiate) Close(); // They don't need to tell the server that
					  // they've changed their minds, since the
				     // server hasn't any idea they had a mind in
 					// the first place.
Form1->Sendcommand("@" + Form1->byte(reqid)
			+ "\xFFxxxxxxx");
Close();
}
//---------------------------------------------------------------------------
void __fastcall TGameReq::changedit() {

if ( initiate || (
(origtime != ScrollBar1->Position) ||
(origranked != ranked) ||
(origfirst != first))) {
	Button1->Caption = "Request";
    GameReq->Caption = "Request to " + name;
	ichangedit = 1;
} else {
	Button1->Caption = "Yes";
    GameReq->Caption = "Request from " + name;
    ichangedit = 0;
}
}

void __fastcall TGameReq::radios() {
if (MeFirst->Checked == true) first = 0;
else if (YouFirst->Checked == true) first = 1;
else if (Random->Checked == true) first = 3;
else if (LowerFirst->Checked == true) first = 2;
changedit();
}
void __fastcall TGameReq::setup() {
; // set up the radio buttons, etc.
  ScrollBar1->Position = origtime;
  first = origfirst;
  ranked = origranked;
  ScrollBar1Change((TObject *) NULL);
  switch (origfirst) {
	case 0: MeFirst->Checked = true; break;
	case 1: YouFirst->Checked = true; break;
	case 2: LowerFirst->Checked = true; break;
	case 3: Random->Checked = true; break;
  }
  if (initiate) { Button1->Caption = "Request";
  	              Button2->Caption = "Cancel";
                  GameReq->Caption = "Request to " + name;}
  else          { Button1->Caption = "Yes";
  				  Button2->Caption = "No";
                  GameReq->Caption = "Request from " + name;}
  if (origranked) Ranked->Checked = true;
  		else      Ranked->Checked = false;
}
void __fastcall TGameReq::RankedClick(TObject *Sender)
{
if (Ranked->Checked == true) ranked = 1;
					   else  ranked = 0;
changedit();
}
//---------------------------------------------------------------------------

void __fastcall TGameReq::radios(TObject *Sender)
{
radios();	
}
//---------------------------------------------------------------------------
