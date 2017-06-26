//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "nul_out.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
   char in[40] = "\0\x12This is the End.\n255: \xFF 254: \xFE, okay.\0";
   send (in,40);
}
void TForm1::send(char * data, int length) {
   // transform
   char * temp, a;
   int newlen=0,x;

   if (!(temp = (char *)malloc(length*2))) fatalerror(1);

   for(x=0;x<length;x++) {
	  if (a=data[x]) {
        if ((a | 1) == '\xFF')   // either 255 or 254
        temp[newlen++] = '\xFE'; // escape it
		temp[newlen++] = a;      // add character
      }
      else
        temp[newlen++] = '\xFF'; // 0 -> 255 always.


   }
   socketsend(temp, newlen);
   free (temp);
}
void TForm1::socketsend(char * temp, int length) {
// Dummy function
temp[length+1]=0;
Application->MessageBox(temp,"Sending This.",MB_OK);
}

void TForm1::fatalerror(int value) {
static int herebefore;
if (herebefore) { printf ("Double fault!\n"); exit (-2); }
herebefore++;
// last minute cleanup. Be careful.

exit(-1);
}
AnsiString TForm1::read() {
	// however you actually get this
    AnsiString poopy = socketread();
    char * temp;
    char * orig = poopy.c_str();
    int newlength=0,x,y;

	if (!(temp = (char *)malloc(y=poopy.Length()))) fatalerror(-3);
	for (x=0;x<y;x++) {
		if (orig[x] == '\xFE'){
			if (x==(y-1)) fatalerror(-4);
            temp[newlength++] = orig[++x];
        } else
        	temp[newlength++] = (orig[x]=='\xFF')?'\0':orig[x];
    }
temp[newlength]=0;
poopy = (AnsiString) temp;
free(temp);
return poopy;
}

AnsiString TForm1::socketread() {
	char in[] = "\xFF\x12This is the End.\n255: \xFE\xFF 254: \xFE\xFE, okay.\xFF\0";
	return (AnsiString) in;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
AnsiString me=read();
for (int x=0;x<me.Length();x++)
     Output->Text += (AnsiString) (int) me[x] + (AnsiString) " ";

//Application->MessageBox(read().c_str(),"Hello",MB_OK);
}
//---------------------------------------------------------------------------