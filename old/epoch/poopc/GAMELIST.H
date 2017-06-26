//---------------------------------------------------------------------------
#ifndef gamelistH
#define gamelistH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
//---------------------------------------------------------------------------
class TGlist : public TForm
{
__published:	// IDE-managed Components
	TListBox *LBox;
	void __fastcall FormHide(TObject *Sender);
	
	void __fastcall LBoxDblClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
void __fastcall Clearb(void);
	__fastcall TGlist(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TGlist *Glist;
//---------------------------------------------------------------------------
#endif
