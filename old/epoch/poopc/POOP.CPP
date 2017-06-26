//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("PClient.cpp", Form1);
USERES("poop.res");
USEUNIT("squares.cpp");
USEFORM("gamelist.cpp", Glist);
USEFORM("options.cpp", OptionsF);
USEFORM("aboutbox.cpp", Form2);
USEFORM("gamerq.cpp", GameReq);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	try
	{
		Application->Initialize();
		Application->Title = "PoopClient";
		Application->CreateForm(__classid(TForm1), &Form1);
		Application->CreateForm(__classid(TGlist), &Glist);
		Application->CreateForm(__classid(TOptionsF), &OptionsF);
		Application->CreateForm(__classid(TForm2), &Form2);
		Application->CreateForm(__classid(TGameReq), &GameReq);
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	return 0;
}
//---------------------------------------------------------------------------
