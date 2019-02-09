// LICENSE FOR THIS FILE ONLY:
// MIGOYA-CHESS.
// Developed by Alejandro Migoya   
// alejandro.migoya@gmail.com
// If you use this code, I큞l appreciate if you mention me in your work.

// MIGOYA-CHESS written in C++ is not really a strong chess engine, that큦 not the objective.
// The objective of this version is to teach how a chess engine algorithm works.
// I tried to keep the code as simple as possible.
// This version does not include castles and check큦.

// Enjoy!

//Dependencies
#include <iostream>
#include <time.h>
#include <math.h>
using namespace std;

//Constant Pieces
const int King = 10000;
const int Queen = 90;
const int Rook = 50;
const int Bishop = 32;
const int Knight = 30;
const int Pawn = 10;

//Matrixes
int Board[12][12];			
int AttackedH[12][12];		
int AttackedC[12][12];		
int Value[20];				

//Variables
int CurrentDepth;			
int Depth;					
int Abort;					
int State;					
int MoveNo;					
int BestMove[4];			
long int Nodes;				

//Functions
int Evaluation();
int Material();
int Development();
int PossibleCrown();
int GameState();
int HumanAproach(int X2, int Y2);
int ComputerAproach(int X2, int Y2);

//Subroutines
void PrintHelp();
void NewGame();				
void PrintBoard();			
void ClearMobility();		
void AllComputerMoves();
void AllHumanMoves();
void StartSearch();
void KingCoordinates(int &XCK, int &YCK, int &XHK, int &YHK);
void HumanMove(int X1, int Y1, int X2, int Y2);
void ComputerMove(int X1, int Y1, int X2, int Y2);
void MakeMovement(int X1, int Y1, int X2, int Y2, int &Moved, int &Taken);
void UnMakeMovement(int X1, int Y1, int X2, int Y2, int Moved, int Taken);
void HumanMoveGenerator(int X1, int Y1, int MoveType);
void HumanLongMoves(int X1, int Y1, int MX, int MY, int MoveType);
void HumanMoveType(int X1, int Y1, int X2, int Y2, int MoveType);
void ComputerMoveGenerator(int X1, int Y1, int MoveType);
void ComputerLongMoves(int X1, int Y1, int MX, int MY, int MoveType);
void ComputerMoveType(int X1, int Y1, int X2, int Y2, int MoveType);


//Main function
void main()
{
	clock_t Time;
	char Inst[4];
	int Moved, Taken, X1, Y1, X2, Y2;
	Depth=5;
	NewGame();
    A1:
	cout<<"Your move: ";
	cin>>Inst;
	if (Inst[0]==110 || Inst[0]==78) { NewGame(); goto A1; }
	if (Inst[0]==120 || Inst[0]==88) { return; }
	if (Inst[0]==63) { PrintHelp(); }
	X1=Inst[0]-48+1;	Y1=Inst[1]-48+1;	X2=Inst[2]-48+1;	Y2=Inst[3]-48+1;
	ClearMobility();
	HumanMoveGenerator(X1,Y1,3);
	if (AttackedH[X2][Y2]!=0)
	{
		MakeMovement(X1,Y1,X2,Y2,Moved,Taken);
		MoveNo++;
		PrintBoard();
		cout<<"\nThinking..."<<"\n";
		Time = clock() / (CLOCKS_PER_SEC / 1000);
		StartSearch();
		MakeMovement(BestMove[0],BestMove[1],BestMove[2],BestMove[3],Moved,Taken);
		MoveNo++;
		PrintBoard();
		Time = (clock() / (CLOCKS_PER_SEC / 1000))-Time;
		cout<<"Move:"<<BestMove[0]-1<<BestMove[1]-1<<BestMove[2]-1<<BestMove[3]-1;
		cout<<"  Time:"<<Time<<"  Depth:"<<Depth<<"  Nodes:"<<Nodes<<"  State:"<<State;
		cout<<"  Val:"<<Value[1]<<"\n";
	}
	goto A1;
	return;
}

//******************************************************************************************
//******************SUPPORT FUNCTIONS*******************************************************
//******************************************************************************************

//Sets the board to original positions
void NewGame()
{
	for (int X=0; X<=11; X++)  
	{
		for (int Y=0; Y<=11; Y++) 
		{	
			Board[X][Y]=2;	
			if ((X>1) && (X<10) && (Y>1) && (Y<10)) { Board[X][Y]=0; }	
		}
	}
	Board[2][2]=-Rook;		Board[3][2]=-Knight;		Board[2][9]=Rook;		Board[3][9]=Knight;
	Board[4][2]=-Bishop;	Board[5][2]=-Queen;			Board[4][9]=Bishop;		Board[5][9]=Queen;		
	Board[6][2]=-King;		Board[7][2]=-Bishop;		Board[6][9]=King;		Board[7][9]=Bishop;	
	Board[8][2]=-Knight;	Board[9][2]=-Rook;			Board[8][9]=Knight;		Board[9][9]=Rook;
	Board[2][3]=-Pawn;		Board[3][3]=-Pawn;			Board[2][8]=Pawn;		Board[3][8]=Pawn;	
	Board[4][3]=-Pawn;		Board[5][3]=-Pawn;			Board[4][8]=Pawn;		Board[5][8]=Pawn;		
	Board[6][3]=-Pawn;		Board[7][3]=-Pawn;			Board[6][8]=Pawn;		Board[7][8]=Pawn;		
	Board[8][3]=-Pawn;		Board[9][3]=-Pawn;			Board[8][8]=Pawn;		Board[9][8]=Pawn;
	MoveNo=0;
	PrintBoard();
}

//Prints the current board on screen
void PrintBoard()
{	int X=0;
	int Y=0;
	cout<<"\n\n\n**********************************************************"<<"\n\n\n";
	cout<<"   MigoyaChess C++ (c) 2010         Enter     ? = HELP    |   x = Exit.\n\n";
	cout<<"    1  2  3  4  5  6  7  8 \n";
	cout<<"   ________________________\n";
	cout<<"  |                        |\n";
	for (Y=9; Y>=2; Y--)
	{
		cout<<"  |";
		for (X=2; X<=9; X++)
		{
			switch (Board[X][Y])
			{
				case -Pawn:		cout<<" i ";		break;
				case -Rook:		cout<<" T ";		break;
				case -Bishop:	cout<<" B ";		break;
				case -Knight:	cout<<" K ";		break;
				case -Queen:	cout<<" W ";		break;
				case -King:		cout<<" X ";		break;
				case Pawn:		cout<<"-i ";		break;
				case Rook:		cout<<"-T ";		break;
				case Bishop:	cout<<"-B ";		break;
				case Knight:	cout<<"-K ";		break;
				case Queen:		cout<<"-W ";		break;
				case King:		cout<<"-X ";		break;
				case 0:			cout<<" . ";		break;
			}
		}
		cout<<"|  "<<Y-1;
		if (Y >2) cout<<"\n  |                        |"<<"\n";
		if (Y==2) cout<<"\n  |________________________|"<<"\n";
	}
	cout<<"\n\n"; 
}

//Prints the help commands on screen
void PrintHelp()
{
	char Temp;
	cout<<"\n\n**********************************************************"<<"\n\n";
	cout<<"   MigoyaChess C++ (c) 2010 \n\n";
	cout<<"   Pieces: \n\n";
	cout<<"   X=King \n";
	cout<<"   W=Queen \n";
	cout<<"   T=Rook \n";
	cout<<"   B=Bishop \n";
	cout<<"   K=Knight \n";
	cout<<"   i=Pawn \n\n";
	cout<<"   Black pieces are represented with a '-' infront of them. \n";
	cout<<"   Computer will always play black side. \n\n";
	cout<<"   Enter your move in the form of: X1 Y1 X2 Y2  (Example:  5254)\n";
	cout<<"   Castles are not yet implemented. \n";
	cout<<"   Checks and check-mates are not yet implemented. \n\n";
	cout<<"   Written by:  Alejandro Migoya. \n\n";
	cout<<"**********************************************************"<<"\n\n";
	cout<<"   Press any key then ENTER to continue.";
	cin>>Temp;
	PrintBoard();	
}

//Makes a move on board for human or computer
void MakeMovement(int X1, int Y1, int X2, int Y2, int &Moved, int &Taken)
{
	Taken = Board[X2][Y2];
	Moved = Board[X1][Y1];
    Board[X2][Y2] = Moved;
    Board[X1][Y1] = 0;
    if ((Y2==2) && (Moved== Pawn)) Board[X2][Y2] =  Queen;
    if ((Y2==9) && (Moved==-Pawn)) Board[X2][Y2] = -Queen;  
}

//UnMakes a move on board for human or computer
void UnMakeMovement(int X1, int Y1, int X2, int Y2, int Moved, int Taken)
{
	Board[X1][Y1] = Moved; 
	Board[X2][Y2] = Taken;
}

//Clears both matrixes of attacked squares
void ClearMobility()
{
	for (int X=0; X<=11; X++)  //Goes through the whole board matrix
	{
		for (int Y=0; Y<=11; Y++) 
		{	
			AttackedC[X][Y]=0; 
			AttackedH[X][Y]=0; 
		}
	}
}

//******************************************************************************************
//******************SEARCH FUNCTIONS********************************************************
//******************************************************************************************

//Starts the search process
void StartSearch()
{
	for (int X=0; X<=19; X++)   { Value[X] = 0;    }		//Reset the layers values.
	for (int X=0; X<=3; X++)    { BestMove[X] = 0; }		//Reset the best move found.
	State=GameState();
	Value[0]=32000;
	Value[1]=-32000;
	Abort=0;
	Nodes=0;
	CurrentDepth=1;
	AllComputerMoves();
}

//Search for all computer possible moves in the current board.
void AllComputerMoves()
{
	for (int X=2; X<=9; X++)
	{
		for (int Y=2; Y<=9; Y++)
		{
			if (Board[X][Y]>0) { ComputerMoveGenerator (X, Y, 1); if (Abort==1) { Abort=0; return; } }
		}
	}
}

//Search for all human possible moves in the current board.
void AllHumanMoves()
{
	for (int X=2; X<=9; X++)
	{
		for (int Y=2; Y<=9; Y++)
		{
			if (Board[X][Y]<0) { HumanMoveGenerator (X, Y, 1); if (Abort==1) { Abort=0; return; } }
		}
	}
}

//Simulates a movement on the board for computer.
void ComputerMove(int X1, int Y1, int X2, int Y2)
{
	int Moved, Taken;
	CurrentDepth++;
	Nodes++;
	MakeMovement (X1, Y1, X2, Y2, Moved, Taken);
	if (Taken == -King)	{ Value[CurrentDepth] = Evaluation(); }
	else { Value[CurrentDepth] = Value[CurrentDepth - 2]; AllHumanMoves(); }
	if (Value[CurrentDepth] > Value[CurrentDepth-1] && CurrentDepth > 2) { Value[CurrentDepth-1] = Value[CurrentDepth]; }
	if (Value[CurrentDepth-1] >= Value[CurrentDepth-2] && CurrentDepth > 2) { Abort = 1; }
	if (CurrentDepth == 2 && Value[2] > Value[1])
	{ 
		Value[1] = Value[2];
		BestMove[0] = X1;		BestMove[1]=Y1;
		BestMove[2] = X2;		BestMove[3]=Y2;
	}
	UnMakeMovement (X1, Y1, X2, Y2, Moved, Taken);
    CurrentDepth--;
}

//Simulates a movement on the board for human.
void HumanMove(int X1, int Y1, int X2, int Y2)
{
	int Moved, Taken;
	CurrentDepth++;
	Nodes++;
	MakeMovement (X1, Y1, X2, Y2, Moved, Taken);
	if (Taken == King || CurrentDepth >= Depth)	{ Value[CurrentDepth] = Evaluation(); }
	else { Value[CurrentDepth] = Value[CurrentDepth - 2]; AllComputerMoves(); }
	if (Value[CurrentDepth] < Value[CurrentDepth-1]) { Value[CurrentDepth-1] = Value[CurrentDepth]; }
	if (Value[CurrentDepth-1] <= Value[CurrentDepth-2]) { Abort = 1; }
	UnMakeMovement (X1, Y1, X2, Y2, Moved, Taken);
    CurrentDepth--;
}

//******************************************************************************************
//******************MOVE GENERATOR**********************************************************
//******************************************************************************************

//Move generator for Computer moves
void ComputerMoveGenerator(int X1, int Y1, int MoveType)
{
	switch (Board[X1][Y1])   
	{
		case Pawn:
			if (Y1==8 && Board[X1][Y1-1]==0 && Board[X1][Y1-2]==0 && Board[X1+1][Y1-2]!=-Pawn && Board[X1-1][Y1-2]!=-Pawn)
															ComputerMoveType (X1, Y1, X1, Y1-2, MoveType);
			if (Board[X1][Y1-1] == 0)						ComputerMoveType (X1, Y1, X1, Y1-1, MoveType);
			if (Board[X1+1][Y1-1] < 0)						ComputerMoveType (X1, Y1, X1+1, Y1-1, MoveType);
			if (Board[X1-1][Y1-1] < 0)						ComputerMoveType (X1, Y1, X1-1, Y1-1, MoveType); break;
		case Knight:		
			if (Board[X1+2][Y1+1] <= 0)						ComputerMoveType (X1, Y1, X1+2, Y1+1, MoveType);
			if (Board[X1+2][Y1-1] <= 0)						ComputerMoveType (X1, Y1, X1+2, Y1-1, MoveType);
			if (Board[X1-2][Y1-1] <= 0)						ComputerMoveType (X1, Y1, X1-2, Y1-1, MoveType);
			if (Board[X1-2][Y1+1] <= 0)						ComputerMoveType (X1, Y1, X1-2, Y1+1, MoveType);
			if (Board[X1+1][Y1+2] <= 0)						ComputerMoveType (X1, Y1, X1+1, Y1+2, MoveType);
			if (Board[X1-1][Y1+2] <= 0)						ComputerMoveType (X1, Y1, X1-1, Y1+2, MoveType);
			if (Board[X1+1][Y1-2] <= 0)						ComputerMoveType (X1, Y1, X1+1, Y1-2, MoveType);
			if (Board[X1-1][Y1-2] <= 0)						ComputerMoveType (X1, Y1, X1-1, Y1-2, MoveType); break;
		case King:		
			if (Board[X1+1][Y1+1] <= 0)						ComputerMoveType (X1, Y1, X1+1, Y1+1, MoveType);
			if (Board[X1-1][Y1+1] <= 0)						ComputerMoveType (X1, Y1, X1-1, Y1+1, MoveType);
			if (Board[X1-1][Y1-1] <= 0)						ComputerMoveType (X1, Y1, X1-1, Y1-1, MoveType);
			if (Board[X1+1][Y1-1] <= 0)						ComputerMoveType (X1, Y1, X1+1, Y1-1, MoveType);
			if (Board[X1][Y1-1] <= 0)						ComputerMoveType (X1, Y1, X1,   Y1-1, MoveType);
			if (Board[X1][Y1+1] <= 0)						ComputerMoveType (X1, Y1, X1,   Y1+1, MoveType);
			if (Board[X1-1][Y1] <= 0)						ComputerMoveType (X1, Y1, X1-1,   Y1, MoveType);
			if (Board[X1+1][Y1] <= 0)						ComputerMoveType (X1, Y1, X1+1,   Y1, MoveType); break;
		case Bishop:		
			ComputerLongMoves(X1, Y1, 1, 1, MoveType);		ComputerLongMoves(X1, Y1, 1, -1, MoveType);
			ComputerLongMoves(X1, Y1, -1, -1, MoveType);	ComputerLongMoves(X1, Y1, -1, 1, MoveType); break;
		case Rook:		
			ComputerLongMoves(X1, Y1, 0, 1, MoveType);		ComputerLongMoves(X1, Y1, 0, -1, MoveType);
			ComputerLongMoves(X1, Y1, 1, 0, MoveType);		ComputerLongMoves(X1, Y1, -1, 0, MoveType); break;
		case Queen:		
			ComputerLongMoves(X1, Y1, 0, 1, MoveType);		ComputerLongMoves(X1, Y1, 0, -1, MoveType);
			ComputerLongMoves(X1, Y1, 1, 0, MoveType);		ComputerLongMoves(X1, Y1, -1, 0, MoveType);
			ComputerLongMoves(X1, Y1, 1, 1, MoveType);		ComputerLongMoves(X1, Y1, 1, -1, MoveType);
			ComputerLongMoves(X1, Y1, -1, -1, MoveType);	ComputerLongMoves(X1, Y1, -1, 1, MoveType); break;
	}
}

//Generates computer long moves
void ComputerLongMoves(int X1, int Y1, int MX, int MY, int MoveType)
{
	int CX = MX;
	int CY = MY;
	int Square;
	while (Board[X1+CX][Y1+CY]!=2)
	{
		Square = Board[X1 + CX][Y1 + CY];   
		if (Square == 0) { ComputerMoveType(X1, Y1, X1 + CX, Y1 + CY, MoveType); }
		if (Square  < 0) { ComputerMoveType(X1, Y1, X1 + CX, Y1 + CY, MoveType);  return; }
		if (Square  > 0) { return; }	
		CX = CX + MX;
		CY = CY + MY;
	}	
}

//Generates an action upon a computer move
void ComputerMoveType(int X1, int Y1, int X2, int Y2, int MoveType)
{
	if (MoveType==1) { ComputerMove (X1, Y1, X2, Y2); return; }
	if (MoveType==3) { AttackedC[X2][Y2] = AttackedC[X2][Y2] + 1; return; }
}

//Move generator for Human moves
void HumanMoveGenerator(int X1, int Y1, int MoveType)
{
	switch (Board[X1][Y1])   
	{
		case -Pawn:	
			if (Y1==3 && Board[X1][Y1+1]==0 && Board[X1][Y1+2]==0 && Board[X1+1][Y1+2]!=Pawn && Board[X1-1][Y1+2]!=Pawn)
																	HumanMoveType (X1, Y1, X1, Y1+2, MoveType);
			if (Board[X1][Y1+1] == 0)								HumanMoveType (X1, Y1, X1, Y1+1, MoveType);
			if (Board[X1+1][Y1+1] > 0 && Board[X1+1][Y1+1]!=2)		HumanMoveType (X1, Y1, X1+1, Y1+1, MoveType);
			if (Board[X1-1][Y1+1] > 0 && Board[X1-1][Y1+1]!=2)		HumanMoveType (X1, Y1, X1-1, Y1+1, MoveType); break;
		case -Knight:		
			if (Board[X1+2][Y1+1] >= 0 && Board[X1+2][Y1+1]!=2)		HumanMoveType (X1, Y1, X1+2, Y1+1, MoveType);
			if (Board[X1+2][Y1-1] >= 0 && Board[X1+2][Y1-1]!=2)		HumanMoveType (X1, Y1, X1+2, Y1-1, MoveType);
			if (Board[X1-2][Y1-1] >= 0 && Board[X1-2][Y1-1]!=2)		HumanMoveType (X1, Y1, X1-2, Y1-1, MoveType);
			if (Board[X1-2][Y1+1] >= 0 && Board[X1-2][Y1+1]!=2)		HumanMoveType (X1, Y1, X1-2, Y1+1, MoveType);
			if (Board[X1+1][Y1+2] >= 0 && Board[X1+1][Y1+2]!=2)		HumanMoveType (X1, Y1, X1+1, Y1+2, MoveType);
			if (Board[X1-1][Y1+2] >= 0 && Board[X1-1][Y1+2]!=2)		HumanMoveType (X1, Y1, X1-1, Y1+2, MoveType);
			if (Board[X1+1][Y1-2] >= 0 && Board[X1+1][Y1-2]!=2)		HumanMoveType (X1, Y1, X1+1, Y1-2, MoveType);
			if (Board[X1-1][Y1-2] >= 0 && Board[X1-1][Y1-2]!=2)		HumanMoveType (X1, Y1, X1-1, Y1-2, MoveType); break;
		case -King:		
			if (Board[X1+1][Y1+1] >= 0 && Board[X1+1][Y1+1]!=2)		HumanMoveType (X1, Y1, X1+1, Y1+1, MoveType);
			if (Board[X1-1][Y1+1] >= 0 && Board[X1-1][Y1+1]!=2)		HumanMoveType (X1, Y1, X1-1, Y1+1, MoveType);
			if (Board[X1-1][Y1-1] >= 0 && Board[X1-1][Y1-1]!=2)		HumanMoveType (X1, Y1, X1-1, Y1-1, MoveType);
			if (Board[X1+1][Y1-1] >= 0 && Board[X1+1][Y1-1]!=2)		HumanMoveType (X1, Y1, X1+1, Y1-1, MoveType);
			if (Board[X1][Y1-1] >= 0   && Board[X1][Y1-1]!=2)		HumanMoveType (X1, Y1, X1,   Y1-1, MoveType);
			if (Board[X1][Y1+1] >= 0   && Board[X1][Y1+1]!=2)		HumanMoveType (X1, Y1, X1,   Y1+1, MoveType);
			if (Board[X1-1][Y1] >= 0   && Board[X1-1][Y1]!=2)		HumanMoveType (X1, Y1, X1-1,   Y1, MoveType);
			if (Board[X1+1][Y1] >= 0   && Board[X1+1][Y1]!=2)		HumanMoveType (X1, Y1, X1+1,   Y1, MoveType); break;
		case -Bishop:		
			HumanLongMoves(X1, Y1, 1, 1, MoveType);					HumanLongMoves(X1, Y1, 1, -1, MoveType);
			HumanLongMoves(X1, Y1, -1, -1, MoveType);				HumanLongMoves(X1, Y1, -1, 1, MoveType); break;
		case -Rook:		
			HumanLongMoves(X1, Y1, 0, 1, MoveType);					HumanLongMoves(X1, Y1, 0, -1, MoveType);
			HumanLongMoves(X1, Y1, 1, 0, MoveType);					HumanLongMoves(X1, Y1, -1, 0, MoveType); break;
		case -Queen:		
			HumanLongMoves(X1, Y1, 0, 1, MoveType);					HumanLongMoves(X1, Y1, 0, -1, MoveType);
			HumanLongMoves(X1, Y1, 1, 0, MoveType);					HumanLongMoves(X1, Y1, -1, 0, MoveType);
			HumanLongMoves(X1, Y1, 1, 1, MoveType);					HumanLongMoves(X1, Y1, 1, -1, MoveType);
			HumanLongMoves(X1, Y1, -1, -1, MoveType);				HumanLongMoves(X1, Y1, -1, 1, MoveType); break;
	}
}

//Generates human long moves
void HumanLongMoves(int X1, int Y1, int MX, int MY, int MoveType)
{
	int CX = MX;
	int CY = MY;
	int Square;
	while (Board[X1+CX][Y1+CY]!=2)
	{
		Square = Board[X1 + CX][Y1 + CY];   
		if (Square == 0) { HumanMoveType(X1, Y1, X1 + CX, Y1 + CY, MoveType); }
		if (Square  > 0) { HumanMoveType(X1, Y1, X1 + CX, Y1 + CY, MoveType);  return; }
		if (Square  < 0) { return; }	
		CX = CX + MX;
		CY = CY + MY;
	}	
}

//Generates an action upon a Human move
void HumanMoveType(int X1, int Y1, int X2, int Y2, int MoveType)
{
	if (MoveType==1) { HumanMove (X1, Y1, X2, Y2); return; }
	if (MoveType==3) { AttackedH[X2][Y2] = AttackedH[X2][Y2] + 1; return; }
}

//******************************************************************************************
//******************EVALUATION FUNCTIONS****************************************************
//******************************************************************************************

//Fast board evaluation - Evaluates board value + board positions - Sate of Game.
int Evaluation()
{
	int Evaluation2=0;
	if (State==1) { Evaluation2 = Material() + Development(); }
	if (State==2) { Evaluation2 = Material() + Development() + HumanAproach(6, 8) - ComputerAproach(6, 3); }
	if (State==3) 
	{ 
		int XCK=0, YCK=0, XHK=0, YHK=0;
		KingCoordinates(XCK, YCK, XHK, YHK);
		Evaluation2 = Material() + Development() + PossibleCrown() + HumanAproach(XCK, YCK) - ComputerAproach(XHK, YHK); 	
	}
	return Evaluation2;
}

//Calculates game state (oppening, middle, end)
int GameState()
{
	int GameState2=2;
	if (MoveNo < 12) { GameState2 = 1; }
	if (MoveNo > 30) { GameState2 = 3; }
	return GameState2;
}

//Calculates all board piece values.
int Material()
{
	int Material2=0;
	for (int X=2; X<=9; X++)
	{
		for (int Y=2; Y<=9; Y++) { Material2 += Board[X][Y]; }
	}
	return Material2;
}

//Evaluates how well the pieces have been developed.
int Development()
{
	int Development2=0;
	if (Board[6][6] == Pawn) { Development2 = Development2 + 5; }
	if (Board[5][6] == Pawn) { Development2 = Development2 + 5; }
	if (Board[3][9] == 0)    { Development2 = Development2 + 2; }
	if (Board[4][9] == 0)    { Development2 = Development2 + 2; }
	if (Board[7][9] == 0)    { Development2 = Development2 + 4; }
	if (Board[8][9] == 0)    { Development2 = Development2 + 2; }
	if (Board[3][2] == 0)    { Development2 = Development2 - 2; }
	if (Board[4][2] == 0)    { Development2 = Development2 - 2; }
	if (Board[7][2] == 0)    { Development2 = Development2 - 4; }
	if (Board[8][2] == 0)    { Development2 = Development2 - 4; }
	for (int X=5; X<=6; X++)
	{
		for (int Y=4; Y<=7; Y++) 
		{ 
			if (Board[X][Y] == Pawn)    { Development2 = Development2 + 4; }
			if (Board[X][Y] == -Pawn)   { Development2 = Development2 - 4; }
		}
	}
	return Development2;
}

//Evaluates how close a pawn is to be crowned.
int PossibleCrown()
{
	int PossibleCrown2=0;
	for (int X=2; X<=9; X++)
	{
		if (Board[X][3] ==  Pawn) { PossibleCrown2 = PossibleCrown2 + 15; }
		if (Board[X][4] ==  Pawn) { PossibleCrown2 = PossibleCrown2 + 10; }
		if (Board[X][5] ==  Pawn) { PossibleCrown2 = PossibleCrown2 +  5; }
		if (Board[X][8] == -Pawn) { PossibleCrown2 = PossibleCrown2 - 15; }
		if (Board[X][7] == -Pawn) { PossibleCrown2 = PossibleCrown2 + 10; }
		if (Board[X][6] == -Pawn) { PossibleCrown2 = PossibleCrown2 +  5; }
	}
	return PossibleCrown2;
}

//Evaluates how close a piece is to it큦 objective for human.
int HumanAproach(int X2, int Y2)
{
	double HumanAproach2=0;
	double Temp=0;
	for (int X=2; X<=9; X++)
	{
		for (int Y=2; Y<=9; Y++) 
		{ 
			if (Board[X][Y] < -Pawn && Board[X][Y] > -King) 
			{ 
				Temp=(abs(X - X2) ^ 2)+(abs(Y - Y2) ^ 2);
				HumanAproach2 = HumanAproach2 + sqrt(Temp); 
			}
			if (State==3) 
			{ 
				if (Board[X][Y]==-King) 
				{ 
					Temp=(abs(X - X2) ^ 2)+(abs(Y - Y2) ^ 2); 
					HumanAproach2 = HumanAproach2 + (4*(sqrt(Temp))); 
				} 
			}
		}
	}
	return floor(HumanAproach2);
}

//Evaluates how close a piece is to it큦 objective for computer.
int ComputerAproach(int X2, int Y2)
{
	double ComputerAproach2=0;
	double Temp=0;
	for (int X=2; X<=9; X++)
	{
		for (int Y=2; Y<=9; Y++) 
		{ 
			if (Board[X][Y] > Pawn && Board[X][Y] < King) 
			{ 
				Temp=(abs(X - X2) ^ 2)+(abs(Y - Y2) ^ 2); 
				ComputerAproach2 = ComputerAproach2 + sqrt(Temp); 
			}
			if (State==3) 
			{ 
				if (Board[X][Y]==King) 
				{ 
					Temp=(abs(X - X2) ^ 2)+(abs(Y - Y2) ^ 2); 
					ComputerAproach2 = ComputerAproach2 + (4*sqrt(Temp)); 
				} 
			}
		}
	}
	return floor(ComputerAproach2);
}

//Returns the King큦 coordinates
void KingCoordinates(int &XCK, int &YCK, int &XHK, int &YHK)
{
	for (int X=2; X<=9; X++)
	{
		for (int Y=2; Y<=9; Y++) 
		{ 
			if (Board[X][Y]==-King) { XHK=X; YHK=Y; }
			if (Board[X][Y]== King) { XCK=X; YCK=Y; }
		}
	}
}
