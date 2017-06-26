
default : sudoku.exe

sudoku.exe : sudoku.cm sudoku.sml makefile
	mlton -const "Exn.keepHistory false" -default-ann "sequenceUnit true" -output sudoku.exe sudoku.cm
