
FLAGS = -mpentiumpro -march=pentiumpro -fno-exceptions -O6

default : squares.exe 
squares.exe : squares.o 
	gcc -Wall -o squares.exe squares.o -s
squares.o : squares.c 
	gcc -Wall  -o squares.o squares.c -c $(FLAGS) 

