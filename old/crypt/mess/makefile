default : mess.exe 
mess.exe : mess.o 
	gcc -Wall -O2 -o mess.exe mess.o 
mess.o : mess.c 
	gcc -Wall -O2 -o mess.o mess.c -c 

compact : default
	strip mess.exe
	djp mess.exe
	del mess.o