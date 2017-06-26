default : text.exe 
text.exe : text.o /djgpp/lib/liballeg.a
	gcc -Wall -O2 -o text.exe text.o -lalleg
text.o : text.c 
	gcc -Wall -O2 -o text.o text.c -c 
