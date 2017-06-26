default : chksum.exe 
chksum.exe : chksum.o 
	gcc -Wall -O2 -o chksum.exe chksum.o 
chksum.o : chksum.c 
	gcc -Wall -O2 -o chksum.o chksum.c -c 
