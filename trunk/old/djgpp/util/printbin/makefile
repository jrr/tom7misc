CCFLAGS = -O2 -Wall

default : printbin.exe

printbin.exe : printbin.o
	gcc $(CCFLAGS) -o printbin.exe printbin.o -lstdcx

printbin.o : printbin.cc
	gcc $(CCFLAGS) -c -o printbin.o printbin.cc

clean :
	del printbin.o

veryclean : clean
	del printbin.exe

compact : printbin.exe
	strip printbin.exe
	djp printbin.exe
	del printbin.o
