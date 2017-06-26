CCFLAGS = -O2 -Wall

default : loop.exe

loop.exe : loop.o
	gcc $(CCFLAGS) -o loop.exe loop.o -lstdcx

loop.o : loop.cc
	gcc $(CCFLAGS) -c -o loop.o loop.cc

clean :
	del loop.o

veryclean : clean
	del loop.exe

compact : loop.exe
	strip loop.exe
	djp loop.exe
