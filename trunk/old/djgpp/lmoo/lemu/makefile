CCFLAGS = -O2 -Wall -g

default : lemu.exe

lemu.exe : lemu.o lob.o func.o
	gcc $(CCFLAGS) -o lemu.exe func.o lob.o lemu.o -lstdcx

func.o : func.cc lob.h func.h
	gcc $(CCFLAGS) -c -o func.o func.cc

lemu.o : lemu.cc lob.h func.h
	gcc $(CCFLAGS) -c -o lemu.o lemu.cc

lob.o : lob.cc lob.h func.h
	gcc $(CCFLAGS) -c -o lob.o lob.cc

clean :
	del lemu.o
	del lob.o
	del func.o

veryclean : clean
	del lemu.exe

compact : lemu.exe
	strip lemu.exe
	djp lemu.exe
	make clean