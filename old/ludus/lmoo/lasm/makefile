CCFLAGS = -O2 -Wall

default : lasm.exe

lasm.exe : lasm.o
	gcc $(CCFLAGS) -o lasm.exe lasm.o -lstdcx

lasm.o : lasm.cc
	gcc $(CCFLAGS) -c -o lasm.o lasm.cc

clean :
	del lasm.o

veryclean : clean
	del lasm.exe

compact : lasm.exe
	strip lasm.exe
	djp lasm.exe
	del lasm.o

rot13 : rot13.lsm lasm.exe
	lasm rot13.lsm ..\\lemu\\rot13.lob
