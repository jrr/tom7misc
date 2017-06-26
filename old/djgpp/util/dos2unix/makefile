target = dos2unix
CCFLAGS = -O2 -Wall
CC = gcc

default : $(target).exe

$(target).exe : $(target).o
	$(CC) $(CCFLAGS) -o $(target).exe $(target).o -lstdcx

clean :
	del $(target).o

veryclean : clean
	del $(target).exe

compact : $(target).exe clean
	strip $(target).exe
	djp $(target).exe

