TARG = authdrv
CC = gcc
CFLAGS = -O2 -Wall -I../src/ -DLUDUS_OFFICIAL_BUILD

default : $(TARG).exe 

$(TARG).exe : $(TARG).o ../src/auth.o
	gcc -o $(TARG).exe $(TARG).o ../src/auth.o