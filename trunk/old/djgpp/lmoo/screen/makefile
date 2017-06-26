TARG = screen
CC = gcc
CPPFLAGS = -O2 -Wall -I../src/ -DLUDUS_OFFICIAL_BUILD -DLUDUS_CLIENT

REQ_OFILES = ../src/text.o ../src/dirt.o

default : $(TARG).exe 

$(TARG).exe : $(TARG).o $(REQ_OFILES)
	gcc -o $(TARG).exe $(TARG).o $(REQ_OFILES) -lalleg -lstdcxx