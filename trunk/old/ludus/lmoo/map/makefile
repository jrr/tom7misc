TARG = map
CC = gcc
CPPFLAGS = -g -O  -Wall -I../src/ -DLUDUS_OFFICIAL_BUILD -DLUDUS_CLIENT 
CXX = gcc

REQ_OFILES = ../src/text.o ../src/dirt.o ../src/window.o ../src/auth.o ../src/limp.o ../src/dict.o maplimp.o ../src/lmap.o

default : $(TARG).exe 

$(TARG).exe : $(TARG).o $(REQ_OFILES)
	gcc -o $(TARG).exe $(TARG).o $(REQ_OFILES) -lalleg -lstdcxx
