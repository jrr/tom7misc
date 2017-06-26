TARG = interl
CC = gcc
CFLAGS = -g -O2 -Wall
CPPFLAGS = -g -O2 -Wall
CXX = gcc

REQ_OFILES = 

default : $(TARG).exe 

$(TARG).exe : $(TARG).o $(REQ_OFILES)
	gcc -o $(TARG).exe $(TARG).o $(REQ_OFILES) -lalleg