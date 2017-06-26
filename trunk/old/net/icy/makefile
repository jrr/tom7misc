OFILES = tom.o ../tsocket/tsocket.o icy.o auxil.o parse.o linkbank.o server.o
HFILES = icy.h linkbank.h wordlink.h tom.h auxil.h server.h parse.h ../tsocket/tsocket.h
CC = g++
CPP = g++
CXX = g++
CXXFLAGS = -Wall -O3 -g -I../tsocket/
CPPFLAGS = -Wall -O3 -g -I../tsocket/

default : icy

icy : $(OFILES) $(HFILES)
	$(CC) $(CXXFLAGS) -o icy $(OFILES)

