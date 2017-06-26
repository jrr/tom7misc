TARG = echod
OFILES = ../tsocket/tsocket.o server.o tom.o
CC = g++
CPP = g++
CXX = g++
CXXFLAGS = -Wall -O3 -g -fomit-frame-pointer -fno-exceptions -I../tsocket/
CPPFLAGS = -Wall -O3 -g -fomit-frame-pointer -fno-exceptions -I../tsocket/

default : $(TARG)

$(TARG) : $(OFILES)
	$(CC) $(CXXFLAGS) -o $(TARG) $(OFILES)

clean : 
	rm -f *.o