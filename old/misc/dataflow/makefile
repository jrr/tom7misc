TARG = prime

OFILES = prime.o

CPP = gcc
CPPFLAGS = -Wall -O3 -g -I. -ansi -pedantic

default : $(TARG)

$(TARG) : $(OFILES)
	$(CPP) $(CPPFLAGS) -o $(TARG) $(OFILES)

clean :
	rm -f *.o

backup :
	tar -cv *.h *.txt *.cc *.c makefile > `date +backup.%m.%d.%y.tar.gz`

