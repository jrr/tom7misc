# $Id: makefile,v 1.2 2005/02/03 00:19:05 tom7 Exp $

TARG = ifcd

AUX = tsocket.o tom.o auxil.o

OFILES = server.o sockstuff.o init.o user.o \
         pending.o channel.o

BONUS = makechannel.o trivialc.o dictionary.o scrobblec.o

CPP = g++
CPPFLAGS = -Wall -O3 -g -I. -fno-exceptions

default : $(TARG) tomhash

$(TARG) : $(OFILES) ifc.h $(AUX) $(BONUS)
	$(CPP) $(CPPFLAGS) -o $(TARG) $(OFILES) $(AUX) $(BONUS)

tomhash : auxil.o tomhash.o
	$(CPP) $(CPPFLAGS) -o $@ $< $@.o

clean :
	rm -f *.o

backup :
	mv *.tar.gz ../crap-ifcd/
	tar -cvh *.h *.txt *.cc makefile > `date +backup.%m.%d.%y.tar`
	gzip `date +backup.%m.%d.%y.tar`