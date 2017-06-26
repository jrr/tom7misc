DYN_UPDATE=
NO_STACK_TRACE=
include Makefile.inc

SOURCES= io.c dyninit.c

OBJECTS=$(SOURCES:.c=.$(O)) hemrunt_prelude.$(O)

all: hemrunt.tali hemrunt.$(A)

hemrunt.$(A): $(OBJECTS)
	ar r $@ $+
	cp hemrunt.$(A) hemrunt.h hemrunt.tali /afs/cs/usr/tom7/concert/conductor/talc-3.0/popcorn/lib

dyninit.c: dyninit.tmpl
	cp dyninit.tmpl dyninit.c
	genCinit.exe --no-string-def --init-code-preamble "extern int dyninit_hemrunt_prelude(void *unused,void *unused2, void (*f)(void *closure,string sym,rep r,void *p), void *f_closure, int no_init);" --init-code-postamble "dyninit_hemrunt_prelude(look_sym,look_closure,upd_sym,upd_closure,no_init);" hemrunt.tali >> dyninit.c

clean:
	rm -f *.$(O) *.$(A) *.*tal* *~ *.to dyninit.c *.o hemrunt_prelude.o
