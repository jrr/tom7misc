# You need to set TALCLIB and POPLIB before compiling.
POP_IN_POP=
USE_LOADER=

MAKE_LOADABLE=y
NO_STACK_TRACE=y

ifndef WINDIR
LIBFILES =
TALCFLAGS =
endif

include ./Makefile.inc

all: mainfn.tal runtime.tal

mainfn.tal: mainfn.pop
	$(POPCORN) $(POPFLAGS) --export-syms -c mainfn.pop


runtime.tal: runtime.pop
	$(POPCORN) -c runtime.pop

clean:
	rm -f *~ *.o *.to *.pil *.obj *.tal* *.lst
