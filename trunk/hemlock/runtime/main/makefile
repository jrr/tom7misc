#compile.sml builds this, so we can probably get rid of makefile*

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

all: main.tal

main.tal: main.pop
	$(POPCORN) -c main.pop

clean:
	rm -f *~ *.o *.to *.pil *.obj *.tal* *.lst
