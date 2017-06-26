# You need to set TALCLIB and POPLIB before compiling.
POP_IN_POP=
USE_LOADER=

MAKE_LOADABLE=
NO_STACK_TRACE=

ifndef WINDIR
LIBFILES =
TALCFLAGS =
endif

include loader/Makefile.inc

all: getwitvec.exe

getwitvec.exe: getwitvec.to
	$(TALC) $(TALCFLAGS) -o getwitvec.exe $^ $(TALCLIBS)

clean:
	rm -f *~ *.o *.to *.pil *.obj *.tal* *.lst
