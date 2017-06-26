# standard flags and setting for compiling and linking popcorn files
# Each file including this file should define HOME_DIR as the root of
# the talc directory.

# Makefile defs 
# MAKE_LOADABLE  compile popcorn file so that it may be dynamically linked
# USE_LOADER	 compile popcorn file so that it may dynamically link in code	
# NO_STACK_TRACE compile popcorn file with stack trace facilities
# DYN_UPDATE	 make file dynamically updateable
# POP_IN_POP     use the popcorn in popcorn compiler
# USE_RTCG       use runtime-code-generation
# WINDIR         compiling under windows.

### Standard definitions

RUNTIME_DIR = $(TALCLIB)
POPLIB_DIR = $(POPLIB)

### Operating system dependencies.

ifdef WINDIR
CC=cl
O=obj
A=lib
LINK=link
else
CC=gcc
O=o
A=a
LINK=ld
endif

### Standard definitions (may be overridden/extended below)
POPCORN  = popcorn.exe
POPFLAGS = -D FLOATS
LIBFILES += core list array string char hashtable math
STDLIBS  += stdlib pop_runtime

TALC      = talc.exe 
TALCFLAGS = --verify-link --verify-program
PRELUDE   = prelude
TALCLIBS  = 
## Default for assembler is now --TALC.
ASSEMBLER = 

ifdef USE_LOADER
#    TALCFLAGS += $(ASSEMBLER) --register-trusted-syms --no-internals
   TALCFLAGS += $(ASSEMBLER) --register-trusted-syms
   TALCFLAGS += --register-syms $(PRELUDE)
   STDLIBS += loader
   export=y
endif
ifdef MAKE_LOADABLE
   POPFLAGS += --loadable
#      NO_STACK_TRACE=y
   export=y
endif
ifdef export
   POPFLAGS += --export-syms 
   LIBFILES += dlpop 
   ifdef DYN_UPDATE
      POPFLAGS += --notice-updates
   endif
endif

ifndef NO_STACK_TRACE
   POPFLAGS += --stack-trace 
endif

ifdef USE_RTCG
  TALCFLAGS += --no-internals
  CYCLONELIB = $(RUNTIME_DIR)/cyclonelib.$(O)
else
  CYCLONELIB = 
endif

POPFLAGS += $(addprefix $(POPLIB_DIR)/, $(addsuffix .$(O), $(LIBFILES)))
TALCLIBS  = $(addprefix $(POPLIB_DIR)/, $(addsuffix .to, $(LIBFILES))) \
	    $(RUNTIME_DIR)/$(PRELUDE).to $(addprefix --std-lib , $(STDLIBS)) \
            $(CYCLONELIB)


### Implicit rules

%.tal: %.pop
	$(POPCORN) $(POPFLAGS) --generate-tal-only $<

%.to %.$(O) %.tal: %.pop
	$(POPCORN) $(POPFLAGS) -c $<

%.to %.$(O): %.tal
	$(TALC) $(TALCFLAGS) -c $<

%.exe: %.to
	$(TALC) $(TALCFLAGS) -o $@ $< $(TALCLIBS)

%.to %.(O) %.tal: %.patch
	$(POPCORN) $(POPFLAGS) -c $<

%.obj: %.c
	$(CC) -Z7 /MT /Fo$@ /c $<

%.o: %.c 
	$(CC) -c -o $@ $<

