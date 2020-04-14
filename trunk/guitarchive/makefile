
default : cleandb.exe chords.exe
all: cleandb.exe chords.exe guitarchive_test.exe chord-parser_test.exe

OPT=-O2
CXXFLAGS=--std=c++17 -Wall $(OPT)
SYMBOLS=-g

CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

CCLIB=../cc-lib

# PLATFORMCFLAGS=-D__MINGW32__ -DHAVE_ALLOCA -D_WIN32_WINNT=0x0602 -D_GLIBCXX_HAS_GTHREADS -mthreads
# PLATFORMCFLAGS=-m64
# PLATFORMLINK=-lwinpthread -lpsapi -L. -static libz.a -Wl,--subsystem,console -L.
PLATFORMLINK=-static

INCLUDES=-I "$(CCLIB)" -I "$(CCLIB)/re2" -I/usr/include
CPPFLAGS=$(INCLUDES) $(PLATFORMCFLAGS) $(SYMBOLS)

# without static, can't find lz or lstdcxx maybe?
#  -L/usr/lib -lprotobuf
LFLAGS=$(PLATFORMLINK)

CCLIB_OBJECTS=$(CCLIB)/base/logging.o $(CCLIB)/base/stringprintf.o $(CCLIB)/util.o $(CCLIB)/textsvg.o $(CCLIB)/arcfour.o $(CCLIB)/edit-distance.o
RE2_OBJECTS=$(CCLIB)/re2/bitstate.o $(CCLIB)/re2/compile.o $(CCLIB)/re2/dfa.o $(CCLIB)/re2/filtered_re2.o $(CCLIB)/re2/mimics_pcre.o $(CCLIB)/re2/nfa.o $(CCLIB)/re2/onepass.o $(CCLIB)/re2/parse.o $(CCLIB)/re2/perl_groups.o $(CCLIB)/re2/prefilter.o $(CCLIB)/re2/prefilter_tree.o $(CCLIB)/re2/prog.o $(CCLIB)/re2/re2.o $(CCLIB)/re2/regexp.o $(CCLIB)/re2/set.o $(CCLIB)/re2/simplify.o $(CCLIB)/re2/stringpiece.o $(CCLIB)/re2/tostring.o $(CCLIB)/re2/unicode_casefold.o $(CCLIB)/re2/unicode_groups.o $(CCLIB)/re2/util/rune.o $(CCLIB)/re2/util/strutil.o

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPT) -c -o $@ $<
	@bash -c "echo -n '.'"

$(CCLIB)/%.o : $(CCLIB)/%.cc
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPT) -c -o $@ $<
	@bash -c "echo -n '_'"

cleandb.exe : cleandb.o guitarchive.o headers.o $(CCLIB_OBJECTS) $(RE2_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

chords.exe : chords.o chord-parser.o guitarchive.o $(CCLIB_OBJECTS) $(RE2_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

guitarchive_test.exe : guitarchive.o guitarchive_test.o $(CCLIB_OBJECTS) $(RE2_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

chord-parser_test.exe : chord-parser.o chord-parser_test.o $(CCLIB_OBJECTS) $(RE2_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe

veryclean : clean
	rm -f $(CCLIB_OBJECTS) $(RE2_OBJECTS)
