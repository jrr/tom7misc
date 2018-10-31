
default: chess_test.exe

CXXFLAGS=--std=c++17 -Wall -Wno-deprecated -Wno-sign-compare
OPT=-O2
SYMBOLS=-g

# Tried switching back to mingw/w64...
# CXX=g++
# CC=gcc
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

INCLUDES=-I "../cc-lib" -I "../cc-lib/re2" -I ../fceulib
CPPFLAGS=-m64 -D__MINGW32__ -DHAVE_ALLOCA -D_GLIBCXX_HAS_GTHREADS -mthreads $(INCLUDES)

# without static, can't find lz or lstdcxx maybe?
LFLAGS=-m64 -mthreads -lwinpthread -L. -static

RE2_OBJECTS=../cc-lib/re2/bitstate.o ../cc-lib/re2/compile.o ../cc-lib/re2/dfa.o ../cc-lib/re2/filtered_re2.o ../cc-lib/re2/mimics_pcre.o ../cc-lib/re2/nfa.o ../cc-lib/re2/onepass.o ../cc-lib/re2/parse.o ../cc-lib/re2/perl_groups.o ../cc-lib/re2/prefilter.o ../cc-lib/re2/prefilter_tree.o ../cc-lib/re2/prog.o ../cc-lib/re2/re2.o ../cc-lib/re2/regexp.o ../cc-lib/re2/set.o ../cc-lib/re2/simplify.o ../cc-lib/re2/stringpiece.o ../cc-lib/re2/tostring.o ../cc-lib/re2/unicode_casefold.o ../cc-lib/re2/unicode_groups.o ../cc-lib/re2/util/rune.o ../cc-lib/re2/util/strutil.o

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/arcfour.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o $(RE2_OBJECTS)

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

../cc-lib/%.o : ../cc-lib/%.cc
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '_'"

chess_test.exe : chess_test.o chess.o pgn.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe $(CCLIB_OBJECTS)
