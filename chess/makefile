
default: chess_test.exe rungames.exe

allstats : stats-2017-04.txt stats-2017-05.txt stats-2017-06.txt stats-2017-07.txt stats-2017-08.txt stats-2017-09.txt stats-2017-10.txt stats-2017-11.txt stats-2017-12.txt stats-2018-01.txt stats-2018-02.txt stats-2018-03.txt stats-2018-04.txt stats-2018-05.txt stats-2018-06.txt stats-2018-07.txt stats-2018-08.txt


CXXFLAGS=--std=c++17 -Wall -Wno-deprecated -Wno-sign-compare
OPT=-O2
SYMBOLS=-g

# Tried switching back to mingw/w64...
# CXX=g++
# CC=gcc
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

INCLUDES=-I "../cc-lib" -I "../cc-lib/city" -I "../cc-lib/re2" -I ../fceulib
CPPFLAGS=-m64 -D__MINGW32__ -DHAVE_ALLOCA -D_GLIBCXX_HAS_GTHREADS -mthreads $(INCLUDES)

# without static, can't find lz or lstdcxx maybe?
LFLAGS=-m64 -mthreads -lwinpthread -L. -static

RE2_OBJECTS=../cc-lib/re2/bitstate.o ../cc-lib/re2/compile.o ../cc-lib/re2/dfa.o ../cc-lib/re2/filtered_re2.o ../cc-lib/re2/mimics_pcre.o ../cc-lib/re2/nfa.o ../cc-lib/re2/onepass.o ../cc-lib/re2/parse.o ../cc-lib/re2/perl_groups.o ../cc-lib/re2/prefilter.o ../cc-lib/re2/prefilter_tree.o ../cc-lib/re2/prog.o ../cc-lib/re2/re2.o ../cc-lib/re2/regexp.o ../cc-lib/re2/set.o ../cc-lib/re2/simplify.o ../cc-lib/re2/stringpiece.o ../cc-lib/re2/tostring.o ../cc-lib/re2/unicode_casefold.o ../cc-lib/re2/unicode_groups.o ../cc-lib/re2/util/rune.o ../cc-lib/re2/util/strutil.o

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/arcfour.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o ../cc-lib/city/city.o $(RE2_OBJECTS)

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

../cc-lib/%.o : ../cc-lib/%.cc
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '_'"

chess_test.exe : chess_test.o chess.o pgn.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

rungames.exe : rungames.o chess.o pgn.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

maketable.exe : maketable.o chess.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe $(CCLIB_OBJECTS)


# This depends on rungames.exe, but not including it because I don't want to recompute stats every single time.
stats-%.txt : d:\\chess\\lichess_db_standard_rated_%.pgn
	nice -n 19 ./rungames.exe $< > $@

