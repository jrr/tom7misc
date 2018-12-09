
default: chess_test.exe rungames.exe

STATS2013= stats-2013-01.txt stats-2013-02.txt stats-2013-03.txt stats-2013-04.txt stats-2013-05.txt stats-2013-06.txt stats-2013-07.txt stats-2013-08.txt stats-2013-09.txt stats-2013-10.txt stats-2013-11.txt stats-2013-12.txt

STATS2014= stats-2014-01.txt stats-2014-02.txt stats-2014-03.txt stats-2014-04.txt stats-2014-05.txt stats-2014-06.txt stats-2014-07.txt stats-2014-08.txt stats-2014-09.txt stats-2014-10.txt stats-2014-11.txt stats-2014-12.txt

STATS2015= stats-2015-01.txt stats-2015-02.txt stats-2015-03.txt stats-2015-04.txt stats-2015-05.txt stats-2015-06.txt stats-2015-07.txt stats-2015-08.txt stats-2015-09.txt stats-2015-10.txt stats-2015-11.txt stats-2015-12.txt

STATS2016= stats-2016-01.txt stats-2016-02.txt stats-2016-03.txt stats-2016-04.txt stats-2016-05.txt stats-2016-06.txt stats-2016-07.txt stats-2016-08.txt stats-2016-09.txt stats-2016-10.txt stats-2016-11.txt stats-2016-12.txt

STATS2017= stats-2017-01.txt stats-2017-02.txt stats-2017-03.txt stats-2017-04.txt stats-2017-05.txt stats-2017-06.txt stats-2017-07.txt stats-2017-08.txt stats-2017-09.txt stats-2017-10.txt stats-2017-11.txt stats-2017-12.txt

STATS2018= stats-2018-01.txt stats-2018-02.txt stats-2018-03.txt stats-2018-04.txt stats-2018-05.txt stats-2018-06.txt stats-2018-07.txt stats-2018-08.txt stats-2018-09.txt stats-2018-10.txt stats-2018-11.txt

allstats : $(STATS2013) $(STATS2014) $(STATS2015) $(STATS2016) $(STATS2017) $(STATS2018)


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

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/arcfour.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o ../cc-lib/city/city.o ../cc-lib/textsvg.o $(RE2_OBJECTS)

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

