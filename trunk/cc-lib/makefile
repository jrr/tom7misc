
default : heap_test.exe minmax-heap_test.exe rle_test.exe interval-tree_test.exe threadutil_test.exe color-util_test.exe textsvg_test.exe lines_test.exe image_test.exe util_test.exe randutil_test.exe json_test.exe arcfour_test.exe lastn-buffer_test.exe list-util_test.exe edit-distance_test.exe re2_test.exe $(TESTCOMPILE)

TESTCOMPILE=stb_image_write.o stb_image.o dr_wav.o bounds.o simpledxf.o

BASE=base/logging.o base/stringprintf.o

# for 64 bits on windows
# CXX=x86_64-w64-mingw32-g++
# CC=x86_64-w64-mingw32-g++
# CXXFLAGS=-Wall -I. -DWIN32 --std=c++17 -O2 -static
# SOCK_LFLAGS= -lws2_32

# Linux
CXX=g++
CC=gcc
CXXFLAGS=-Wall -I. --std=c++17 -O2
SOCK_LFLAGS=

# For linux, others...
# (pthreads can't be linked statically)
# CXXFLAGS=-I. --std=c++14 -O2 -DNDEBUG=1

RE2_OBJECTS=re2/bitstate.o re2/compile.o re2/dfa.o re2/filtered_re2.o re2/mimics_pcre.o re2/nfa.o re2/onepass.o re2/parse.o re2/perl_groups.o re2/prefilter.o re2/prefilter_tree.o re2/prog.o re2/re2.o re2/regexp.o re2/set.o re2/simplify.o re2/stringpiece.o re2/tostring.o re2/unicode_casefold.o re2/unicode_groups.o re2/util/rune.o re2/util/strutil.o

heap_test.o : heap_test.cc heap.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

heap_test.exe : heap_test.o
	$(CXX) $(CXXFLAGS) $^ -o $@

minmax-heap_test.o : minmax-heap_test.cc minmax-heap.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

minmax-heap_test.exe : minmax-heap_test.o arcfour.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

list-util_test.o : list-util_test.cc list-util.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

list-util_test.exe : list-util_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

rle_test.o : rle_test.cc rle.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

rle_test.exe : rle_test.o rle.o $(BASE) arcfour.o
	$(CXX) $(CXXFLAGS) $^ -o $@

arcfour_test.exe : arcfour_test.o arcfour.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

md5_test.exe : md5_test.o md5.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

lastn-buffer_test.o : lastn-buffer_test.cc lastn-buffer.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

lastn-buffer_test.exe : lastn-buffer_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

interval-tree_test.exe : interval-tree_test.o $(BASE) arcfour.o
	$(CXX) $(CXXFLAGS) $^ -o $@

simpledxf.o : simpledxf.cc simpledxf.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

threadutil_test.exe : threadutil.h threadutil_test.o $(BASE)
	$(CXX) $(CXXFLAGS) threadutil_test.o $(BASE) -o $@ -lpthread

color-util_test.exe : color-util.o color-util_test.o stb_image_write.o arcfour.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

textsvg_test.exe : textsvg.o textsvg_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

lines_test.exe : lines_test.o arcfour.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

image_test.exe : image_test.o arcfour.o image.o stb_image.o stb_image_write.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

util_test.exe : util_test.o util.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

randutil_test.exe : randutil.h randutil_test.o arcfour.o $(BASE)
	$(CXX) $(CXXFLAGS) randutil_test.o arcfour.o $(BASE) -o $@

json_test.exe : json_test.o $(BASE)
	$(CXX) $(CXXFLAGS) json_test.o $(BASE) -o $@

edit-distance_test.exe : edit-distance_test.o edit-distance.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

re2_test.exe : re2_test.o $(RE2_OBJECTS) $(BASE) 
	$(CXX) $(CXXFLAGS) $^ -o $@

web_test.exe : web_test.o web.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(SOCK_LFLAGS) -lpthread

clean :
	rm -f *.o *.exe
