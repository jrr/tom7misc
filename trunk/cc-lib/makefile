
default : heap_test.exe minmax-heap_test.exe rle_test.exe interval-tree_test.exe threadutil_test.exe color-util_test.exe textsvg_test.exe lines_test.exe image_test.exe util_test.exe randutil_test.exe json_test.exe arcfour_test.exe lastn-buffer_test.exe list-util_test.exe $(TESTCOMPILE)

TESTCOMPILE=stb_image_write.o stb_image.o dr_wav.o bounds.o

BASE=base/logging.o base/stringprintf.o

# for 64 bits on windows
# CXX=x86_64-w64-mingw32-g++
# CC=x86_64-w64-mingw32-g++
CXX=g++
CC=gcc
CXXFLAGS=-I. --std=c++14 -O2 # -static

# For linux, others...
# (pthreads can't be linked statically)
# CXXFLAGS=-I. --std=c++14 -O2 -DNDEBUG=1

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

clean :
	rm -f *.o *.exe
