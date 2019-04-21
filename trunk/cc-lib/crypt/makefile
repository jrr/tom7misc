
default : aes_test.exe

BASE=../base/logging.o ../base/stringprintf.o

# for 64 bits on windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-g++
CXXFLAGS=-I. -I.. --std=c++14 -O2 -static

# For linux, others...
# (pthreads can't be linked statically)
# CXX=g++
# CC=gcc
# CXXFLAGS=-I. -I.. --std=c++14 -O2 -DNDEBUG=1

aes.o : aes.cc aes.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

aes_test.o : aes_test.cc aes.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

aes_test.exe : aes.o aes_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@


clean :
	rm -f *.o *.exe
