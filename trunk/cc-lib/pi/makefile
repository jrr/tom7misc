
default : netutil_test.exe $(TESTCOMPILE)

TESTCOMPILE=bcm2835.o

BASE=../base/logging.o ../base/stringprintf.o

# no-psabi suppresses warnings all over STL in GCC 7.1 for ARM (e.g. pi)
# it can probably be removed in a few years?
CXX=g++
CC=gcc
CXXFLAGS=-Wall -I. --std=c++17 -O2 -Wno-psabi

netutil_test.exe : netutil_test.o netutil.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

clean :
	rm -f *.o *.exe
