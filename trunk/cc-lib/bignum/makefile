
default : big_test.exe

BASE=../base/logging.o ../base/stringprintf.o

# for 64 bits on windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-g++
# CXX=g++
# CC=gcc
CXXFLAGS=-I. -I.. --std=c++14 -O2 # -static

%.o : %.cc *.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

big_test.exe : bigz.o bign.o bigq.o big_test.o
	$(CXX) $(CXXFLAGS) $^ -o $@ -static

clean :
	rm -f *.o *.exe
