
default : opt_test.exe

BASE=../base/logging.o ../base/stringprintf.o

# for 64 bits on windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-g++
CXXFLAGS=-march=native -I.. --std=c++17 -O2 -static

# For linux, others...
# CXX=g++
# CC=gcc
# CXXFLAGS=-I. --std=c++17 -O2 -DNDEBUG=1

opt.o : opt.cc opt.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

opt_test.o : opt_test.cc opt.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

opt_test.exe : opt.o opt_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

clean :
	rm -f *.o *.exe
