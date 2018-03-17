default : test.exe

# TDM-GCC has weird problems with cerr... :/
CXX=x86_64-w64-mingw32-g++

BASE=../cc-lib/util.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o
CXXFLAGS=-I../cc-lib --std=c++17 -O2

test.exe : test.o $(BASE)
	${CXX} -o $@ $^ -static
