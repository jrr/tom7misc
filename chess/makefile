
default: chess.exe

CXXFLAGS=--std=c++17 -Wall -Wno-deprecated -Wno-sign-compare
OPT=-O2
SYMBOLS=-g

# Tried switching back to mingw/w64...
# CXX=g++
# CC=gcc
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

CPPFLAGS=-m64 -D__MINGW32__ -DHAVE_ALLOCA -D_GLIBCXX_HAS_GTHREADS -mthreads

# without static, can't find lz or lstdcxx maybe?
LFLAGS=-m64 -mthreads -lwinpthread -L. -static

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

chess.exe : chess.o
	$(CXX) $^ -o $@ $(LFLAGS)
