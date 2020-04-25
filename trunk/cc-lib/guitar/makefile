default : gencc.exe name.exe finger.exe
all : gencc.exe name.exe guitar_test.exe

CCLIB=../base/logging.o ../base/stringprintf.o ../util.o

# Windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-g++
CXXFLAGS=-Wall -I.. -DWIN32 --std=c++17 -O2
LFLAGS=-static

# Linux
# CXX=g++-8
# CC=gcc-8
# CXXFLAGS=-Wall -I.. --std=c++17 -O2
# LFLAGS=

%.o : %.cc *.h
	@$(CXX) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

gencc.exe : gencc.o $(CCLIB)
	$(CXX) $^ -o $@ $(LFLAGS)

guitar.cc : gencc.exe guitar.json guitar-head.cc guitar-tail.cc
	./gencc.exe guitar.json

guitar_test.exe : guitar.o guitar_test.o $(CCLIB)
	$(CXX) $^ -o $@ $(LFLAGS)

name.exe : name.o guitar.o $(CCLIB)
	$(CXX) $^ -o $@ $(LFLAGS)

finger.exe : finger.o guitar.o $(CCLIB)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe
