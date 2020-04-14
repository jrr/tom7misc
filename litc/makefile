default : test.exe

CXX=x86_64-w64-mingw32-g++

BASE=../cc-lib/util.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o
CXXFLAGS=-I../cc-lib --std=c++17 -O2

%.o : %.cc
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

test.exe : test.o $(BASE)
	${CXX} -o $@ $^ -static
