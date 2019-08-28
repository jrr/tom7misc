
default : makegrid.exe resize.exe testpattern.exe

CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

CXXFLAGS=--std=c++17 -O2 -Wall -I../cc-lib/
LFLAGS=-static

CCLIB_OBJECTS=../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o ../cc-lib/util.o

%.o : %.cc
	@$(CXX) $(CXXFLAGS) -c $^ -o $@
	@echo -n "."

makegrid.exe : makegrid.o $(CCLIB_OBJECTS)
	$(CXX) $(LFLAGS) $^ -o $@

resize.exe : resize.o $(CCLIB_OBJECTS)
	$(CXX) $(LFLAGS) $^ -o $@

testpattern.exe : testpattern.o $(CCLIB_OBJECTS)
	$(CXX) $(LFLAGS) $^ -o $@

clean :
	rm -f *.o *.exe
