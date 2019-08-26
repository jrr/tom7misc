
default : makegrid.exe resize.exe testpattern.exe

CXXFLAGS=--std=c++17 -O2 -Wall -I../cc-lib/

CCLIB_OBJECTS=../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o ../cc-lib/util.o

%.o : %.cc
	@$(CXX) $(CXXFLAGS) -c $^ -o $@
	@echo -n "."

makegrid.exe : makegrid.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@

resize.exe : resize.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@

testpattern.exe : testpattern.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@

clean :
	rm -f *.o *.exe
