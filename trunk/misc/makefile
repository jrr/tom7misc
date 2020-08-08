all : decode-monika.exe pass.exe swipe-ambiguous.exe memtest.exe optional.exe bus.exe cubedex.exe cubedex2.exe


CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

# -Wno-write-strings
CPPFLAGS=-m64 -march=native -Wall -Wno-sign-compare -O3 --std=c++17 -I ../cc-lib/
LFLAGS=-static
CC_LIB=../cc-lib


%.o : %.cc makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

pass.s : pass.cc
	$(CXX) -g -m64 --std=c++17 -I $(CC_LIB)/ -O2 -Wa,-aslh $^ -c > $@

pass.exe : pass.o $(CC_LIB)/base64.o $(CC_LIB)/util.o $(CC_LIB)/crypt/cryptrand.o $(CC_LIB)/crypt/aes.o $(CC_LIB)/crypt/sha256.o $(CC_LIB)/arcfour.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o
	$(CXX) $^ -o $@ $(LFLAGS)

decode-monika.exe : decode-monika.o $(CC_LIB)/stb_image.o $(CC_LIB)/stb_image_write.o $(CC_LIB)/image.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/base/logging.o
	$(CXX) $^ -o $@ $(LFLAGS)

swipe-ambiguous.exe : swipe-ambiguous.o $(CC_LIB)/base/logging.o $(CC_LIB)/util.o $(CC_LIB)/base/stringprintf.o
	$(CXX) $^ -o $@ $(LFLAGS)

memtest.exe : memtest.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/arcfour.o
	$(CXX) $^ -o $@ $(LFLAGS)

path.exe : path.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/arcfour.o
	$(CXX) $^ -o $@ $(LFLAGS)

optional.exe : optional.o
	$(CXX) $^ -o $@ $(LFLAGS)

bus.exe : bus.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/util.o
	$(CXX) $^ -o $@ $(LFLAGS)

cubedex.exe : cubedex.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/util.o
	$(CXX) $^ -o $@ $(LFLAGS)

cubedex2.exe : cubedex2.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/util.o
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.exe *.o core *.s
