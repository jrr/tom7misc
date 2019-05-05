all : decode-monika.exe


CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

# -Wno-write-strings
CPPFLAGS=-m64 -Wall -Wno-sign-compare -O2 --std=c++14 -I ../cc-lib/
LFLAGS=-static

%.o : %.cc makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

pass.s : pass.cc
	$(CXX) -g -m64 --std=c++14 -I ../cc-lib/ -O2 -Wa,-aslh $^ -c > $@

pass.exe : pass.o ../cc-lib/base64.o ../cc-lib/util.o ../cc-lib/crypt/cryptrand.o ../cc-lib/crypt/aes.o ../cc-lib/crypt/sha256.o ../cc-lib/arcfour.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o
	$(CXX) $^ -o $@ $(LFLAGS)

decode-monika.exe : decode-monika.o ../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o
	$(CXX) $^ -o $@ $(LFLAGS)

memtest.exe : memtest.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o ../cc-lib/arcfour.o
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.exe *.o core *.s
