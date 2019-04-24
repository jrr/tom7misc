all : decode-monika.exe


CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

CPPFLAGS=-Wno-write-strings -m64 -O2 --std=c++14 -I ../cc-lib/
LFLAGS=-static

%.o : %.cc makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

pass.exe : pass.o ../cc-lib/crypt/sha256.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o
	$(CXX) $^ -o $@ $(LFLAGS)

decode-monika.exe : decode-monika.o ../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o
	$(CXX) $^ -o $@ $(LFLAGS)

memtest.exe : memtest.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o ../cc-lib/arcfour.o
	$(CXX) $^ -o $@ $(LFLAGS)
