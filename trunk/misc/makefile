all : decode-monika.exe


CXX=g++
CPPFLAGS=-Wno-write-strings -m64 -O2 --std=c++14 -I ../cc-lib/
LFLAGS=-static

%.o : %.cc makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."


decode-monika.exe : decode-monika.o ../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o
	$(CXX) $^ -o $@ $(LFLAGS)
