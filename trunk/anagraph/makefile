all : frameserver.exe anagraph.exe

MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

frameserver.exe : frameserver.mlb frameserver.sml
	${MLTON} -output $@ frameserver.mlb

anagraph.exe : anagraph.mlb pixel-data.sml atom*.sml anagraph.sml anagraph-main.sml
	${MLTON} -output $@ anagraph.mlb

tree.js : anagraph.exe
	./anagraph.exe -js

CXX=g++
CPPFLAGS=-Wno-write-strings -m64 -O2 --std=c++14 -I ../cc-lib/
LFLAGS=-static

%.o : %.cc makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

makepixels.exe : makepixels.o ../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o
	$(CXX) $^ -o $@ $(LFLAGS)

pixel-data.sml : makepixels.exe
	./makepixels.exe
