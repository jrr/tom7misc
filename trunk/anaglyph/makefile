all : frameserver.exe anaglyph.exe

MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

frameserver.exe : frameserver.mlb frameserver.sml
	${MLTON} -output $@ frameserver.mlb

anaglyph.exe : anaglyph.mlb pixel-data.sml atom*.sml anaglyph.sml anaglyph-main.sml
	${MLTON} -output $@ anaglyph.mlb

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
