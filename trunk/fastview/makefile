
default: fastview.exe

CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc
WINDRES=x86_64-w64-mingw32-windres

CCLIB=../cc-lib

DEFINES=-DWIN32=1 -D_WIN32_WINNT=0x0601
INCLUDES=-I $(CCLIB)

MINGWLIB=/usr/x86_64-w64-mingw32/sys-root/mingw/lib
WINDOWS_OBJECTS=$(MINGWLIB)/libwinpthread.a $(MINGWLIB)/libd2d1.a

#  -flto
CXXFLAGS=$(DEFINES) $(INCLUDES) -march=native -Wall --std=c++17 -O3
LFLAGS=$(CXXFLAGS) -ffunction-sections -mwindows -static -static-libgcc -static-libstdc++ -s

# Don't use -flto to see assembly!
fastview.s : fastview.cc makefile
	$(CXX) $(DEFINES) $(INCLUDES) -march=native -Wall --std=c++17 -O3 -g -Wa,-aslh $^ -c > $@

../cc-lib/%.o : ../cc-lib/%.cc
	@$(CXX) $(CXXFLAGS) -c $< -o $@
	@bash -c "echo -n '_'"

%.o : %.cc makefile
	@$(CXX) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

OBJECTS=fastview.o
CCLIB_OBJECTS=$(CCLIB)/image.o $(CCLIB)/stb_image.o $(CCLIB)/stb_image_write.o $(CCLIB)/base/logging.o $(CCLIB)/base/stringprintf.o

fastview.exe: $(OBJECTS) $(CCLIB_OBJECTS)
	$(CXX) $(LFLAGS) $^ $(WINDOWS_OBJECTS) -o $@


clean:
	rm -f $(CCLIB_OBJECTS) $(OBJECTS) fastview.exe
