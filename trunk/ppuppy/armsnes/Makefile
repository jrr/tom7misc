DEBUG = 0
CC = gcc
TARGET := libpocketsnes.so
fpic := -fPIC
SHARED := -shared -Wl,--version-script=libretro/link.T

ifeq ($(DEBUG), 1)
CFLAGS += -O0 -g
else
CFLAGS += -Os
endif

OBJECTS    = ./src/apu.o ./src/apuaux.o ./src/c4.o ./src/c4emu.o ./src/cheats.o ./src/cheats2.o \
	 			 ./src/clip.o ./src/data.o ./src/dsp1.o ./src/fxemu.o ./src/fxinst.o  \
				 ./src/globals.o ./src/ppu.o ./src/dma.o ./src/memmap.o ./src/cpu.o \
				 ./src/cpuexec.o ./src/cpuops.o ./src/sa1.o ./src/sa1cpu.o ./src/sdd1.o ./src/sdd1emu.o \
				 ./src/snapshot.o ./src/soundux.o ./src/spc700.o ./src/spc700a.o ./src/srtc.o \
				 ./src/spc_decode.o ./src/tile16.o ./src/tile16add.o ./src/tile16add1_2.o \
				 ./src/tile16fadd1_2.o ./src/tile16sub.o ./src/tile16sub1_2.o ./src/tile16fsub1_2.o \
				 ./src/mode7new.o ./src/mode7.o ./src/mode7add.o ./src/mode7add1_2.o ./src/mode7sub.o \
				 ./src/mode7sub1_2.o ./src/mode7prio.o ./src/mode7addprio.o ./src/mode7add1_2prio.o \
				 ./src/mode7subprio.o ./src/mode7sub1_2prio.o ./src/gfx16.o ./src/rops.o \
				 ./libretro/libretro.o ./libretro/memstream.o

INCLUDES   = -I.
DEFINES    = -DHAVE_STRINGS_H -DHAVE_STDINT_H -DHAVE_INTTYPES_H -DUSE_SA1
WARNINGS_DEFINES = -Wall -W -Wno-unused-parameter -Wno-parentheses -Wno-write-strings -Wno-comment
CODE_DEFINES = -fomit-frame-pointer
ALL_DEFINES = $(DEFINES) $(CODE_DEFINES) $(WARNINGS_DEFINES) $(fpic)

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CXX) $(fpic) $(SHARED) $(INCLUDES) -o $@ $(OBJECTS) -lm

%.o: %.c
	$(CC) $(INCLUDES) $(CFLAGS) $(DEFINES) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(INCLUDES) $(CFLAGS) $(DEFINES) -c -o $@ $<

%.o: %.s
	$(CXX) $(INCLUDES) $(CFLAGS) $(DEFINES) -Wa,-I./src/ -c -o $@ $<

clean:
	rm -f $(OBJECTS) $(TARGET)

.PHONY: clean

