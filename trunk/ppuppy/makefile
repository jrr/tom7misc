all : ppuppy.exe

# CXX=g++
CXX=x86_64-w64-mingw32-g++
# consider O2 for releases...
CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O2 --std=c++14 -I ../cc-lib
# XXX for windows
LFLAGS=-static

%.o : %.cc *.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
#	bash -c "echo -n '.'"

%.s : %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -S -c -o $@ $<

# ../cc-lib/util.o  ../cc-lib/base/stringprintf.o ../cc-lib/arcfour.o
CCLIB_OBJECTS= ../cc-lib/base/logging.o ../cc-lib/stb_image.o
ALL_CCLIB_OBJECTS=$(CCLIB_OBJECTS) ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/util.o ../cc-lib/arcfour.o

ppuppy.exe : ppuppy.o demos.o screen.o bcm2835.o convert.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

gpio.exe : gpio.o
	$(CXX) $^ -o $@ $(LFLAGS)

clear.exe : clear.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

set.exe : set.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

read.exe : read.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

deconvert.exe : deconvert.o screen.o convert.o $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

# XXX integrate armsnes/Makefile
# basically just need to compile with g++ -I. -Os -DHAVE_STRINGS_H -DHAVE_STDINT_H -DHAVE_INTTYPES_H -DUSE_SA1 -c -o
ARMSNES_OBJECTS=armsnes/src/apu.o armsnes/src/apuaux.o armsnes/src/c4.o armsnes/src/c4emu.o armsnes/src/cheats.o armsnes/src/cheats2.o armsnes/src/clip.o armsnes/src/data.o armsnes/src/dsp1.o armsnes/src/fxemu.o armsnes/src/fxinst.o armsnes/src/globals.o armsnes/src/ppu.o armsnes/src/dma.o armsnes/src/memmap.o armsnes/src/cpu.o armsnes/src/cpuexec.o armsnes/src/cpuops.o armsnes/src/sa1.o armsnes/src/sa1cpu.o armsnes/src/sdd1.o armsnes/src/sdd1emu.o armsnes/src/snapshot.o armsnes/src/soundux.o armsnes/src/spc700.o armsnes/src/spc700a.o armsnes/src/srtc.o armsnes/src/spc_decode.o armsnes/src/tile16.o armsnes/src/tile16add.o armsnes/src/tile16add1_2.o armsnes/src/tile16fadd1_2.o armsnes/src/tile16sub.o armsnes/src/tile16sub1_2.o armsnes/src/tile16fsub1_2.o armsnes/src/mode7new.o armsnes/src/mode7.o armsnes/src/mode7add.o armsnes/src/mode7add1_2.o armsnes/src/mode7sub.o armsnes/src/mode7sub1_2.o armsnes/src/mode7prio.o armsnes/src/mode7addprio.o armsnes/src/mode7add1_2prio.o armsnes/src/mode7subprio.o armsnes/src/mode7sub1_2prio.o armsnes/src/gfx16.o armsnes/src/rops.o armsnes/libretro/libretro.o armsnes/libretro/memstream.o

snestest.exe : snestest.o $(ARMSNES_OBJECTS) $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS) -lm

clean :
	rm -f *.o *.exe core $(ALL_CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
