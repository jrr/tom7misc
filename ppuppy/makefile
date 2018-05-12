all : ppuppy.exe

talk : deconstruct.ctalk
jstalk : jstalk/slides.js

# CXX=g++
CXX=x86_64-w64-mingw32-g++
# consider O2 for releases...
CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O2 --std=c++14 -I ../cc-lib
# XXX for windows
LFLAGS=-static -lpthread

%.o : %.cc *.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
#	bash -c "echo -n '.'"

%.s : %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -S -c -o $@ $<

CCLIB_OBJECTS= ../cc-lib/base/logging.o ../cc-lib/stb_image.o  ../cc-lib/arcfour.o ../cc-lib/util.o ../cc-lib/color-util.o
ALL_CCLIB_OBJECTS=$(CCLIB_OBJECTS) ../cc-lib/stb_image_write.o ../cc-lib/image.o ../cc-lib/base/stringprintf.o

# XXX integrate armsnes/Makefile
# basically just need to compile with g++ -I. -Os -DHAVE_STRINGS_H -DHAVE_STDINT_H -DHAVE_INTTYPES_H -DUSE_SA1 -c -o
ARMSNES_OBJECTS=armsnes/src/apu.o armsnes/src/apuaux.o armsnes/src/c4.o armsnes/src/c4emu.o armsnes/src/clip.o armsnes/src/data.o armsnes/src/dsp1.o armsnes/src/fxemu.o armsnes/src/fxinst.o armsnes/src/globals.o armsnes/src/ppu.o armsnes/src/dma.o armsnes/src/memmap.o armsnes/src/cpu.o armsnes/src/cpuexec.o armsnes/src/cpuops.o armsnes/src/sa1.o armsnes/src/sa1cpu.o armsnes/src/sdd1.o armsnes/src/sdd1emu.o armsnes/src/snapshot.o armsnes/src/soundux.o armsnes/src/spc700.o armsnes/src/spc700a.o armsnes/src/srtc.o armsnes/src/spc_decode.o armsnes/src/tile16.o armsnes/src/tile16add.o armsnes/src/tile16add1_2.o armsnes/src/tile16fadd1_2.o armsnes/src/tile16sub.o armsnes/src/tile16sub1_2.o armsnes/src/tile16fsub1_2.o armsnes/src/mode7new.o armsnes/src/mode7.o armsnes/src/mode7add.o armsnes/src/mode7add1_2.o armsnes/src/mode7sub.o armsnes/src/mode7sub1_2.o armsnes/src/mode7prio.o armsnes/src/mode7addprio.o armsnes/src/mode7add1_2prio.o armsnes/src/mode7subprio.o armsnes/src/mode7sub1_2prio.o armsnes/src/gfx16.o armsnes/src/rops.o armsnes/libretro/libretro.o armsnes/libretro/memstream.o

ppuppy.exe : ppuppy.o schedule.o demos.o screen.o talk.o bcm2835.o convert.o $(CCLIB_OBJECTS) $(ARMSNES_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

gpio.exe : gpio.o
	$(CXX) $^ -o $@ $(LFLAGS)

clear.exe : clear.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

set.exe : set.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

out.exe : out.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

read.exe : read.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

deconvert.exe : deconvert.o deconversion.o screen.o convert.o $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

detalk.exe : detalk.o deconversion.o talk.o screen.o convert.o $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

jstalk/slides.js : detalk.exe deconstruct.ctalk
	./detalk.exe deconstruct.ctalk deconstruct.screens jstalk

maketalk.exe : maketalk.o talk.o screen.o convert.o $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

deconstruct.ctalk : images/*.png maketalk.exe deconstruct.talk
	./maketalk.exe deconstruct.talk deconstruct.ctalk deconstruct.screens

deconverted/%.png : images/%.png deconvert.exe
	./deconvert.exe $< $@

deconverted/%.png : images/%.jpg deconvert.exe
	./deconvert.exe $< $@

deconverted: deconverted/ppuppy-logo.png deconverted/self.png deconverted/motherboard.png deconverted/motherboard-cpu.png deconverted/motherboard-ppu.png deconverted/nes2cart.png deconverted/motherboard-cic-troll.png deconverted/motherboard-cic.png deconverted/arst-arsw.png deconverted/motherboard-caps.png deconverted/abc.png deconverted/venn1.png deconverted/venn2.png deconverted/venn3.png deconverted/venn4.png deconverted/samus.png deconverted/marioboot.png deconverted/cartclosed.png deconverted/cartinside.png deconverted/cartinside-labeled.png deconverted/cartinside-bicameral.png deconverted/emulation-intro.png deconverted/emulation.png deconverted/emulation-reverse.png deconverted/zero-w.png deconverted/motherbrain.png deconverted/trick-capacitor.png deconverted/goldfingers-mismatch.png deconverted/onetrickpony.png deconverted/titletest.png deconverted/real-schematic.png deconverted/ppuppy-board.png deconverted/underbelly.png deconverted/fcc.png deconverted/fantasy.png deconverted/fantasy2.png deconverted/learnfun1.png deconverted/learnfun2.png deconverted/learnfun3.png deconverted/dtd4.png deconverted/boot-disgusted.png deconverted/boot-neutral.png deconverted/quine.png deconverted/tables.png deconverted/tables-color.png


snestest.exe : snestest.o $(ARMSNES_OBJECTS) $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS) -lm

clean :
	rm -f *.o *.exe core $(ALL_CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
