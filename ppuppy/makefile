all : ppuppy.exe

talk : deconstruct.ctalk
jstalk : jstalk/slides.js

CXX=g++
# CXX=x86_64-w64-mingw32-g++
# consider O2 for releases...
CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O2 --std=c++14 -I ../cc-lib  -DPSS_STYLE=1
# XXX for windows
LFLAGS=-static -lpthread

ppuppy-demo.o : ppuppy.cc *.h
	$(CXX) -DDEMO=1 $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

%.o : %.cc *.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
#	bash -c "echo -n '.'"

%.s : %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -S -c -o $@ $<

CCLIB_OBJECTS= ../cc-lib/base/logging.o ../cc-lib/stb_image.o  ../cc-lib/arcfour.o ../cc-lib/util.o ../cc-lib/color-util.o ../cc-lib/base/stringprintf.o
ALL_CCLIB_OBJECTS=$(CCLIB_OBJECTS) ../cc-lib/stb_image_write.o ../cc-lib/image.o 

# XXX integrate armsnes/Makefile
# basically just need to compile with g++ -I. -Os -DHAVE_STRINGS_H -DHAVE_STDINT_H -DHAVE_INTTYPES_H -DUSE_SA1 -c -o
ARMSNES_OBJECTS=armsnes/src/apu.o armsnes/src/apuaux.o armsnes/src/c4.o armsnes/src/c4emu.o armsnes/src/clip.o armsnes/src/data.o armsnes/src/dsp1.o armsnes/src/fxemu.o armsnes/src/fxinst.o armsnes/src/globals.o armsnes/src/ppu.o armsnes/src/dma.o armsnes/src/memmap.o armsnes/src/cpu.o armsnes/src/cpuexec.o armsnes/src/cpuops.o armsnes/src/sa1.o armsnes/src/sa1cpu.o armsnes/src/sdd1.o armsnes/src/sdd1emu.o armsnes/src/snapshot.o armsnes/src/soundux.o armsnes/src/spc700.o armsnes/src/spc700a.o armsnes/src/srtc.o armsnes/src/spc_decode.o armsnes/src/tile16.o armsnes/src/tile16add.o armsnes/src/tile16add1_2.o armsnes/src/tile16fadd1_2.o armsnes/src/tile16sub.o armsnes/src/tile16sub1_2.o armsnes/src/tile16fsub1_2.o armsnes/src/mode7new.o armsnes/src/mode7.o armsnes/src/mode7add.o armsnes/src/mode7add1_2.o armsnes/src/mode7sub.o armsnes/src/mode7sub1_2.o armsnes/src/mode7prio.o armsnes/src/mode7addprio.o armsnes/src/mode7add1_2prio.o armsnes/src/mode7subprio.o armsnes/src/mode7sub1_2prio.o armsnes/src/gfx16.o armsnes/src/rops.o armsnes/libretro/libretro.o armsnes/libretro/memstream.o

FCEULIB=../fceulib
FCEULIB_OBJECTS=$(FCEULIB)/mappers/6.o $(FCEULIB)/mappers/61.o $(FCEULIB)/mappers/24and26.o $(FCEULIB)/mappers/51.o $(FCEULIB)/mappers/69.o $(FCEULIB)/mappers/77.o $(FCEULIB)/mappers/40.o $(FCEULIB)/mappers/mmc2and4.o $(FCEULIB)/mappers/71.o $(FCEULIB)/mappers/79.o $(FCEULIB)/mappers/41.o $(FCEULIB)/mappers/72.o $(FCEULIB)/mappers/80.o $(FCEULIB)/mappers/42.o $(FCEULIB)/mappers/62.o $(FCEULIB)/mappers/73.o $(FCEULIB)/mappers/85.o $(FCEULIB)/mappers/emu2413.o $(FCEULIB)/mappers/46.o $(FCEULIB)/mappers/65.o $(FCEULIB)/mappers/75.o $(FCEULIB)/mappers/50.o $(FCEULIB)/mappers/67.o $(FCEULIB)/mappers/76.o $(FCEULIB)/mappers/tengen.o $(FCEULIB)/utils/memory.o $(FCEULIB)/utils/crc32.o $(FCEULIB)/utils/endian.o $(FCEULIB)/utils/md5.o $(FCEULIB)/utils/xstring.o $(FCEULIB)/boards/mmc1.o $(FCEULIB)/boards/mmc5.o $(FCEULIB)/boards/datalatch.o $(FCEULIB)/boards/mmc3.o $(FCEULIB)/boards/01-222.o $(FCEULIB)/boards/32.o $(FCEULIB)/boards/gs-2013.o $(FCEULIB)/boards/103.o $(FCEULIB)/boards/33.o $(FCEULIB)/boards/h2288.o $(FCEULIB)/boards/106.o $(FCEULIB)/boards/34.o $(FCEULIB)/boards/karaoke.o $(FCEULIB)/boards/108.o $(FCEULIB)/boards/3d-block.o $(FCEULIB)/boards/kof97.o $(FCEULIB)/boards/112.o $(FCEULIB)/boards/411120-c.o $(FCEULIB)/boards/konami-qtai.o $(FCEULIB)/boards/116.o $(FCEULIB)/boards/43.o $(FCEULIB)/boards/ks7012.o $(FCEULIB)/boards/117.o $(FCEULIB)/boards/57.o $(FCEULIB)/boards/ks7013.o $(FCEULIB)/boards/120.o $(FCEULIB)/boards/603-5052.o $(FCEULIB)/boards/ks7017.o $(FCEULIB)/boards/121.o $(FCEULIB)/boards/68.o $(FCEULIB)/boards/ks7030.o $(FCEULIB)/boards/12in1.o $(FCEULIB)/boards/8157.o $(FCEULIB)/boards/ks7031.o $(FCEULIB)/boards/15.o $(FCEULIB)/boards/82.o $(FCEULIB)/boards/ks7032.o $(FCEULIB)/boards/151.o $(FCEULIB)/boards/8237.o $(FCEULIB)/boards/ks7037.o $(FCEULIB)/boards/156.o $(FCEULIB)/boards/830118c.o $(FCEULIB)/boards/ks7057.o $(FCEULIB)/boards/164.o $(FCEULIB)/boards/88.o $(FCEULIB)/boards/le05.o $(FCEULIB)/boards/168.o $(FCEULIB)/boards/90.o $(FCEULIB)/boards/lh32.o $(FCEULIB)/boards/17.o $(FCEULIB)/boards/91.o $(FCEULIB)/boards/lh53.o $(FCEULIB)/boards/170.o $(FCEULIB)/boards/95.o $(FCEULIB)/boards/malee.o $(FCEULIB)/boards/175.o $(FCEULIB)/boards/96.o $(FCEULIB)/boards/176.o $(FCEULIB)/boards/99.o $(FCEULIB)/boards/177.o $(FCEULIB)/boards/178.o $(FCEULIB)/boards/a9746.o $(FCEULIB)/boards/18.o $(FCEULIB)/boards/ac-08.o $(FCEULIB)/boards/n625092.o $(FCEULIB)/boards/183.o $(FCEULIB)/boards/addrlatch.o $(FCEULIB)/boards/novel.o $(FCEULIB)/boards/185.o $(FCEULIB)/boards/ax5705.o $(FCEULIB)/boards/onebus.o $(FCEULIB)/boards/186.o $(FCEULIB)/boards/pec-586.o $(FCEULIB)/boards/187.o $(FCEULIB)/boards/bb.o $(FCEULIB)/boards/sa-9602b.o $(FCEULIB)/boards/189.o $(FCEULIB)/boards/bmc13in1jy110.o $(FCEULIB)/boards/193.o $(FCEULIB)/boards/bmc42in1r.o $(FCEULIB)/boards/sc-127.o $(FCEULIB)/boards/199.o $(FCEULIB)/boards/bmc64in1nr.o $(FCEULIB)/boards/sheroes.o $(FCEULIB)/boards/208.o $(FCEULIB)/boards/bmc70in1.o $(FCEULIB)/boards/sl1632.o $(FCEULIB)/boards/222.o $(FCEULIB)/boards/bonza.o $(FCEULIB)/boards/smb2j.o $(FCEULIB)/boards/225.o $(FCEULIB)/boards/bs-5.o $(FCEULIB)/boards/228.o $(FCEULIB)/boards/cityfighter.o $(FCEULIB)/boards/super24.o $(FCEULIB)/boards/230.o $(FCEULIB)/boards/dance2000.o $(FCEULIB)/boards/n106.o $(FCEULIB)/boards/supervision.o $(FCEULIB)/boards/232.o $(FCEULIB)/boards/t-227-1.o $(FCEULIB)/boards/234.o $(FCEULIB)/boards/deirom.o $(FCEULIB)/boards/t-262.o $(FCEULIB)/boards/sachen.o $(FCEULIB)/boards/235.o $(FCEULIB)/boards/dream.o $(FCEULIB)/boards/244.o $(FCEULIB)/boards/edu2000.o $(FCEULIB)/boards/tf-1201.o $(FCEULIB)/boards/bandai.o $(FCEULIB)/boards/246.o $(FCEULIB)/boards/famicombox.o $(FCEULIB)/boards/transformer.o $(FCEULIB)/boards/252.o $(FCEULIB)/boards/fk23c.o $(FCEULIB)/boards/vrc2and4.o $(FCEULIB)/boards/253.o $(FCEULIB)/boards/ghostbusters63in1.o $(FCEULIB)/boards/vrc7.o $(FCEULIB)/boards/28.o $(FCEULIB)/boards/gs-2004.o $(FCEULIB)/boards/yoko.o $(FCEULIB)/input/arkanoid.o $(FCEULIB)/input/ftrainer.o $(FCEULIB)/input/oekakids.o $(FCEULIB)/input/suborkb.o $(FCEULIB)/input/bworld.o $(FCEULIB)/input/hypershot.o $(FCEULIB)/input/powerpad.o $(FCEULIB)/input/toprider.o $(FCEULIB)/input/cursor.o $(FCEULIB)/input/mahjong.o $(FCEULIB)/input/quiz.o $(FCEULIB)/input/zapper.o $(FCEULIB)/input/fkb.o $(FCEULIB)/input/shadow.o $(FCEULIB)/cart.o $(FCEULIB)/version.o $(FCEULIB)/emufile.o $(FCEULIB)/fceu.o $(FCEULIB)/fds.o $(FCEULIB)/file.o $(FCEULIB)/filter.o $(FCEULIB)/ines.o $(FCEULIB)/input.o $(FCEULIB)/palette.o $(FCEULIB)/ppu.o $(FCEULIB)/sound.o $(FCEULIB)/state.o $(FCEULIB)/unif.o $(FCEULIB)/vsuni.o $(FCEULIB)/x6502.o $(FCEULIB)/git.o $(FCEULIB)/fc.o $(FCEULIB)/emulator.o $(FCEULIB)/headless-driver.o $(FCEULIB)/simplefm2.o $(FCEULIB)/stringprintf.o

ppuppy.exe : ppuppy.o schedule.o demos.o screen.o talk.o bcm2835.o convert.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

demo.exe : ppuppy-demo.o schedule.o demos.o snesdemo.o screen.o talk.o bcm2835.o convert.o convert565.o $(CCLIB_OBJECTS) $(ARMSNES_OBJECTS) $(FCEULIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS) -lz

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

getbit.exe : getbit.o bcm2835.o $(CCLIB_OBJECTS)
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


snestest.exe : snestest.o convert565.o screen.o $(ARMSNES_OBJECTS) $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS) -lm

clean :
	rm -f *.o *.exe core $(ALL_CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
