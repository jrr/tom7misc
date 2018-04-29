default: cart.nes

CC65 = ../../../cc65/bin/cc65
CA65 = ../../../cc65/bin/ca65
LD65 = ../../../cc65/bin/ld65

cart.nes: cart.o reset.o input.o dma.o nes.cfg
	$(LD65) -C nes.cfg -o cart.nes reset.o input.o cart.o dma.o nes.lib
	@echo cart.nes created

reset.o: reset.s *.chr
	$(CA65) reset.s

input.o: input.s
	$(CA65) input.s

dma.o: dma.s
	$(CA65) dma.s

cart.o: cart.s
	$(CA65) cart.s

cart.s: cart.c
	$(CC65) -Oi cart.c --add-source

makeimage.exe : makeimage.cc
	$(CXX) --std=c++14 makeimage.cc -O2 -o $@

cart.rom: cart.nes makeimage.exe
	./makeimage.exe -mirror -prg cart.nes $@

clean:
	rm -f cart.nes *.o cart.s
