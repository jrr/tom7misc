default: cart.nes

CC65 = ../../../cc65/bin/cc65
CA65 = ../../../cc65/bin/ca65
LD65 = ../../../cc65/bin/ld65

cart.nes: cart.o reset.o nes.cfg
	$(LD65) -C nes.cfg -o cart.nes reset.o cart.o nes.lib
	@echo cart.nes created

reset.o: reset.s
	$(CA65) reset.s

cart.o: cart.s
	$(CA65) cart.s

cart.s: cart.c
	$(CC65) -Oi cart.c --add-source

makeimage.exe : makeimage.c
	$(CXX) makeimage.c -O -o $@

cart.rom: cart.nes makeimage.exe
	./makeimage.exe cart.nes $@

clean:
	rm -f cart.nes *.o cart.s
