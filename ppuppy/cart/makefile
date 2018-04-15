default: lesson1.nes

CC65 = ../../../cc65/bin/cc65
CA65 = ../../../cc65/bin/ca65
LD65 = ../../../cc65/bin/ld65

lesson1.nes: lesson1.o reset.o nes.cfg
	$(LD65) -C nes.cfg -o lesson1.nes reset.o lesson1.o nes.lib
	@echo lesson1.nes created

reset.o: reset.s
	$(CA65) reset.s

lesson1.o: lesson1.s
	$(CA65) lesson1.s

lesson1.s: lesson1.c
	$(CC65) -Oi lesson1.c --add-source

clean:
	rm -f lesson1.nes *.o lesson1.s
