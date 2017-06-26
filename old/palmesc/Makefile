OBJS = gfx.oo

CC = m68k-p~5

PILRC = pilrc
OBJRES = m68k-~10
BUILDPRC = build-prc

ICONTEXT = "Escape"
APPID = GrFX
PRC = gfx.prc

CFLAGS = -O2 -Wall

all: $(PRC)

.S.o:
	$(CC) $(TARGETFLAGS) -c $<

.c.s:
	$(CC) -S -O2 $<

$(PRC): code.stamp bin.stamp
	$(BUILDPRC) $@ $(ICONTEXT) $(APPID) *.grc *.bin

code.stamp: gfx
	$(OBJRES) gfx
	touch code.stamp

bin.stamp: gfx.rcp
	$(PILRC) gfx.rcp
	touch bin.stamp

pilrctst: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

clean:
	rm -rf *.[oa] gfx *.bin *.stamp *.grc gfx.prc



