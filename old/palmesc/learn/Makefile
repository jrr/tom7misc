OBJS = learn.oo

CC = m68k-p~5

PILRC = pilrc
OBJRES = m68k-~10
BUILDPRC = build-prc

ICONTEXT = "Learn Me"
APPID = TomL
PRC = learn.prc

CFLAGS = -O2 -Wall

all: $(PRC)

.S.o:
	$(CC) $(TARGETFLAGS) -c $<

.c.s:
	$(CC) -S -O2 $<

$(PRC): code.stamp bin.stamp
	$(BUILDPRC) $@ $(ICONTEXT) $(APPID) *.grc *.bin

code.stamp: learn
	$(OBJRES) learn
	touch code.stamp

bin.stamp: learn.rcp
	$(PILRC) learn.rcp
	touch bin.stamp

pilrctst: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

clean:
	rm -rf *.[oa] learn *.bin *.stamp *.grc learn.prc



