all : abc.exe benchbytes.exe makefreq.exe twocolumn.exe text2pdf.exe histo.exe historec.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

ABC_SML = abc.sml cil.sml toasm-sig.sml acc-sig.sml encode-x86-sig.sml optimize-asm-sig.sml toasm.sml acc.sml encode-x86.sml optimize-asm.sml tocil-sig.sml allocate-tmps-sig.sml exe-sig.sml optimize-cil-sig.sml tocil.sml allocate-tmps.sml exe.sml optimize-cil.sml tox86-sig.sml asm.sml flags.sml segment-sig.sml tox86.sml liveness-sig.sml segment.sml cil-pass-sig.sml liveness.sml cil-pass.sml machine-sig.sml tactics-sig.sml x86.sml cil-util-sig.sml machine.sml tactics-test.sml cil-util.sml main.sml tactics.sml

abc.exe : $(ABC_SML) abc.mlb
	$(MLTON) -output $@ abc.mlb

maketest.exe : maketest.sml maketest.mlb
	$(MLTON) -output $@ maketest.mlb

truncpage.exe : truncpage.sml truncpage.mlb
	$(MLTON) -output $@ truncpage.mlb

makefreq.exe : makefreq.sml makefreq.mlb
	$(MLTON) -output $@ makefreq.mlb

twocolumn.exe : twocolumn.sml twocolumn.mlb
	$(MLTON) -output $@ twocolumn.mlb

histo.exe : histo.sml histo.mlb
	$(MLTON) -output $@ histo.mlb

historec.exe : historec.sml historec.mlb
	$(MLTON) -output $@ historec.mlb

benchbytes.exe : benchbytes.sml benchbytes.mlb
	$(MLTON) -output $@ benchbytes.mlb

TESTS = sizeof.exe builtin.exe simpcall.exe globals.exe globals2.exe callargs.exe dead.exe cond.exe cond2.exe simpleop.exe for.exe write.exe strlit.exe bitop.exe struct.exe fswitch.exe puts.exe

text2pdf.exe : text2pdf.c
	gcc -O2 $< -o $@

png2ascii.exe : png2ascii.cc
	g++ -std=c++11 -O2 $< -o $@

paper/x86.txt : paper/x86.png png2ascii.exe makefile
	./png2ascii "##234%%====---- " $< -newlines > $@

paper/firstpage.2c : twocolumn.exe paper/firstpage.txt makefile
	./twocolumn.exe -startline 0 -startcol 40 -pages 1 -endlines 76 -endcol 64 -botclearance 1 paper/firstpage.txt -o $@

paper/postreloc.2c : twocolumn.exe paper/postreloc.txt makefile
	./twocolumn.exe -topclearance 1 -startline 1 -startcol 0 -pages 5 -endlines 73 -endcol 64 -botclearance 2 paper/postreloc.txt -o $@
#	./text2pdf -c160 -s6 -v6 -l128 debug0.page > debug0.pdf

paper/postdata1.2c : twocolumn.exe paper/postdata1.txt makefile
	./twocolumn.exe -startline 78 -topclearance 1 -startcol 64 -pages 2 paper/postdata1.txt -o $@

paper/postdata2.2c : twocolumn.exe paper/postdata2.txt makefile
	./twocolumn.exe -startline 0 -startcol 0 -pages 1 -endlines 19 -botclearance 2 -endcol 128 paper/postdata2.txt -o $@

paper/postdata.2c : paper/postdata1.2c paper/postdata2.2c paper/sigbovik.txt
	cat paper/postdata1.2c paper/sigbovik.txt paper/postdata2.2c > paper/postdata.2c

paper/end1.2c : twocolumn.exe paper/end.txt paper.c makefile
	./twocolumn.exe -topclearance 3 -startline 12 -pages 3 paper/end.txt -o $@

paper/end.2c : paper/end1.2c paper/histopage.txt makefile
	cat paper/end1.2c paper/histopage.txt > $@

paper/sigbovik.txt : paper/sigbovik.png png2ascii.exe makefile
	./png2ascii.exe "##23456:89ABC-- " $< > $@

paper/paper.pdf : dos/paper.exe text2pdf.exe makefile
	./text2pdf -c160 -s6 -v6 -l128 $< > paper/paper.pdf

tests : $(addprefix dos/,$(TESTS)) dos/tests.bat

testclean :
	rm -f $(addprefix dos/,$(TESTS))
	rm -f dos/*.asm dos/*.esm dos/*.cil dos/*.bytes

dos/tests.bat : maketest.exe makefile
	./maketest.exe $(TESTS) > dos/tests.bat

TEXT2C=paper/firstpage.2c paper/postreloc.2c paper/postdata.2c paper/end.2c paper/bytetable.txt paper/bytetable0.txt
dos/%.exe : %.c abc.exe $(TEXT2C)
	./abc.exe -v $< -o $@

dos/paper.exe : paper.c abc.exe historec.exe $(TEXT2C)
	./abc.exe $< -o $@
	echo "XXX skipping convergence"
#	./historec.exe dos/paper.exe
