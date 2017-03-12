all : abc.exe benchbytes.exe makefreq.exe twocolumn.exe text2pdf.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

ABC_SML = abc.sml cil.sml toasm-sig.sml acc-sig.sml encode-x86-sig.sml optimize-asm-sig.sml toasm.sml acc.sml encode-x86.sml optimize-asm.sml tocil-sig.sml allocate-tmps-sig.sml exe-sig.sml optimize-cil-sig.sml tocil.sml allocate-tmps.sml exe.sml optimize-cil.sml tox86-sig.sml asm.sml flags.sml segment-sig.sml tox86.sml liveness-sig.sml segment.sml cil-pass-sig.sml liveness.sml solveseg.sml cil-pass.sml machine-sig.sml tactics-sig.sml x86.sml cil-util-sig.sml machine.sml tactics-test.sml cil-util.sml main.sml tactics.sml

abc.exe : $(ABC_SML) abc.mlb
	$(MLTON) -output $@ abc.mlb

maketest.exe : maketest.sml maketest.mlb
	$(MLTON) -output $@ maketest.mlb

makefreq.exe : makefreq.sml makefreq.mlb
	$(MLTON) -output $@ makefreq.mlb

twocolumn.exe : twocolumn.sml twocolumn.mlb
	$(MLTON) -output $@ twocolumn.mlb

benchbytes.exe : benchbytes.sml benchbytes.mlb
	$(MLTON) -output $@ benchbytes.mlb

TESTS = builtin.exe simpcall.exe globals.exe globals2.exe callargs.exe dead.exe cond.exe cond2.exe simpleop.exe for.exe write.exe strlit.exe # forswitch.exe

text2pdf.exe : text2pdf.c
	gcc -O2 text2pdf.c -o text2pdf.exe

paper/firstpage.2c : twocolumn.exe paper/firstpage.txt makefile
	./twocolumn.exe -startline 0 -startcol 40 -pages 1 -endlines 76 -endcol 64 paper/firstpage.txt -o $@

paper/postreloc.2c : twocolumn.exe paper/postreloc.txt makefile
	# XXX endlines is estimate
	./twocolumn.exe -debug -startline 1 -startcol 0 -pages 5 -endlines 73 -endcol 64 paper/postreloc.txt -o $@
	./text2pdf -c160 -s6 -v6 -l128 debug0.page > debug0.pdf

paper/paper.pdf : dos/paper.exe text2pdf.exe makefile
	./text2pdf -c160 -s6 -v6 -l128 $< > paper/paper.pdf

tests : $(addprefix dos/,$(TESTS)) dos/tests.bat

testclean :
	rm -f $(addprefix dos/,$(TESTS))
	rm -f dos/*.asm dos/*.esm dos/*.cil dos/*.bytes

dos/tests.bat : maketest.exe makefile
	./maketest.exe $(TESTS) > dos/tests.bat

TEXT2C=paper/firstpage.2c paper/postreloc.2c
dos/%.exe : %.c abc.exe $(TEXT2C)
	./abc.exe -v $< -o $@
