all : abc.exe benchbytes.exe twocolumn.exe text2pdf.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

abc.exe : *.sml abc.mlb
	$(MLTON) -output $@ abc.mlb

maketest.exe : maketest.sml maketest.mlb
	$(MLTON) -output $@ maketest.mlb

twocolumn.exe : twocolumn.sml twocolumn.mlb
	$(MLTON) -output $@ twocolumn.mlb

benchbytes.exe : benchbytes.sml benchbytes.mlb
	$(MLTON) -output $@ benchbytes.mlb

TESTS = builtin.exe simpcall.exe globals.exe globals2.exe callargs.exe dead.exe cond.exe cond2.exe simpleop.exe for.exe write.exe strlit.exe # forswitch.exe

text2pdf.exe : text2pdf.c
	gcc -O2 text2pdf.c -o text2pdf.exe

paper/firstpage.2c : twocolumn.exe paper/firstpage.txt
	# These args are bogus; for testing...
	./twocolumn.exe -startline 12 -startcol 19 paper/firstpage.txt -o $@
	./text2pdf -c160 -s6 -v6 -l128 debug.page > debug.pdf

paper/paper.pdf : dos/strlit.exe text2pdf.exe makefile
	./text2pdf -c160 -s6 -v6 -l128 dos/strlit.exe > paper/paper.pdf

tests : $(addprefix dos/,$(TESTS)) dos/tests.bat

testclean :
	rm -f $(addprefix dos/,$(TESTS))
	rm -f dos/*.asm dos/*.esm dos/*.cil dos/*.bytes

dos/tests.bat : maketest.exe makefile
	./maketest.exe $(TESTS) > dos/tests.bat

dos/%.exe : %.c abc.exe
	./abc.exe $< -o $@
