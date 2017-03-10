all : abc.exe benchbytes.exe text2pdf.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

abc.exe : *.sml abc.mlb
	$(MLTON) -output $@ abc.mlb

maketest.exe : maketest.sml maketest.mlb
	$(MLTON) -output $@ maketest.mlb

benchbytes.exe : benchbytes.sml benchbytes.mlb
	$(MLTON) -output $@ benchbytes.mlb

TESTS = builtin.exe simpcall.exe globals.exe globals2.exe callargs.exe dead.exe cond.exe cond2.exe simpleop.exe for.exe write.exe strlit.exe # forswitch.exe

text2pdf.exe : text2pdf.c
	gcc -O2 text2pdf.c -o text2pdf.exe

paper/paper.pdf : dos/write.exe text2pdf.exe makefile
	./text2pdf -c160 -s6 -v6 -l128 dos/write.exe > paper/paper.pdf

tests : $(addprefix dos/,$(TESTS)) dos/tests.bat

testclean :
	rm -f $(addprefix dos/,$(TESTS))
	rm -f dos/*.asm dos/*.esm dos/*.cil dos/*.bytes

dos/tests.bat : maketest.exe makefile
	./maketest.exe $(TESTS) > dos/tests.bat

dos/%.exe : %.c abc.exe
	./abc.exe $< -o $@
