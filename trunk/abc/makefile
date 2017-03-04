all : abc.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

abc.exe : *.sml abc.mlb
	$(MLTON) -output $@ abc.mlb

maketest.exe : maketest.sml maketest.mlb
	$(MLTON) -output $@ maketest.mlb

TESTS = builtin.exe simpcall.exe globals.exe globals2.exe callargs.exe dead.exe cond.exe cond2.exe

tests : $(addprefix dos/,$(TESTS)) dos/tests.bat

testclean :
	rm -f $(addprefix dos/,$(TESTS))
	rm -f dos/*.asm dos/*.esm dos/*.cil dos/*.bytes

dos/test.bat : maketest.exe makefile
	./maketest.exe $(TESTS) > dos/tests.bat

dos/%.exe : %.c abc.exe
	./abc.exe $< -o $@
