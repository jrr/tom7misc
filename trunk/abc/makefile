all : abc.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

abc.exe : *.sml abc.mlb
	$(MLTON) -output $@ abc.mlb

maketest.exe : maketest.sml maketest.mlb
	$(MLTON) -output $@ maketest.mlb

TESTS = builtin.exe simpcall.exe globals.exe globals2.exe callargs.exe

tests : $(addprefix dos/,$(TESTS)) dos/test.bat

dos/test.bat : maketest.exe makefile
	./maketest.exe $(TESTS) > dos/test.bat

dos/%.exe : %.c abc.exe
	./abc.exe $< -o $@
