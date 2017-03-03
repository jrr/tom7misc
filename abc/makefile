all : abc.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat
# MLTON=mlton

abc.exe : *.sml abc.mlb
	$(MLTON) -output $@ abc.mlb

tests : dos/builtin.exe dos/simpcall.exe

dos/%.exe : %.c abc.exe
	./abc.exe $< -o $@
