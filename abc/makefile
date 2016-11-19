all : abc.exe

# just MLTON=mlton should work on most installations btw
MLTON=/c/mlton/bin/mlton.bat

abc.exe : *.sml abc.mlb
	$(MLTON) -output $@ abc.mlb
