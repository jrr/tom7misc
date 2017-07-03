all : frameserver.exe anaglyph.exe

MLTON=/c/mlton/bin/mlton.bat

frameserver.exe : frameserver.mlb frameserver.sml
	${MLTON} -output $@ frameserver.mlb

anaglyph.exe : anaglyph.mlb anaglyph.sml
	${MLTON} -output $@ anaglyph.mlb
