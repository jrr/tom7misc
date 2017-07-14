all : frameserver.exe anaglyph.exe

MLTON=/c/mlton/bin/mlton.bat

frameserver.exe : frameserver.mlb frameserver.sml
	${MLTON} -output $@ frameserver.mlb

anaglyph.exe : anaglyph.mlb atom*.sml anaglyph.sml anaglyph-main.sml
	${MLTON} -output $@ anaglyph.mlb
