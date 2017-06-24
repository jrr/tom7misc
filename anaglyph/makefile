all : frameserver.exe

MLTON=/c/mlton/bin/mlton.bat

frameserver.exe : frameserver.mlb frameserver.sml
	${MLTON} -output $@ frameserver.mlb
