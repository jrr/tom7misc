# Spinoff of pactom with a focus on regular running of the same
# routes.

all : makesegments.exe

MLTON=/c/mlton/bin/mlton.bat

makesegments.exe : makesegments.mlb gpx*.sml segments*.sml makesegment*.sml
	${MLTON} -output $@ makesegments.mlb
