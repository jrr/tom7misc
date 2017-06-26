net use l: \\ludus\source /y
del src\*.o
del map\*.o
strip map\map.exe
djp map\map.exe
ducpy < texts l:\
xcmd < bins copy /y
lastup\lastup l:\updated.day
