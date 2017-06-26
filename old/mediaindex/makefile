
default : mediaindex.exe

mediaindex.exe : mediaindex.cpp dirent.c util.cpp mediaindex.res
	cl /GX /nologo /D "WIN32" $^ /o mediaindex.exe

mediaindex.res : mediaindex.rc mediaindex.ico
	rc /r /fo mediaindex.res mediaindex.rc
