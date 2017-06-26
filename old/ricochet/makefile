default : ricochetex2.exe

CLFLAGS= /Ot /Ob2 /Og /Oi /Oy /Oa /O2 /Gs /GR- /GX- /Gi- /Gm- /Gr /nologo /W2
#CLFLAGS=/nologo /W2 /GZ /Yd /Zi

ricochet.exe : ricochet.c makefile
	cl $(CLFLAGS) ricochet.c

# for profiling: add -pg
ricochet : ricochet.c makefile
	gcc -O3 -fomit-frame-pointer -funroll-loops ricochet.c -o ricochet
#	gcc -O -g ricochet.c -o ricochet -lefence

ricochetex.exe : ricochetex.c makefile
	cl $(CLFLAGS) ricochetex.c

ricochetex2.exe : ricochetex2.cpp makefile
	cl $(CLFLAGS) ricochetex2.cpp


bench : ricochet dummy
	./ricochet -version > benchtmp.txt~~
	echo "" >> benchtmp.txt~~
	uname -a >> benchtmp.txt~~
	echo "" >> benchtmp.txt~~
	time -a -o benchtmp.txt~~ ./ricochet -b tests/real02.txt
	mv benchtmp.txt~~ bench/`date +%y.%j.%H.%M.%S`.txt

dummy :
	echo

clean :
	rm -f *~ *.pdb *.obj
