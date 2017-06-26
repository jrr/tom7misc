
default : blam.exe


blam.exe : *.sml gpsac.o gpsa-mtx.o
	mlton -default-ann "allowImport true" -output blam.exe blam.cm gpsac.o gpsa-mtx.o

gpsa-mtx-portable.S : gpsa-mtx-portable.c
	gcc -O3 -fomit-frame-pointer -march=athlon-xp -funroll-loops -S -o gpsa-mtx-portable.S -fverbose-asm $^

gpsac.o : gpsa.c
	gcc -O3 -funroll-loops -fomit-frame-pointer gpsa.c -c -o gpsac.o

gpsa-mtx.o : gpsa-mtx.c
	gcc -O3 -fomit-frame-pointer gpsa-mtx.c -c -o gpsa-mtx.o

gpsa-mtx.S : gpsa-mtx.c
	gcc -O3 -fomit-frame-pointer gpsa-mtx.c -S -o gpsa-mtx.S

gpsa : *.sml gpsac.o
	mlton gpsa.cm gpsac.o

gpsac.S : gpsa.c
	gcc -g -O3 -funroll-loops -fomit-frame-pointer gpsa.c -S -o gpsac.S

test : test.c gpsa.c
	gcc -g -O3 -funroll-loops -fomit-frame-pointer $^ -o $@

upload : blam.exe
	pscp blam.exe tom@spacebar.org:/home/httpd/spacebar/blam/
