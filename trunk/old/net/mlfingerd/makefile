
default : mlfingerd

sockprim.o : sockprim.c
	gcc -O -c sockprim.c

mlfingerd : *.sml mlfingerd.cm sockprim.o
	mlton -default-ann 'allowImport true' mlfingerd.cm sockprim.o
	strip mlfingerd

