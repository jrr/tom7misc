
default : mlsshd

../sock/sockprim.o : ../sock/sockprim.c
	gcc -O -c ../sock/sockprim.c -o ../sock/sockprim.o

mlsshd : mlsshd.cm *.sml /usr/src/sml-lib/crypt/*.sml ../sock/sockprim.o
	mlton -default-ann "allowImport true" mlsshd.cm ../sock/sockprim.o
	strip mlsshd

