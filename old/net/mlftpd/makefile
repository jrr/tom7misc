
default : mlftpd

sockprim.o : sockprim.c
	gcc -O -c sockprim.c

#procprim.o : procprim.c
#	gcc -O -c procprim.c

mlftpd : *.sml mlftpd.cm sockprim.o 
	mlton mlftpd.cm sockprim.o
	strip mlftpd

