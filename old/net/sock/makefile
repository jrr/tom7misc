
default : sock

sockprim.o : sockprim.c
	gcc -O -Wall -c sockprim.c

sock : *.sml sockprim.o sock.cm
	mlton sock.cm sockprim.o
	strip sock

send : *.sml sockprim.o send.cm
	mlton send.cm sockprim.o
	strip send

recv : *.sml sockprim.o recv.cm
	mlton recv.cm sockprim.o
	strip recv

nj : *.sml *.cm
	cat sockprim.sml | sed -e 's/_ffi/Unsafe.cast/' > sockprim-fake.sml
	cat sock.cm | sed -e 's/sockprim.sml/sockprim-fake.sml/' > sources.cm

typecheck : nj
	echo "CM.make();" | sml
