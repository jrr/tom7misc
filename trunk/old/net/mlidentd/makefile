
default : mlidentd

mlidentd : *.sml mlidentd.cm sockprim.o
	mlton mlidentd.cm sockprim.o
	strip mlidentd

