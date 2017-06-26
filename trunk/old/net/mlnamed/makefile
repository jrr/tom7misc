
default : mlnamed

mlnamed : *.sml mlnamed.cm
	mlton -v mlnamed.cm sockprim.c
	strip mlnamed

