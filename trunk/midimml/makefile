
default : midimml.exe

midimml.exe : midimml.cm inst.sml main.sml
	mlton -output midimml.exe midimml.cm 

# include more examples?
midimml2.2.zip : midimml.exe README default.mmc COPYING cygwin1.dll calmboat.mid calmboat.mmc changelog
	rm -f $@
	zip -9 $@ $^

release : midimml2.2.zip
	pscp midimml2.2.zip tom7@gs82.sp.cs.cmu.edu:www/midimml/
