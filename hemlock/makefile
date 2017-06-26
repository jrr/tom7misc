
default : hemlock

hemlock.cm : sources.cm
	rm -f hemlock.cm
	cp sources.cm hemlock.cm
	echo "" >> hemlock.cm
	echo "main.sml" >> hemlock.cm

hemlock : makefile hemlock.cm *.sml cps/*.sml front/*.sml cparsing/*.sml rtl/*.sml talend/*.sml runtime/*.sml parser/*.sml util/*.sml
	time mlton hemlock.cm

typecheck : *.sml parser/*.sml cparsing/*.sml
	echo "CM.make();" | sml

# should remove some generated files in runtime/...
clean :
	rm -f `find . -name "*~"` *.exe hemlock.cm

wc :
	find . -name "*.sml" | grep -v CM | xargs wc -l

linelen :
	linelen `find . -name "*.sml" | grep -v CM`
