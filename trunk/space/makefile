all : music.js

upload :
	pscp *.js *.png *.wav index.html root@spacebar.org:/var/www/FIXME/

# Generates music javascript data from MIDI files.
music.js : theme.mid theme-maj.mid ../renderhero/dumpmidi.exe
	echo "/* Generated file, do not edit */" > music.js
	../renderhero/dumpmidi.exe theme.mid song_theme >> music.js
	../renderhero/dumpmidi.exe theme-maj.mid song_theme_maj >> music.js
	dos2unix music.js

