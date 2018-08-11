all : music.js

upload :
	pscp *.js *.png *.wav index.html root@spacebar.org:/var/www/FIXME/

# Generates music javascript data from MIDI files.
music.js : theme.mid ../renderhero/dumpmidi.exe
	echo "/* Generated file, do not edit */" > music.js
	../renderhero/dumpmidi.exe theme.mid song_theme >> music.js
	dos2unix music.js

