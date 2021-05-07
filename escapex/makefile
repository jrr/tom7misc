ifeq ($(OS),Windows_NT)
	export WIN32=true
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
		export LINUX=true
    endif
    ifeq ($(UNAME_S),Darwin)
		export OSX=true
		export OS_MACOSX
    endif
endif

# ----
# Things that I expect to remain OS-specific here. (like which compiler to use)
# Maybe many of the OS-specific bits further down can eventually go away?

ifdef LINUX

CXX=g++

# for multiuser, add -DMULTIUSER -DDATADIR=\"/tmp/escapedata/\" -DSTARTUP_LEVELS=\"/tmp/escapelevels\"
# with appropriate paths to the static data (trailing slash for data dir; no trailing for startup)
CPPFLAGS = `sdl-config --cflags` $(SDLNETINCLUDES) -DNOSOUND=1 -DLINUX=1
CXXFLAGS = --std=c++17 -g -Wall -O2 -Wno-sign-compare $(CCLIBINCLUDES)

LDFLAGS = -Wl,-rpath=.
LDLIBS = `sdl-config --libs`

STATIC=-static

STRIP=@true

endif

ifdef WIN32

CXX=x86_64-w64-mingw32-g++
WINDRES=x86_64-w64-mingw32-windres

CPPFLAGS=-O2 -I .
CXXFLAGS= -std=c++17 ${OPT} $(DEBUG) -DNOSOUND=1 -DWIN32 -Wall -Wno-format -Wno-unused-function -Wno-deprecated -Wno-sign-compare -I/usr/local/include -I/usr/include $(CCLIBINCLUDES) -I.
SDL_CXXFLAGS= ${CXXFLAGS} ${SDLINCLUDES} ${SDLNETINCLUDES}

STATIC=-static
STRIP=strip

endif

ifdef OSX

CXX=g++-8

CXXFLAGS=-std=c++17 -DOS_MACOSX -DNOSOUND=1
CPPFLAGS= -arch x86_64 -I. -I../cc-lib -I/usr/local/include -I${FRAMEWORKS}/SDL.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_net.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_mixer.framework/Versions/Current/Headers -D_THREAD_SAFE -DOSX

LDLIBS = `sdl-config --libs`

# deliberately empty:
STATIC=

STRIP=@true

endif


# -------------------------------------------------------------------
# factoring out common bits here:

default : escape.exe

# cc-lib is my personal collection of C++ libraries. It should be in a
# directory next to "escapex."
CC_LIB=../cc-lib

# Included with escape with some superficial local fixes, but feel
# free to try a pristine copy instead.
# SDL_net is just compiled from the small number of sources now,
# included in this subdirectory
SDL_NET=SDL_net

# objects that do not need SDL; can be linked into tools like screenshot.exe; server
esclib.a: level.o solution.o rle.o disamb.o escape-util.o graphics.o
	ar cr $@ $^

sdlutil.a: $(CC_LIB)/sdl/sdlutil.o $(CC_LIB)/sdl/font.o
	ar cr $@ $^

# cc-lib assumes it can include something like "base/macros.h"
CCLIBINCLUDES=-I$(CC_LIB)

cclib.a: $(CC_LIB)/image.o $(CC_LIB)/util.o $(CC_LIB)/stb_image.o $(CC_LIB)/stb_image_write.o $(CC_LIB)/crypt/md5.o $(CC_LIB)/base64.o $(CC_LIB)/base/logging.o $(CC_LIB)/base/stringprintf.o
	ar cr $@ $^


SDLNETINCLUDES=-I$(SDL_NET)

sdlnet.a: $(SDL_NET)/SDLnet.o $(SDL_NET)/SDLnetselect.o $(SDL_NET)/SDLnetTCP.o $(SDL_NET)/SDLnetUDP.o
	ar cr $@ $^

ESCOBJECTS=main.o loadlevel.o play.o player.o playerdb.o prompt.o draw.o drawable.o edit.o editprefab.o mainmenu.o upgrade.o http.o httputil.o textscroll.o message.o update.o editai.o dircache.o upper.o registration.o upload.o rating.o menu.o prefs.o chunks.o dirindex.o textbox.o analysis.o generator.o primes.o commenting.o cleanup.o mainshow.o handhold.o animation.o dirt.o sound.o optimize.o solutionuploading.o client.o progress.o leveldb.o startup.o esclib.a backgrounds.o escapex.o browse.o


%-console.o : %.cc
	${CXX} ${CXXFLAGS} ${CPPFLAGS} -DNOSOUND=1 -DUSE_DISPLAY_FORMAT=0 $^ -c -o $@

# -------------------------------------------------------------------

#######################
######## LINUX ########
#######################

ifdef LINUX
# makefile for linux

# to distribute dynamic: sdl-config --prefix=.
# then ldd escape.exe
# and upload the .so files with the thingy


# -lvorbisfile -lvorbis -logg
# -lSDL_net -lSDL_mixer

#  LDLIBS = -lefence

#  -lvorbisfile -lvorbis -logg

sound_load.h : sound.pack packsound.cc
	make packsound.exe
	./packsound.exe sound.pack sound


.dummy :

version : .dummy
	${CXX} version.cc -o version.exe
	./version.exe
	make clean
	make escape.exe

# we should just distribute a statically linked binary?
LIBFILES=libogg.so.0 libSDL-1.2.so.0 libSDL_net-1.2.so.0 libvorbisfile.so.3 libvorbis.so.0 libartsc.so.0 libesd.so.0

# XXX Unfortunately, no machine on which to build linux binaries in 2010. (VMware?)
RELEASEFILES=
# RELEASEFILES=escape.exe font.png fontsmall.png tiles.png tileutil.png title.png icon.png escape.txt COPYING changelog animation.png splash.png ${LIBFILES}


releasefiles.linux : ${RELEASEFILES}
	echo ${RELEASEFILES} > releasefiles.linux

mkupgrade.exe : mkupgrade.o ../cc-lib/crypt/md5.o
	${CXX} $^ -o mkupgrade.exe

CURRENT : .dummy escape.exe releasefiles.linux symlinks.linux deletefiles.linux mkupgrade.exe
	$(STRIP) escape.exe
	./mkupgrade.exe releasefiles.linux symlinks.linux deletefiles.linux > UPGRADE
	./mkupgrade.exe -v > CURRENT

UPGRADE: CURRENT

release : UPGRADE
	pscp -C ${RELEASEFILES} UPGRADE CURRENT root@spacebar.org:/var/www/escape/linux/
	rm -f UPGRADE mkupgrade.exe

zip : escape-beta.tar.gz

escape-beta.tar.gz : escape.exe
	rm -rf /tmp/escape
	rm -f /tmp/escape-beta.tar
	rm -f /tmp/escape-beta.tar.gz
	rm -f escape-beta.tar.gz
	mkdir /tmp/escape
	$(STRIP) escape.exe
	cp ${RELEASEFILES} /tmp/escape
	cp -a triage /tmp/escape
	cp -a official /tmp/escape
	mkdir /tmp/escape/mylevels
	cp mylevels/index.esi /tmp/escape/mylevels/
	cd /tmp && tar -c escape > escape-beta.tar && gzip escape-beta.tar
	mv /tmp/escape-beta.tar.gz .

zipup : escape-beta.tar.gz
	scp escape-beta.tar.gz root@spacebar.org:/var/www/escape/

source : escape-src.tar.bz2

escape-src.tar.bz2 :
	cd /usr/src/escape-src && svn up
	rm -rf /usr/src/escape-src/web
	rm -rf /usr/src/escape-src/*.esp /usr/src/escape-src/*.esd
	cd /usr/src/escape-src && chmod a-x `find . -name "*.h"` `find . -name "*.cc"` `find . -name "*.pack"` `find . -name "*.esx"` `find . -name "*.m"`
	cd /usr/src && tar -c escape-src --exclude "*CVS*" --exclude "*.svn*" --exclude "*.cvsignore" > /tmp/escape-src.tar
	cd /tmp && bzip2 escape-src.tar
	mv /tmp/escape-src.tar.bz2 .

# assumes we're running it on spacebar
sourceup : source CURRENT
	cp escape-src.tar.bz2 /var/www/escape/source/escape-src-`cat CURRENT`.tar.bz2
	rm -rf /var/www/escape/source/escape-src.tar.bz2
	ln -s escape-src-`cat CURRENT`.tar.bz2 /var/www/escape/source/escape-src.tar.bz2


# end linux makefile
endif


#########################
######## WINDOWS ########
#########################

# mingw makefile. This is the new way to compile on Windows.
ifdef WIN32


# Depending on your installation, this may work better for you...
# CXX=g++
# CC=gcc
# WINDRES=windres
# But this is how Tom compiles, specifically requesting the 64-bit compiler.

# point this at the SDL (1.2 series) installation. You need to compile
# SDL in there too; we just link in the object files.
SDL=/c/code/SDL


# assume SDL build for .o files in the current directory
%.o : %.cc
	@${CXX} ${CPPFLAGS} ${SDL_CXXFLAGS} -c -o $@ $<
	@bash -c "echo -n '.'"

# .. and sdl_net
$(SDL_NET)/%.o : $(SDL_NET)/%.c
	@${CXX} ${CPPFLAGS} ${SDL_CXXFLAGS} -c -o $@ $<
	@bash -c "echo -n '.'"

SDLARCH=x64
SDLINCLUDES=-I$(SDL)/include
SDLOPATH=${SDL}/build
SDLOBJECTS=${SDLOPATH}/SDL.o ${SDLOPATH}/SDL_error.o ${SDLOPATH}/SDL_fatal.o ${SDLOPATH}/SDL_audio.o ${SDLOPATH}/SDL_audiocvt.o ${SDLOPATH}/SDL_audiodev.o ${SDLOPATH}/SDL_mixer.o ${SDLOPATH}/SDL_mixer_MMX.o ${SDLOPATH}/SDL_mixer_MMX_VC.o ${SDLOPATH}/SDL_mixer_m68k.o ${SDLOPATH}/SDL_wave.o ${SDLOPATH}/SDL_cdrom.o ${SDLOPATH}/SDL_cpuinfo.o ${SDLOPATH}/SDL_active.o ${SDLOPATH}/SDL_events.o ${SDLOPATH}/SDL_expose.o ${SDLOPATH}/SDL_keyboard.o ${SDLOPATH}/SDL_mouse.o ${SDLOPATH}/SDL_quit.o ${SDLOPATH}/SDL_resize.o ${SDLOPATH}/SDL_rwops.o ${SDLOPATH}/SDL_getenv.o ${SDLOPATH}/SDL_iconv.o ${SDLOPATH}/SDL_malloc.o ${SDLOPATH}/SDL_qsort.o ${SDLOPATH}/SDL_stdlib.o ${SDLOPATH}/SDL_string.o ${SDLOPATH}/SDL_thread.o ${SDLOPATH}/SDL_timer.o ${SDLOPATH}/SDL_RLEaccel.o ${SDLOPATH}/SDL_blit.o ${SDLOPATH}/SDL_blit_0.o ${SDLOPATH}/SDL_blit_1.o ${SDLOPATH}/SDL_blit_A.o ${SDLOPATH}/SDL_blit_N.o ${SDLOPATH}/SDL_bmp.o ${SDLOPATH}/SDL_cursor.o ${SDLOPATH}/SDL_gamma.o ${SDLOPATH}/SDL_pixels.o ${SDLOPATH}/SDL_stretch.o ${SDLOPATH}/SDL_surface.o ${SDLOPATH}/SDL_video.o ${SDLOPATH}/SDL_yuv.o ${SDLOPATH}/SDL_yuv_mmx.o ${SDLOPATH}/SDL_yuv_sw.o ${SDLOPATH}/SDL_joystick.o ${SDLOPATH}/SDL_nullevents.o ${SDLOPATH}/SDL_nullmouse.o ${SDLOPATH}/SDL_nullvideo.o ${SDLOPATH}/SDL_diskaudio.o ${SDLOPATH}/SDL_dummyaudio.o ${SDLOPATH}/SDL_sysevents.o ${SDLOPATH}/SDL_sysmouse.o ${SDLOPATH}/SDL_syswm.o ${SDLOPATH}/SDL_wingl.o ${SDLOPATH}/SDL_dibevents.o ${SDLOPATH}/SDL_dibvideo.o ${SDLOPATH}/SDL_dx5events.o ${SDLOPATH}/SDL_dx5video.o ${SDLOPATH}/SDL_dx5yuv.o ${SDLOPATH}/SDL_dibaudio.o ${SDLOPATH}/SDL_dx5audio.o ${SDLOPATH}/SDL_mmjoystick.o ${SDLOPATH}/SDL_syscdrom.o ${SDLOPATH}/SDL_sysmutex.o ${SDLOPATH}/SDL_syssem.o ${SDLOPATH}/SDL_systhread.o ${SDLOPATH}/SDL_syscond.o ${SDLOPATH}/SDL_systimer.o ${SDLOPATH}/SDL_sysloadso.o sdl-win32-main.o


LINKSDL=  -lm -mthreads -luser32 -lgdi32 -lwinmm -ldxguid
LINKSDLNET= -lws2_32 -liphlpapi

# XXX reenable sound
DEBUG=-g
OPT=-O2
#


# -Wl,-rpath=.

# It works fine to pick either one of
#    -Wl,--subsystem,console
#    -Wl,--subsystem,windows
# and the former opens the console for useful/annoying debugging info.
# SUBSYSTEM=windows
SUBSYSTEM=console
# You can remove this GNU-ism if you need to; it just prevents me from
# accidentally releasing the console build.
release_check = \
	$(if $(filter windows,${SUBSYSTEM}),,\
	$(error SUBSYSTEM is ${SUBSYSTEM} -- do not release in console mode!))

LDFLAGS =  -L. ${LINKSDL} ${LINKSDLNET} -m64 -Wl,--subsystem,${SUBSYSTEM} ${OPT}  -static

# -lSDL_mixer
# at end  -lvorbisfile -lvorbis -logg
# LDLIBS = -lSDL_net  `sdl-config --libs` -lz
#  LDLIBS = -lefence

# XXX need to specify sdl flags presumably
# CPPFLAGS = -DNOSOUND=1 `sdl-config --cflags`

icon.o : escape.ico escape.rc
	$(WINDRES) -i escape.rc -o $@

EXTRA_OBJS=icon.o ${SDLOBJECTS}


replace.exe : replace.o escape-util.o
	${CXX} -g -o $@ $^ ${LDFLAGS}


rle_test.exe : lib/rle.o lib/rle_test.o
	${CXX} $(CXXFLAGS) -o $@ $^ -static
	$(STRIP) rle_test.exe

sound_load.h : sound.pack packsound.cc packsound.exe
	./packsound.exe sound.pack sound

# if you don't want to deal with zopflipng to optimize images,
# you could probably just replace this with like ZOPFLI=cp
ZOPFLI=./zopflipng.exe --splitting=3 --filters=01234mepb --lossy_8bit --lossy_transparent

# generate animation_syms.h, animation.png, etc. % is just used as a
# trick so that gnu make lets this generate multiple targets (it
# requires a common substring to match in all positions. here % =
# animation. $* yields the matched string in the commands.)
%_copy.h %.png %_defs.h %_syms.h : packpng.exe %.pack
	./packpng.exe $*.pack $*
	rm -f $*-zop.png
	${ZOPFLI} $*.png $*-zop.png
	rm -f $*.png
	mv $*-zop.png $*.png

optimize-png : animation.png .dummy
	rm -f zop-*.png
	${ZOPFLI} --prefix=zop- font.png fontsmall.png icon.png splash.png tiles.png tileutil.png title.png animation.png
	cp zop-font.png font.png
	cp zop-fontsmall.png fontsmall.png
	cp zop-icon.png icon.png
	cp zop-splash.png splash.png
	cp zop-tiles.png tiles.png
	cp zop-tileutil.png tileutil.png
	cp zop-title.png title.png
	cp zop-animation.png animation.png
	rm -f zop-*.png


packpng.exe : packpng.o cclib.a
	${CXX} -o $@ $^ -static

.dummy :

version : .dummy
	@:$(call release_check)
	${CXX} ${CXXFLAGS} ${CPPFLAGS} version.cc -o version.exe
	./version.exe
	make clean
	make -j 8 escape.exe

# SDL.dll SDL_net.dll zlib1.dll SDL_mixer.dll
RELEASEFILES=escape.exe replace.exe font.png fontsmall.png tiles.png tileutil.png title.png icon.png escape.txt COPYING changelog animation.png splash.png

releasefiles.win32 : ${RELEASEFILES}
	echo ${RELEASEFILES} > releasefiles.win32

mkupgrade.exe : mkupgrade.cc ../cc-lib/crypt/md5.cc
	${CXX} ${CPPFLAGS} ${CXXFLAGS} -DWIN32 mkupgrade.cc ../cc-lib/crypt/md5.cc -o mkupgrade.exe

CURRENT : .dummy escape.exe releasefiles.win32 symlinks.win32 deletefiles.win32 mkupgrade.exe
	@:$(call release_check)
	$(STRIP) escape.exe
	$(STRIP) replace.exe
	./mkupgrade.exe releasefiles.win32 symlinks.win32 deletefiles.win32 > UPGRADE
	./mkupgrade.exe -v > CURRENT

UPGRADE: CURRENT

release : UPGRADE
	pscp -C ${RELEASEFILES} UPGRADE CURRENT root@spacebar.org:/var/www/escape/win32/
	rm -f UPGRADE CURRENT mkupgrade.exe

zip : escape-beta.zip

escape-beta.zip : ${RELEASEFILES} makefile
	@:$(call release_check)
	$(STRIP) escape.exe
	$(STRIP) replace.exe
	rm -f $@
	rm -rf tmp_escapezip
	mkdir tmp_escapezip
	cp ${RELEASEFILES} tmp_escapezip
#	Note: This does mercifully ignore .svn dirs.
	cp -r triage tmp_escapezip/
	cp -r official tmp_escapezip/
	mkdir tmp_escapezip/mylevels
	cp mylevels/index.esi tmp_escapezip/mylevels
	cd tmp_escapezip && zip -r -9 -y $@ *
	mv tmp_escapezip/$@ .
# 	optional, improves size using zopfli. advancemame.it
	./advzip.exe -4 -z $@
	rm -rf tmp_escapezip

zipup : escape-beta.zip
	pscp escape-beta.zip root@spacebar.org:/var/www/escape/

# default location
MAKENSIS='c:\program files (x86)\nsis\makensis.exe'

escapesetup.exe : .dummy ${RELEASEFILES} escape.nsi
	@:$(call release_check)
	$(STRIP) escape.exe
	$(STRIP) replace.exe
	${MAKENSIS} escape.nsi

installer : escapesetup.exe

installerup : escapesetup.exe
	pscp escapesetup.exe root@spacebar.org:/var/www/escape/

source : escape-src.tar.bz2

# XXX wrong, bad
escape-src.tar.bz2 :
	cd /usr0/src/escape-src && cvs up -dP
	rm -rf /usr0/src/escape-src/web /usr0/src/escape-src/*.es{p,d}
	cd /usr0/src/escape-src && chmod a-x `find . -name "*.h"` `find . -name "*.cc"` `find . -name "*.pack"` `find . -name "*.esx"` `find . -name "*.m"`
	cd /usr0/src && tar -c escape-src --exclude "*CVS*" --exclude "*.svn*" --exclude "*.cvsignore" > /tmp/escape-src.tar
	cd /tmp && bzip2 escape-src.tar
	mv /tmp/escape-src.tar.bz2 .

# XXX two copies.. better if we could make src.tar be a symlink to the versioned tar.
sourceup : source CURRENT
	pscp escape-src.tar.bz2 root@spacebar.org:/var/www/escape/source/
	pscp escape-src.tar.bz2 root@spacebar.org:/var/www/escape/source/escape-src-`cat CURRENT`.tar.bz2


# end mingw makefile
endif

####### old osx stuff below here

# -------------------------------------------------------------------


########################
######## MAC OS ########
########################

ifdef OSX
# makefile for OSX

# XXX reinstate sdl_mixer before doing another release!

default : escape.exe

FRAMEWORKS=OSX_build/Frameworks

# No longer cavemen, we target a recent release of OS X.
VERSION_TARGET=10.10
# Hack from t7eshero. Maybe should be fixed.
RPATH=-rpath @executable_path/../Frameworks

# Suppress compilation commands, but show some indication of progress.
# (Explicitly invoke bash to get shell builtin, since on OS X echo
# otherwise treats -n literally.)
%.o : %.cc
	${CXX} ${CPPFLAGS} ${CXXFLAGS} -c -o $@ $<
	# @${CXX} ${CPPFLAGS} ${CXXFLAGS} -c -o $@ $<
	# @bash -c "echo -n '.'"

# sound is disabled, but it's time to make it available on the OS X
# release. Do this!
# to add it, remove -DNOSOUND and add
#
# to CPPFLAGS and add
#  -framework SDL_mixer
# to the g++ line linking below


# XXX: I added -I. here and made a symlink to the SDL include
# directory called "SDL", since some frameworks want to include like
# <SDL/sdl.h>. (is it just the now-deleted sdl_image, though?) Don't
# know what the right fix is.

# good for 10.11 MacBook
# used to use /usr/local/lib
LIBS=-L/usr/lib

CCLIBOBJECTS=../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/sdl/sdlutil.o ../cc-lib/sdl/font.o ../cc-lib/crypt/md5.o ../cc-lib/base64.o

escape_mac_old.exe : ${ESCOBJECTS} cclib.a sdlnet.a sdlutil.a ${EXTRA_OBJS}
	@export MACOSX_DEPLOYMENT_TARGET=${VERSION_TARGET}
	${CXX} -arch x86_64 -F${FRAMEWORKS} -framework SDL_mixer -framework SDL_net ${LIBS} -framework SDL -framework OpenGL -framework AGL -framework IOKit -framework Carbon -framework Cocoa ${RPATH} $^ -o escape.exe

.dummy :

version : .dummy
	${CXX} version.cc -o version.exe
	./version.exe
	make clean
	make escape.exe

RELEASEFILES=escape.exe font.png fontsmall.png tiles.png tileutil.png title.png icon.png escape.txt COPYING changelog animation.png splash.png

mkupgrade.exe : mkupgrade.cc ../cc-lib/crypt/md5.cc
	${CXX} ${CPPFLAGS} ${CXXFLAGS} -DOSX mkupgrade.cc ../cc-lib/crypt/md5.cc -o mkupgrade.exe

releasefiles.osx :
	echo ${RELEASEFILES} > releasefiles.osx

UPGRADE : .dummy mkupgrade.exe releasefiles.osx symlinks.osx deletefiles.osx
	$(STRIP) escape.exe
	./mkupgrade.exe releasefiles.osx symlinks.osx deletefiles.osx > UPGRADE
	./mkupgrade.exe -v > CURRENT

release : UPGRADE
	rm -rf ../release
	mkdir ../release
	cp ${RELEASEFILES} CURRENT UPGRADE ../release/
	rm -f UPGRADE CURRENT mkupgrade.executables

releaseup : release
	cd ../release && scp * root@spacebar.org:/var/www/escape/osx

zip : escape-beta-osx.zip

# XXX Note: in 2016 I didn't need to do this stuff; svn seems to handle
# symlinks correctly.
# for some reason frameworks need some redundance built with symlinks
# XXX give this deps so it knows when to run
framelinks :
	-mkdir OSX_build/Frameworks/SDL.framework/Versions
	-mkdir OSX_build/Frameworks/SDL.framework/Versions/A
	-mkdir OSX_build/Frameworks/SDL_net.framework/Versions
	-mkdir OSX_build/Frameworks/SDL_net.framework/Versions/A
	-mkdir OSX_build/Frameworks/SDL_mixer.framework/Versions
	-mkdir OSX_build/Frameworks/SDL_mixer.framework/Versions/A
	cd OSX_build/Frameworks/SDL.framework/Versions/A && ln -s ../../SDL SDL && ln -s ../../Headers Headers && ln -s ../../Resources Resources && cd .. && ln -s A Current
	cd OSX_build/Frameworks/SDL_net.framework/Versions/A && ln -s ../../SDL_net SDL_net && ln -s ../../Headers Headers && ln -s ../../Resources Resources && cd .. && ln -s A Current


# OSX_build goes inside an otherwise empty folder called
# Escape.app. The folder should be renamed to Contents.
# inside Contents/MacOS goes all of the regularly distributed
# files.
# the folder Escape.app is zipped to produce the result
app : escape.exe
	rm -rf /tmp/Escape.app
	rm -f /tmp/escape-beta-osx.zip
	rm -f escape-beta-osx.zip
	mkdir /tmp/Escape.app
	mkdir /tmp/Escape.app/Contents
	mkdir /tmp/Escape.app/Contents/MacOS
	cp -R OSX_build/* /tmp/Escape.app/Contents/
	cp ${RELEASEFILES} /tmp/Escape.app/Contents/MacOS/
	cp -R triage /tmp/Escape.app/Contents/MacOS/
	cp -R official /tmp/Escape.app/Contents/MacOS/
	mkdir /tmp/Escape.app/Contents/MacOS/mylevels
	cp mylevels/index.esi /tmp/Escape.app/Contents/MacOS/mylevels/
	mkdir /tmp/Escape.app/Contents/MacOS/OSX_build
	mkdir /tmp/Escape.app/Contents/MacOS/OSX_build/Frameworks
	rm -f /tmp/Escape.app/Contents/Frameworks/SDL.framework/Headers/*.h
	rm -f /tmp/Escape.app/Contents/Frameworks/SDL_net.framework/Headers/*.h
	rm -f /tmp/Escape.app/Contents/Frameworks/SDL_mixer.framework/Headers/*.h
	rm -rf `find /tmp/Escape.app -name CVS`
	rm -rf `find /tmp/Escape.app -name .svn`
	rm -rf `find /tmp/Escape.app -name .DS_Store`

desktop : app
	rm -rf ~/Desktop/Escape.app.old
	-mv ~/Desktop/Escape.app ~/Desktop/Escape.app.old
	cp -R /tmp/Escape.app ~/Desktop/

escape-beta-osx.zip : app
	cd /tmp && zip -r -9 -y $@ Escape.app
	rm -f $@
	cp /tmp/$@ .

zipup : escape-beta-osx.zip
	scp escape-beta-osx.zip root@spacebar.org:/var/www/escape/

# end OSX makefile
endif

wc :
	wc -l *.cc *.h

clean :
	rm -f *.o escape.exe core core.[0-9]* .DS_Store gmon.out escape-src.tar.bz2 escape-beta.tar.gz vgcore.* *.a

escape.exe : ${ESCOBJECTS} cclib.a sdlnet.a sdlutil.a ${EXTRA_OBJS}
	${CXX} -g -o $@ $^ ${LDLIBS} ${LDFLAGS}

screenshot.exe : screenshot.o esclib.a cclib.a
	$(CXX) $(CXXFLAGS) -o $@ $^ $(STATIC)

packsound.exe : packsound.o cclib.a
	${CXX} -o $@ $^ ${LDFLAGS} $(STATIC)
	$(STRIP) packsound.exe