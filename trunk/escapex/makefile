# This makefile relies on environment variables to select the
# correct platform. You need to do one of
#   export LINUX=true
#   export OSX=true
#   export MINGW=true
# before you run make, or nothing will happen. See HACKING for
# more information on compiling escape.

# -------------------------------------------------------------------

ifdef LINUX
# makefile for linux

CCLIBOBJECTS=../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/sdl/sdlutil.o ../cc-lib/md5.o ../cc-lib/base64.o

OFILES = main.o level.o load.o font.o play.o util.o player.o playerdb.o prompt.o draw.o drawable.o edit.o editprefab.o mainmenu.o upgrade.o http.o httputil.o textscroll.o message.o update.o editai.o dircache.o upper.o registration.o upload.o rating.o menu.o prefs.o chunks.o dirindex.o textbox.o analysis.o generator.o prime.o commenting.o cleanup.o mainshow.o handhold.o animation.o dirt.o sound.o optimize.o smanage.o client.o progress.o leveldb.o startup.o backgrounds.o escapex.o directories.o browse.o ${CCLIBOBJECTS}

default : escape.exe

# to distribute dynamic: sdl-config --prefix=.
# then ldd escape.exe
# and upload the .so files with the thingy

LDFLAGS = -Wl,-rpath=.

# -lvorbisfile -lvorbis -logg
LDLIBS = -lSDL_net -lSDL_mixer `sdl-config --libs`

#  LDLIBS = -lefence

# for multiuser, add -DMULTIUSER -DDATADIR=\"/tmp/escapedata/\" -DSTARTUP_LEVELS=\"/tmp/escapelevels\"
# with appropriate paths to the static data (trailing slash for data dir; no trailing for startup)
CPPFLAGS = `sdl-config --cflags` 
CXXFLAGS = --std=c++11 -g -Wall -O2 -Wno-sign-compare

escape.exe : ${OFILES}
	g++ -o escape.exe ${LDFLAGS} $^ ${LDLIBS}


default : escape.exe

#  -lvorbisfile -lvorbis -logg
packsound.exe : util-console.o packsound-console.o
	g++ -o $@ $^ `sdl-config --static-libs`
	strip packsound.exe

sound.o : sound.cc sound_load.h
	g++ ${CXXFLAGS} ${CPPFLAGS} -c -o $@ $<

level.o : level.cc move.h
	g++ ${CXXFLAGS} ${CPPFLAGS} -c -o $@ $<

sound_load.h : sound.pack packsound.cc
	make packsound.exe
	./packsound.exe sound.pack sound


%-console.o : %.cc
	g++ ${CXXFLAGS} ${CPPFLAGS} -DNOSOUND=1 -DUSE_DISPLAY_FORMAT=0 $^ -c -o $@

# but this should be static
# XXX how to get sdlutil-console from cc-lib?
screenshot.exe : draw-console.o level-console.o font-console.o util-console.o sdlutil-console.o screenshot-console.o animation-console.o sound-console.o message-console.o drawable-console.o progress-console.o escapex-console.o
	g++ -g -o screenshot.exe $^ `sdl-config --static-libs`

.dummy :

version : .dummy
	g++ version.cc -o version.exe
	./version.exe
	make clean
	make escape.exe

LIBFILES=libogg.so.0 libSDL-1.2.so.0 libSDL_net-1.2.so.0 libvorbisfile.so.3 libvorbis.so.0 libartsc.so.0 libesd.so.0

# XXX Unfortunately, no machine on which to build linux binaries in 2010. (VMware?)
RELEASEFILES=
# RELEASEFILES=escape.exe font.png fontsmall.png tiles.png tileutil.png title.png icon.png escape.txt COPYING changelog animation.png splash.png ${LIBFILES}


releasefiles.linux : ${RELEASEFILES}
	echo ${RELEASEFILES} > releasefiles.linux

mkupgrade.exe : mkupgrade.cc md5.cc
	g++ -DLINUX mkupgrade.cc md5.cc -o mkupgrade.exe

CURRENT : .dummy escape.exe releasefiles.linux symlinks.linux deletefiles.linux mkupgrade.exe
	strip escape.exe
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
	strip escape.exe
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

wc :
	wc -l *.cc *.h

clean :
	rm -f *.o escape.exe core core.[0-9]* gmon.out escape-src.tar.bz2 escape-beta.tar.gz vgcore.*

# end linux makefile
endif

# -------------------------------------------------------------------

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
	@${CXX} ${CPPFLAGS} ${CXXFLAGS} -c -o $@ $<
	@bash -c "echo -n '.'"

# sound is disabled, but it's time to make it available on the OS X
# release. Do this!
# to add it, remove -DNOSOUND and add
# 
# to CPPFLAGS and add
#  -framework SDL_mixer
# to the g++ line linking below

CXXFLAGS=-std=c++14

# XXX: I added -I. here and made a symlink to the SDL include
# directory called "SDL", since some frameworks want to include like
# <SDL/sdl.h>. (is it just the now-deleted sdl_image, though?) Don't
# know what the right fix is.
CPPFLAGS= -arch x86_64 -I. -I/usr/local/include -I${FRAMEWORKS}/SDL.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_net.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_mixer.framework/Versions/Current/Headers -D_THREAD_SAFE -DOSX

# XXX Suppress warning in SDL-supplied .m file, but should figure out what's going on here.
CFLAGS=-Wno-objc-protocol-method-implementation

# good for 10.11 MacBook
# used to use /usr/local/lib
LIBS=-L/usr/lib

CCLIBOBJECTS=../cc-lib/stb_image.o ../cc-lib/stb_image_write.o ../cc-lib/sdl/sdlutil.o ../cc-lib/md5.o ../cc-lib/base64.o

escape.exe : escapex.o browse.o main.o level.o load.o font.o play.o util.o player.o playerdb.o prompt.o draw.o drawable.o edit.o editprefab.o mainmenu.o upgrade.o http.o httputil.o textscroll.o message.o update.o editai.o dircache.o upper.o registration.o upload.o rating.o sdlmain.o menu.o prefs.o chunks.o dirindex.o textbox.o analysis.o generator.o prime.o commenting.o cleanup.o mainshow.o handhold.o animation.o dirt.o sound.o optimize.o smanage.o client.o progress.o leveldb.o startup.o backgrounds.o ${CCLIBOBJECTS}
	@export MACOSX_DEPLOYMENT_TARGET=${VERSION_TARGET}
	g++ -arch x86_64 -F${FRAMEWORKS} -framework SDL_mixer -framework SDL_net ${LIBS} -framework SDL -framework OpenGL -framework AGL -framework IOKit -framework Carbon -framework Cocoa ${RPATH} $^ -o escape.exe

# XXX this would allow us to build "Screenshot.app", but we should
# figure out some other approach for command-line tools.
screenshot.exe : sdlmain.o draw.o screenshot.o escapex.o level.o util.o animation.o message.o progress.o sound.o font.o drawable.o ${CCLIBOBJECTS}
	@export MACOSX_DEPLOYMENT_TARGET=${VERSION_TARGET}
	g++ -arch x86_64 -F${FRAMEWORKS} -framework SDL_mixer -framework SDL_net ${LIBS} -framework SDL -framework OpenGL -framework AGL -framework IOKit -framework Carbon -framework Cocoa ${RPATH} $^ -o $@

.dummy :

version : .dummy
	g++ version.cc -o version.exe
	./version.exe
	make clean
	make escape.exe

RELEASEFILES=escape.exe font.png fontsmall.png tiles.png tileutil.png title.png icon.png escape.txt COPYING changelog animation.png splash.png

mkupgrade.exe : mkupgrade.cc ../cc-lib/md5.cc
	g++ -DOSX mkupgrade.cc ../cc-lib/md5.cc -o mkupgrade.exe

releasefiles.osx :
	echo ${RELEASEFILES} > releasefiles.osx

UPGRADE : .dummy mkupgrade.exe releasefiles.osx symlinks.osx deletefiles.osx
	strip escape.exe
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

# XXX Note: in 2016 I didn't need to do this stuff.
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
	# rm -rf /tmp/Escape.app/Contents/Frameworks/SDL_mixer.framework
	# cp -R OSX_build/Frameworks/SDL_mixer.framework /tmp/Escape.app/Contents/MacOS/OSX_build/Frameworks/
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
	cd /tmp && zip -r -9 -y escape-beta-osx.zip Escape.app
	rm -f escape-beta-osx.zip
	cp /tmp/escape-beta-osx.zip .

zipup : escape-beta-osx.zip
	scp escape-beta-osx.zip root@spacebar.org:/var/www/escape/

wc :
	wc -l *.cc *.h

clean :
	rm -f *.o escape.exe core core.[0-9]* .DS_Store ${CCLIBOBJECTS}


# end OSX makefile
endif

# mingw makefile, using TDM-GCC. This is the new way to compile on Windows.
ifdef MINGW

default : escape.exe

# Tom, you may need to do this:
# export PATH=/c/TDM-GCC-64/bin:${PATH}

%.o : %.cc
	@${CXX} ${CPPFLAGS} ${CXXFLAGS} -c -o $@ $<
	@bash -c "echo -n '.'"

CXX=g++
CC=gcc

SDLARCH=x64
SDLINCLUDES=-ISDL/include
SDLOPATH=SDL/build
SDLOBJECTS=${SDLOPATH}/SDL.o ${SDLOPATH}/SDL_error.o ${SDLOPATH}/SDL_fatal.o ${SDLOPATH}/SDL_audio.o ${SDLOPATH}/SDL_audiocvt.o ${SDLOPATH}/SDL_audiodev.o ${SDLOPATH}/SDL_mixer.o ${SDLOPATH}/SDL_mixer_MMX.o ${SDLOPATH}/SDL_mixer_MMX_VC.o ${SDLOPATH}/SDL_mixer_m68k.o ${SDLOPATH}/SDL_wave.o ${SDLOPATH}/SDL_cdrom.o ${SDLOPATH}/SDL_cpuinfo.o ${SDLOPATH}/SDL_active.o ${SDLOPATH}/SDL_events.o ${SDLOPATH}/SDL_expose.o ${SDLOPATH}/SDL_keyboard.o ${SDLOPATH}/SDL_mouse.o ${SDLOPATH}/SDL_quit.o ${SDLOPATH}/SDL_resize.o ${SDLOPATH}/SDL_rwops.o ${SDLOPATH}/SDL_getenv.o ${SDLOPATH}/SDL_iconv.o ${SDLOPATH}/SDL_malloc.o ${SDLOPATH}/SDL_qsort.o ${SDLOPATH}/SDL_stdlib.o ${SDLOPATH}/SDL_string.o ${SDLOPATH}/SDL_thread.o ${SDLOPATH}/SDL_timer.o ${SDLOPATH}/SDL_RLEaccel.o ${SDLOPATH}/SDL_blit.o ${SDLOPATH}/SDL_blit_0.o ${SDLOPATH}/SDL_blit_1.o ${SDLOPATH}/SDL_blit_A.o ${SDLOPATH}/SDL_blit_N.o ${SDLOPATH}/SDL_bmp.o ${SDLOPATH}/SDL_cursor.o ${SDLOPATH}/SDL_gamma.o ${SDLOPATH}/SDL_pixels.o ${SDLOPATH}/SDL_stretch.o ${SDLOPATH}/SDL_surface.o ${SDLOPATH}/SDL_video.o ${SDLOPATH}/SDL_yuv.o ${SDLOPATH}/SDL_yuv_mmx.o ${SDLOPATH}/SDL_yuv_sw.o ${SDLOPATH}/SDL_joystick.o ${SDLOPATH}/SDL_nullevents.o ${SDLOPATH}/SDL_nullmouse.o ${SDLOPATH}/SDL_nullvideo.o ${SDLOPATH}/SDL_diskaudio.o ${SDLOPATH}/SDL_dummyaudio.o ${SDLOPATH}/SDL_sysevents.o ${SDLOPATH}/SDL_sysmouse.o ${SDLOPATH}/SDL_syswm.o ${SDLOPATH}/SDL_wingl.o ${SDLOPATH}/SDL_dibevents.o ${SDLOPATH}/SDL_dibvideo.o ${SDLOPATH}/SDL_dx5events.o ${SDLOPATH}/SDL_dx5video.o ${SDLOPATH}/SDL_dx5yuv.o ${SDLOPATH}/SDL_dibaudio.o ${SDLOPATH}/SDL_dx5audio.o ${SDLOPATH}/SDL_mmjoystick.o ${SDLOPATH}/SDL_syscdrom.o ${SDLOPATH}/SDL_sysmutex.o ${SDLOPATH}/SDL_syssem.o ${SDLOPATH}/SDL_systhread.o ${SDLOPATH}/SDL_syscond.o ${SDLOPATH}/SDL_systimer.o ${SDLOPATH}/SDL_sysloadso.o sdl-win32-main.o

SDLNETINCLUDES=-ISDL_net
SDLNETOBJECTS=SDL_net/SDLnet.o SDL_net/SDLnetselect.o SDL_net/SDLnetTCP.o SDL_net/SDLnetUDP.o

LINKSDL=  -lm -mthreads -luser32 -lgdi32 -lwinmm -ldxguid
LINKSDLNET= -lws2_32 -liphlpapi

# XXX some of these have moved to cc-lib
ESCOBJECTS= main.o level.o load.o font.o play.o util.o player.o playerdb.o prompt.o draw.o drawable.o edit.o editprefab.o mainmenu.o upgrade.o http.o httputil.o textscroll.o message.o update.o editai.o dircache.o upper.o registration.o upload.o rating.o menu.o prefs.o chunks.o dirindex.o textbox.o analysis.o generator.o prime.o commenting.o cleanup.o mainshow.o handhold.o animation.o dirt.o sound.o optimize.o smanage.o client.o progress.o leveldb.o startup.o icon.o browse.o backgrounds.o escapex.o

# XXX
SDLUTILOBJECTS= ../cc-lib/sdl/sdlutil.o ../cc-lib/sdl/font.o
CCLIBOBJECTS= ../cc-lib/md5.o ../cc-lib/base64.o ../cc-lib/stb_image_write.o ../cc-lib/stb_image.o


# XXX reenable sound
DEBUG=-g
OPT=-O2
CPPFLAGS=${DEBUG} -DNOSOUND=1 -DWIN32 -Wall -Wno-format -Wno-unused-function -Wno-deprecated -Wno-sign-compare -I/usr/local/include -I/usr/include  -I. ${SDLINCLUDES} ${SDLNETINCLUDES}
CXXFLAGS= -std=c++14 ${OPT}

# -Wl,-rpath=. 
LDFLAGS =  -L. ${LINKSDL} ${LINKSDLNET} -m64 -Wl,--subsystem,console ${OPT}  -static

# -lSDL_mixer
# at end  -lvorbisfile -lvorbis -logg
# LDLIBS = -lSDL_net  `sdl-config --libs` -lz
#  LDLIBS = -lefence

# XXX need to specify sdl flags presumably
# CPPFLAGS = -DNOSOUND=1 `sdl-config --cflags` 

# windres is part of TDM distribution
icon.o : escape.ico escape.rc
	windres -i escape.rc -o $@

escape.exe : ${ESCOBJECTS} ${CCLIBOBJECTS} ${SDLUTILOBJECTS} ${SDLOBJECTS} ${SDLNETOBJECTS}
	${CXX} -g -o $@ $^ ${LDFLAGS}

replace.exe : replace.o util.o
	${CXX} -g -o $@ $^ ${LDFLAGS}

packsound.exe : util-console.o packsound-console.o ${SDLOBJECTS}
	${CXX} -o $@ $^ ${LDFLAGS}
	strip packsound.exe

sound.o : sound.cc sound_load.h
	${CXX} ${CXXFLAGS} ${CPPFLAGS} -c -o $@ $<

level.o : level.cc move.h
	${CXX} ${CXXFLAGS} ${CPPFLAGS} -c -o $@ $<

animation.o : animation.cc animation.h animation_load.h
	${CXX} ${CXXFLAGS} ${CPPFLAGS} -c -o $@ $<

leveldb.o : leveldb.cc leveldb.h leveldb-query.h
	${CXX} ${CXXFLAGS} ${CPPFLAGS} -c -o $@ $<

sound_load.h : sound.pack packsound.cc
	make packsound.exe
	./packsound.exe sound.pack sound

ZOPFLI=./zopflipng.exe --splitting=3 --filters=01234mepb --lossy_8bit --lossy_transparent

animation_load.h animation.png animation_decs.h animation_defs.h : animation.pack
	rm -f animation_load.h animation.png animation_decs.h animation_defs.h
	make packpng.exe
	packpng.exe animation.pack animation
	rm -f animation-zop.png
	${ZOPFLI} animation.png animation-zop.png
	rm -f animation.png
	mv animation-zop.png animation.png

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

# XXX better way to do this? 
%-console.o : %.cc
	${CXX} ${CXXFLAGS} ${CPPFLAGS} -DNOSOUND=1 -DUSE_DISPLAY_FORMAT=0 $^ -c -o $@

packpng.exe : util-console.o font-console.o level-console.o ../cc-lib/sdl/sdlutil-console.o packpng-console.o winmain-console.o
	${CXX} -o $@ ${LDFLAGS} $^ ${LDLIBS}

# but this should be static
screenshot.exe : draw-console.o level-console.o ../cc-lib/sdl/font-console.o util-console.o ../cc-lib/sdl/sdlutil-console.o screenshot-console.o animation-console.o sound-console.o message-console.o drawable-console.o progress-console.o escapex-console.o
	${CXX} -g -o screenshot.exe $^ `sdl-config --static-libs`

.dummy :

version : .dummy
	${CXX} version.cc -o version.exe
	./version.exe
	make clean
	make -j 8 escape.exe

# SDL.dll SDL_net.dll zlib1.dll SDL_mixer.dll
RELEASEFILES=escape.exe replace.exe font.png fontsmall.png tiles.png tileutil.png title.png icon.png escape.txt COPYING changelog animation.png splash.png

releasefiles.win32 : ${RELEASEFILES}
	echo ${RELEASEFILES} > releasefiles.win32

mkupgrade.exe : mkupgrade.cc ../cc-lib/md5.cc
	${CXX} ${CPPFLAGS} ${CXXFLAGS} -DWIN32 mkupgrade.cc ../cc-lib/md5.cc -o mkupgrade.exe

CURRENT : .dummy escape.exe releasefiles.win32 symlinks.win32 deletefiles.win32 mkupgrade.exe
	strip escape.exe
	./mkupgrade.exe releasefiles.win32 symlinks.win32 deletefiles.win32 > UPGRADE
	./mkupgrade.exe -v > CURRENT

UPGRADE: CURRENT

release : UPGRADE
	echo "Note, use of pscp and scp don't seem to work when making under mingw,"
	echo "so you want to use cygwin bash for this."
	pscp -C ${RELEASEFILES} UPGRADE CURRENT root@spacebar.org:/var/www/escape/win32/
	rm -f UPGRADE CURRENT mkupgrade.exe

zip : escape-beta.tar.gz

zipup : escape-beta.tar.gz
	echo "Note, use of pscp and scp don't seem to work when making under mingw,"
	echo "so you want to use cygwin bash for this."
	pscp escape-beta.tar.gz root@spacebar.org:/var/www/escape/

# default location
MAKENSIS='c:\program files (x86)\nsis\makensis.exe'

escapesetup.exe : .dummy ${RELEASEFILES} escape.nsi
	${MAKENSIS} escape.nsi

installer : escapesetup.exe

installerup : escapesetup.exe
	echo "Note, use of pscp and scp don't seem to work when making under mingw,"
	echo "so you want to use cygwin bash for this."
	pscp escapesetup.exe root@spacebar.org:/var/www/escape/

source : escape-src.tar.bz2

# 	cd /usr0/src/escape-src && find . -name CVS > /usr0/src/escape-src/.tarignore
#       this includes binaries for OSX...
escape-src.tar.bz2 :
	cd /usr0/src/escape-src && cvs up -dP
	rm -rf /usr0/src/escape-src/web /usr0/src/escape-src/*.es{p,d}
	cd /usr0/src/escape-src && chmod a-x `find . -name "*.h"` `find . -name "*.cc"` `find . -name "*.pack"` `find . -name "*.esx"` `find . -name "*.m"`
	cd /usr0/src && tar -c escape-src --exclude "*CVS*" --exclude "*.svn*" --exclude "*.cvsignore" > /tmp/escape-src.tar
	cd /tmp && bzip2 escape-src.tar
	mv /tmp/escape-src.tar.bz2 .

# XXX two copies.. better if we could make src.tar be a symlink to the versioned tar.
sourceup : source CURRENT
	echo "Note, use of pscp and scp don't seem to work when making under mingw,"
	echo "so you want to use cygwin bash for this."
	pscp escape-src.tar.bz2 root@spacebar.org:/var/www/escape/source/
	pscp escape-src.tar.bz2 root@spacebar.org:/var/www/escape/source/escape-src-`cat CURRENT`.tar.bz2


wc :
	wc -l *.cc *.h

clean :
	rm -f *.o escape.exe core core.[0-9]* gmon.out escape-src.tar.bz2 escape-beta.tar.gz vgcore.* SDL_net/*.o

# end mingw makefile
endif
