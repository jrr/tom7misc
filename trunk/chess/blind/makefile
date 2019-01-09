
default: unblind.exe eval-unblinder.exe

all: unblind.exe eval-unblinder.exe vacuum.exe

# -fno-strict-aliasing
CXXFLAGS=-Wall -Wno-format -Wno-unused-function -Wno-deprecated -Wno-sign-compare -I/usr/local/include -I/usr/include -ISDL/include -I. -I../../cc-lib -I../../cc-lib/re2 -std=c++17
OPT=-O2

# for 64 bits on windows

# CXX=g++
# CC=gcc

CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

# Note, this builds against AMD's distribution of OpenCL, since that's
# the card I had when I started. It also works fine with an NVIDIA GPU,
# and nvidia libs are a bit hard to find since they love CUDA.
AMD_ARCH=x86_64
# AMDSDK="/c/Program Files (x86)/AMD APP SDK"
# mingw wants /c/, but TDM likes c:/
# AMDSDK=c:/Program Files (x86)/AMD APP SDK/3.0
AMDSDK=c:/code/AMD_SDK/

# SDLOPATH=SDL/build
# SDLOPATH=c:/code/SDL/build
SDLSDK=c:/code/SDL
SDLOPATH=$(SDLSDK)/build
# SDLINCLUDES=-ISDL/include
SDLINCLUDES=-I$(SDLSDK)/include

SDLARCH=x64
SDLUTILOBJECTS=../../cc-lib/sdl/sdlutil.o ../../cc-lib/sdl/font.o
SDLOBJECTS=$(SDLOPATH)/SDL.o $(SDLOPATH)/SDL_error.o $(SDLOPATH)/SDL_fatal.o $(SDLOPATH)/SDL_audio.o $(SDLOPATH)/SDL_audiocvt.o $(SDLOPATH)/SDL_audiodev.o $(SDLOPATH)/SDL_mixer.o $(SDLOPATH)/SDL_mixer_MMX.o $(SDLOPATH)/SDL_mixer_MMX_VC.o $(SDLOPATH)/SDL_mixer_m68k.o $(SDLOPATH)/SDL_wave.o $(SDLOPATH)/SDL_cdrom.o $(SDLOPATH)/SDL_cpuinfo.o $(SDLOPATH)/SDL_active.o $(SDLOPATH)/SDL_events.o $(SDLOPATH)/SDL_expose.o $(SDLOPATH)/SDL_keyboard.o $(SDLOPATH)/SDL_mouse.o $(SDLOPATH)/SDL_quit.o $(SDLOPATH)/SDL_resize.o $(SDLOPATH)/SDL_rwops.o $(SDLOPATH)/SDL_getenv.o $(SDLOPATH)/SDL_iconv.o $(SDLOPATH)/SDL_malloc.o $(SDLOPATH)/SDL_qsort.o $(SDLOPATH)/SDL_stdlib.o $(SDLOPATH)/SDL_string.o $(SDLOPATH)/SDL_thread.o $(SDLOPATH)/SDL_timer.o $(SDLOPATH)/SDL_RLEaccel.o $(SDLOPATH)/SDL_blit.o $(SDLOPATH)/SDL_blit_0.o $(SDLOPATH)/SDL_blit_1.o $(SDLOPATH)/SDL_blit_A.o $(SDLOPATH)/SDL_blit_N.o $(SDLOPATH)/SDL_bmp.o $(SDLOPATH)/SDL_cursor.o $(SDLOPATH)/SDL_gamma.o $(SDLOPATH)/SDL_pixels.o $(SDLOPATH)/SDL_stretch.o $(SDLOPATH)/SDL_surface.o $(SDLOPATH)/SDL_video.o $(SDLOPATH)/SDL_yuv.o $(SDLOPATH)/SDL_yuv_mmx.o $(SDLOPATH)/SDL_yuv_sw.o $(SDLOPATH)/SDL_joystick.o $(SDLOPATH)/SDL_nullevents.o $(SDLOPATH)/SDL_nullmouse.o $(SDLOPATH)/SDL_nullvideo.o $(SDLOPATH)/SDL_diskaudio.o $(SDLOPATH)/SDL_dummyaudio.o $(SDLOPATH)/SDL_sysevents.o $(SDLOPATH)/SDL_sysmouse.o $(SDLOPATH)/SDL_syswm.o $(SDLOPATH)/SDL_wingl.o $(SDLOPATH)/SDL_dibevents.o $(SDLOPATH)/SDL_dibvideo.o $(SDLOPATH)/SDL_dx5events.o $(SDLOPATH)/SDL_dx5video.o $(SDLOPATH)/SDL_dx5yuv.o $(SDLOPATH)/SDL_dibaudio.o $(SDLOPATH)/SDL_dx5audio.o $(SDLOPATH)/SDL_mmjoystick.o $(SDLOPATH)/SDL_syscdrom.o $(SDLOPATH)/SDL_sysmutex.o $(SDLOPATH)/SDL_syssem.o $(SDLOPATH)/SDL_systhread.o $(SDLOPATH)/SDL_syscond.o $(SDLOPATH)/SDL_systimer.o $(SDLOPATH)/SDL_sysloadso.o $(SDLUTILOBJECTS) sdl-win32-main.o

LINKSDL=  -lm -mthreads -luser32 -lgdi32 -lwinmm -ldxguid

CLINCLUDES="-I$(AMDSDK)/include"
CLLIBS='-L${AMDSDK}/lib/${AMD_ARCH}'

RE2_OBJECTS=../../cc-lib/re2/bitstate.o ../../cc-lib/re2/compile.o ../../cc-lib/re2/dfa.o ../../cc-lib/re2/filtered_re2.o ../../cc-lib/re2/mimics_pcre.o ../../cc-lib/re2/nfa.o ../../cc-lib/re2/onepass.o ../../cc-lib/re2/parse.o ../../cc-lib/re2/perl_groups.o ../../cc-lib/re2/prefilter.o ../../cc-lib/re2/prefilter_tree.o ../../cc-lib/re2/prog.o ../../cc-lib/re2/re2.o ../../cc-lib/re2/regexp.o ../../cc-lib/re2/set.o ../../cc-lib/re2/simplify.o ../../cc-lib/re2/stringpiece.o ../../cc-lib/re2/tostring.o ../../cc-lib/re2/unicode_casefold.o ../../cc-lib/re2/unicode_groups.o ../../cc-lib/re2/util/rune.o ../../cc-lib/re2/util/strutil.o

UTIL_OBJECTS=../../cc-lib/util.o ../../cc-lib/arcfour.o ../../cc-lib/base/stringprintf.o ../../cc-lib/base/logging.o ../../cc-lib/stb_image.o ../../cc-lib/stb_image_write.o ../../cc-lib/stb_truetype.o ../../cc-lib/color-util.o ../../cc-lib/image.o

CHESS_OBJECTS=../pgn.o ../chess.o

CPPFLAGS= -DDISABLE_SOUND=1 -DPSS_STYLE=1 -DDUMMY_UI -DHAVE_ASPRINTF -Wno-write-strings -m64 $(OPT) -D__MINGW32__ -DHAVE_ALLOCA -DNOWINSTUFF $(SDLINCLUDES) $(PROFILE) $(FLTO) $(CLINCLUDES) -I ../cc-lib/ --std=c++14

%.o : %.cc *.h makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

OBJECTS=$(UTIL_OBJECTS) $(CHESS_OBJECTS) $(RE2_OBJECTS) unblinder.o unblinder-mk0.o loadpositions.o network.o

# without static, can't find lz or lstdcxx maybe?
LFLAGS= -L. $(LINKSDL) -m64 -Wl,--subsystem,console $(CLLIBS) -lz -lOpenCL $(OPT) $(FLTO) $(PROFILE) -static

eval-unblinder.exe : $(OBJECTS) eval-unblinder.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

ui-unblinder.exe : $(OBJECTS) $(SDLOBJECTS) ui-unblinder.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

unblind.exe : $(OBJECTS) $(SDLOBJECTS) clutil.o unblind.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

vacuum.exe : $(OBJECTS) vacuum.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

clean :
	rm -f *.o $(UTIL_OBJECTS) $(CHESS_OBJECTS) $(RE2_OBJECTS) *.exe
