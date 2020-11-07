
default: drawfft.exe

all: drawfft.exe

# -fno-strict-aliasing
CXXFLAGS=-Wall -Wno-format -Wno-unused-function -Wno-deprecated -Wno-sign-compare -I/usr/local/include -I/usr/include -ISDL/include -I. -I../../cc-lib -I../../cc-lib/re2 -std=c++17
OPT=-g -O2

CCLIB=../cc-lib

# for 64 bits on windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

# SDLOPATH=SDL/build
# SDLOPATH=c:/code/SDL/build
SDLSDK=c:/code/SDL
SDLOPATH=$(SDLSDK)/build
# SDLINCLUDES=-ISDL/include
SDLINCLUDES=-I$(SDLSDK)/include

SDLARCH=x64
SDLUTILOBJECTS=$(CCLIB)/sdl/sdlutil.o $(CCLIB)/sdl/font.o $(CCLIB)/sdl/cursor.o
SDLOBJECTS=$(SDLOPATH)/SDL.o $(SDLOPATH)/SDL_error.o $(SDLOPATH)/SDL_fatal.o $(SDLOPATH)/SDL_audio.o $(SDLOPATH)/SDL_audiocvt.o $(SDLOPATH)/SDL_audiodev.o $(SDLOPATH)/SDL_mixer.o $(SDLOPATH)/SDL_mixer_MMX.o $(SDLOPATH)/SDL_mixer_MMX_VC.o $(SDLOPATH)/SDL_mixer_m68k.o $(SDLOPATH)/SDL_wave.o $(SDLOPATH)/SDL_cdrom.o $(SDLOPATH)/SDL_cpuinfo.o $(SDLOPATH)/SDL_active.o $(SDLOPATH)/SDL_events.o $(SDLOPATH)/SDL_expose.o $(SDLOPATH)/SDL_keyboard.o $(SDLOPATH)/SDL_mouse.o $(SDLOPATH)/SDL_quit.o $(SDLOPATH)/SDL_resize.o $(SDLOPATH)/SDL_rwops.o $(SDLOPATH)/SDL_getenv.o $(SDLOPATH)/SDL_iconv.o $(SDLOPATH)/SDL_malloc.o $(SDLOPATH)/SDL_qsort.o $(SDLOPATH)/SDL_stdlib.o $(SDLOPATH)/SDL_string.o $(SDLOPATH)/SDL_thread.o $(SDLOPATH)/SDL_timer.o $(SDLOPATH)/SDL_RLEaccel.o $(SDLOPATH)/SDL_blit.o $(SDLOPATH)/SDL_blit_0.o $(SDLOPATH)/SDL_blit_1.o $(SDLOPATH)/SDL_blit_A.o $(SDLOPATH)/SDL_blit_N.o $(SDLOPATH)/SDL_bmp.o $(SDLOPATH)/SDL_cursor.o $(SDLOPATH)/SDL_gamma.o $(SDLOPATH)/SDL_pixels.o $(SDLOPATH)/SDL_stretch.o $(SDLOPATH)/SDL_surface.o $(SDLOPATH)/SDL_video.o $(SDLOPATH)/SDL_yuv.o $(SDLOPATH)/SDL_yuv_mmx.o $(SDLOPATH)/SDL_yuv_sw.o $(SDLOPATH)/SDL_joystick.o $(SDLOPATH)/SDL_nullevents.o $(SDLOPATH)/SDL_nullmouse.o $(SDLOPATH)/SDL_nullvideo.o $(SDLOPATH)/SDL_diskaudio.o $(SDLOPATH)/SDL_dummyaudio.o $(SDLOPATH)/SDL_sysevents.o $(SDLOPATH)/SDL_sysmouse.o $(SDLOPATH)/SDL_syswm.o $(SDLOPATH)/SDL_wingl.o $(SDLOPATH)/SDL_dibevents.o $(SDLOPATH)/SDL_dibvideo.o $(SDLOPATH)/SDL_dx5events.o $(SDLOPATH)/SDL_dx5video.o $(SDLOPATH)/SDL_dx5yuv.o $(SDLOPATH)/SDL_dibaudio.o $(SDLOPATH)/SDL_dx5audio.o $(SDLOPATH)/SDL_mmjoystick.o $(SDLOPATH)/SDL_syscdrom.o $(SDLOPATH)/SDL_sysmutex.o $(SDLOPATH)/SDL_syssem.o $(SDLOPATH)/SDL_systhread.o $(SDLOPATH)/SDL_syscond.o $(SDLOPATH)/SDL_systimer.o $(SDLOPATH)/SDL_sysloadso.o $(SDLUTILOBJECTS) sdl-win32-main.o

LINKSDL=  -lm -mthreads -luser32 -lgdi32 -lwinmm -ldxguid

CCLIB_OBJECTS=$(CCLIB)/util.o $(CCLIB)/arcfour.o $(CCLIB)/base/stringprintf.o $(CCLIB)/base/logging.o $(CCLIB)/stb_image.o $(CCLIB)/stb_image_write.o $(CCLIB)/stb_truetype.o $(CCLIB)/color-util.o $(CCLIB)/image.o $(CCLIB)/crypt/sha256.o

CPPFLAGS= -DDISABLE_SOUND=1 -DPSS_STYLE=1 -DDUMMY_UI -DHAVE_ASPRINTF -Wno-write-strings -m64 $(OPT) -D__MINGW32__ -DHAVE_ALLOCA -DNOWINSTUFF $(SDLINCLUDES) $(PROFILE) $(FLTO) -I ../cc-lib/ --std=c++17

%.o : %.cc *.h makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

OBJECTS=$(CCLIB_OBJECTS)

# without static, can't find lz or lstdcxx maybe?
LFLAGS= -L. $(LINKSDL) -march=native -m64 -Wl,--subsystem,console -lz $(OPT) $(FLTO) $(PROFILE) -static

drawfft.exe : $(OBJECTS) $(SDLOBJECTS) drawfft.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

clean :
	rm -f *.o $(CCLIB_OBJECTS) *.exe
