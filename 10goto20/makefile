default: all

all: 10goto20.exe test
test: interval-tree_test.exe interval-cover_test.exe
	./interval-tree_test.exe --gtest_print_time=0
	./interval-cover_test.exe --gtest_print_time=0

CXXFLAGS=-Wall -fno-strict-aliasing -Wno-deprecated -Wno-sign-compare -I/usr/local/include --std=c++17
OPT=-O2

# for 64 bits on windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-g++
# CC=g++
# CXX=g++

# -Wl,--subsystem,console
DEBUGGING=-DDEBUG=1 -g -fno-omit-frame-pointer
# XXX OPT mode with fomit-frame-pointer.
CCSDL= -I SDL/include -I SDL_net

CC_LIB=../cc-lib

# old: (gcc 4.5) -mno-cygwin
LINKSDL= -lm -mthreads -luser32 -lgdi32 -lwinmm -ldxguid -lpthread
SDLOPATH=SDL/build
SDL_OBJECTS=$(SDLOPATH)/SDL.o $(SDLOPATH)/SDL_error.o $(SDLOPATH)/SDL_fatal.o $(SDLOPATH)/SDL_audio.o $(SDLOPATH)/SDL_audiocvt.o $(SDLOPATH)/SDL_audiodev.o $(SDLOPATH)/SDL_mixer.o $(SDLOPATH)/SDL_mixer_MMX.o $(SDLOPATH)/SDL_mixer_MMX_VC.o $(SDLOPATH)/SDL_mixer_m68k.o $(SDLOPATH)/SDL_wave.o $(SDLOPATH)/SDL_cdrom.o $(SDLOPATH)/SDL_cpuinfo.o $(SDLOPATH)/SDL_active.o $(SDLOPATH)/SDL_events.o $(SDLOPATH)/SDL_expose.o $(SDLOPATH)/SDL_keyboard.o $(SDLOPATH)/SDL_mouse.o $(SDLOPATH)/SDL_quit.o $(SDLOPATH)/SDL_resize.o $(SDLOPATH)/SDL_rwops.o $(SDLOPATH)/SDL_getenv.o $(SDLOPATH)/SDL_iconv.o $(SDLOPATH)/SDL_malloc.o $(SDLOPATH)/SDL_qsort.o $(SDLOPATH)/SDL_stdlib.o $(SDLOPATH)/SDL_string.o $(SDLOPATH)/SDL_thread.o $(SDLOPATH)/SDL_timer.o $(SDLOPATH)/SDL_RLEaccel.o $(SDLOPATH)/SDL_blit.o $(SDLOPATH)/SDL_blit_0.o $(SDLOPATH)/SDL_blit_1.o $(SDLOPATH)/SDL_blit_A.o $(SDLOPATH)/SDL_blit_N.o $(SDLOPATH)/SDL_bmp.o $(SDLOPATH)/SDL_cursor.o $(SDLOPATH)/SDL_gamma.o $(SDLOPATH)/SDL_pixels.o $(SDLOPATH)/SDL_stretch.o $(SDLOPATH)/SDL_surface.o $(SDLOPATH)/SDL_video.o $(SDLOPATH)/SDL_yuv.o $(SDLOPATH)/SDL_yuv_mmx.o $(SDLOPATH)/SDL_yuv_sw.o $(SDLOPATH)/SDL_joystick.o $(SDLOPATH)/SDL_nullevents.o $(SDLOPATH)/SDL_nullmouse.o $(SDLOPATH)/SDL_nullvideo.o $(SDLOPATH)/SDL_diskaudio.o $(SDLOPATH)/SDL_dummyaudio.o $(SDLOPATH)/SDL_sysevents.o $(SDLOPATH)/SDL_sysmouse.o $(SDLOPATH)/SDL_syswm.o $(SDLOPATH)/SDL_wingl.o $(SDLOPATH)/SDL_dibevents.o $(SDLOPATH)/SDL_dibvideo.o $(SDLOPATH)/SDL_dx5events.o $(SDLOPATH)/SDL_dx5video.o $(SDLOPATH)/SDL_dx5yuv.o $(SDLOPATH)/SDL_dibaudio.o $(SDLOPATH)/SDL_dx5audio.o $(SDLOPATH)/SDL_mmjoystick.o $(SDLOPATH)/SDL_syscdrom.o $(SDLOPATH)/SDL_sysmutex.o $(SDLOPATH)/SDL_syssem.o $(SDLOPATH)/SDL_systhread.o $(SDLOPATH)/SDL_syscond.o $(SDLOPATH)/SDL_systimer.o $(SDLOPATH)/SDL_sysloadso.o sdl_win32_main.o

JDKSPATH=$(CC_LIB)/jdksmidi
JDKSMIDI_OBJECTS=$(JDKSPATH)/jdksmidi_advancedsequencer.o $(JDKSPATH)/jdksmidi_driver.o $(JDKSPATH)/jdksmidi_driverdump.o $(JDKSPATH)/jdksmidi_edittrack.o $(JDKSPATH)/jdksmidi_file.o $(JDKSPATH)/jdksmidi_fileread.o $(JDKSPATH)/jdksmidi_filereadmultitrack.o $(JDKSPATH)/jdksmidi_fileshow.o $(JDKSPATH)/jdksmidi_filewrite.o $(JDKSPATH)/jdksmidi_filewritemultitrack.o $(JDKSPATH)/jdksmidi_keysig.o $(JDKSPATH)/jdksmidi_manager.o $(JDKSPATH)/jdksmidi_matrix.o $(JDKSPATH)/jdksmidi_midi.o $(JDKSPATH)/jdksmidi_msg.o $(JDKSPATH)/jdksmidi_multitrack.o $(JDKSPATH)/jdksmidi_parser.o $(JDKSPATH)/jdksmidi_process.o $(JDKSPATH)/jdksmidi_queue.o $(JDKSPATH)/jdksmidi_sequencer.o $(JDKSPATH)/jdksmidi_showcontrol.o $(JDKSPATH)/jdksmidi_showcontrolhandler.o $(JDKSPATH)/jdksmidi_smpte.o $(JDKSPATH)/jdksmidi_sysex.o $(JDKSPATH)/jdksmidi_tempo.o $(JDKSPATH)/jdksmidi_tick.o $(JDKSPATH)/jdksmidi_track.o $(JDKSPATH)/jdksmidi_utils.o 

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPT) -c -o $@ $<
	@bash -c "echo -n '.'"

../cc-lib/%.o : ../cc-lib/%.cc
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OPT) -c -o $@ $<
	@bash -c "echo -n '_'"

# PROFILE=-pg
PROFILE=

# enable link time optimizations?
# FLTO=-flto
FLTO=

INCLUDES=-I "$(CC_LIB)"

# MINGW_ANSI_STDIO suppresses warnings about %lld by using ANSI printf
# (maybe its own internal implementation). LGTM.
CPPFLAGS= $(DEBUGGING) -D__USE_MINGW_ANSI_STDIO=1 $(CCSDL) -DPSS_STYLE=1 -DDUMMY_UI -DHAVE_ASPRINTF -Wno-write-strings -m64 $(OPT) -D__MINGW32__ -DHAVE_ALLOCA -DNOWINSTUFF $(INCLUDES) $(PROFILE) $(FLTO)

CCLIB_OBJECTS=$(CC_LIB)/util.o $(CC_LIB)/arcfour.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/city/city.o $(CC_LIB)/textsvg.o $(CC_LIB)/stb_image.o $(CC_LIB)/wavesave.o $(CC_LIB)/stb_image_write.o

CCLIBSDL_OBJECTS=$(CC_LIB)/sdl/sdlutil.o $(CC_LIB)/sdl/font.o

BASE_OBJECTS=$(CCLIB_OBJECTS)

GTEST_OBJECTS=$(CC_LIB)/gtest/gtest-all.o

OBJECTS=$(BASE_OBJECTS) $(SDL_OBJECTS) $(CCLIBSDL_OBJECTS) $(JDKSMIDI_OBJECTS) audio-engine.o bleep-bloop-sample-layer.o controllers.o midi-music-layer.o mix-layer.o play-music-layer.o revision.o logging.o interval-cover.o

# without static, can't find lz or lstdcxx maybe?
LFLAGS=$(LINKSDL)  -m64 -Wl,--subsystem,console -lz $(OPT) $(FLTO) $(PROFILE) -static
# -Wl,--subsystem,console
# -static -fwhole-program

10goto20.exe : $(OBJECTS) 10goto20.o
	$(CXX) $^ -o $@ $(LFLAGS)

interval-tree_test.exe : $(CCLIB_OBJECTS) $(GTEST_OBJECTS) interval-tree_test.o
	$(CXX) $^ -o $@ $(OPT) -static

interval-cover_test.exe : $(CCLIB_OBJECTS) $(GTEST_OBJECTS) interval-cover_test.o
	$(CXX) $^ -o $@ $(OPT) -static

clean :
	rm -f *.o
