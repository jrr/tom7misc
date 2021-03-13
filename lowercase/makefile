
default: train.exe collate-errors.exe vacuum.exe cull.exe widen.exe deepen.exe ui.exe

all: ui.exe cleandb.exe samecase.exe train.exe collate-errors.exe vacuum.exe widen.exe get-modelinfo.exe font-problem_test.exe top_test.exe vector-eval.exe sdf-eval.exe vector-info.exe sdf-info.exe area.exe get-layerweights.exe makefeatures.exe plot.exe bitmap-sdf.exe trace-sdf.exe vector-makefont.exe bitmap-makefont.exe makefont.exe hallucinate.exe sdf-figure.exe

modelinfos: modelinfo0.png modelinfo1.png
layerweights: weights0-layer0.png weights1-layer0.png

CC_LIB=../cc-lib

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

CXXFLAGS=-march=native -m64 -Wall -Wno-format -Wno-unused-function -Wno-deprecated -Wno-sign-compare -I/usr/local/include -I/usr/include $(SDLINCLUDES) -I. -I$(CC_LIB) -I$(CC_LIB)/re2 -std=c++17
OPT=-O2

SDLARCH=x64
SDLUTILOBJECTS=$(CC_LIB)/sdl/sdlutil.o $(CC_LIB)/sdl/cursor.o $(CC_LIB)/sdl/font.o
SDLOBJECTS=$(SDLOPATH)/SDL.o $(SDLOPATH)/SDL_error.o $(SDLOPATH)/SDL_fatal.o $(SDLOPATH)/SDL_audio.o $(SDLOPATH)/SDL_audiocvt.o $(SDLOPATH)/SDL_audiodev.o $(SDLOPATH)/SDL_mixer.o $(SDLOPATH)/SDL_mixer_MMX.o $(SDLOPATH)/SDL_mixer_MMX_VC.o $(SDLOPATH)/SDL_mixer_m68k.o $(SDLOPATH)/SDL_wave.o $(SDLOPATH)/SDL_cdrom.o $(SDLOPATH)/SDL_cpuinfo.o $(SDLOPATH)/SDL_active.o $(SDLOPATH)/SDL_events.o $(SDLOPATH)/SDL_expose.o $(SDLOPATH)/SDL_keyboard.o $(SDLOPATH)/SDL_mouse.o $(SDLOPATH)/SDL_quit.o $(SDLOPATH)/SDL_resize.o $(SDLOPATH)/SDL_rwops.o $(SDLOPATH)/SDL_getenv.o $(SDLOPATH)/SDL_iconv.o $(SDLOPATH)/SDL_malloc.o $(SDLOPATH)/SDL_qsort.o $(SDLOPATH)/SDL_stdlib.o $(SDLOPATH)/SDL_string.o $(SDLOPATH)/SDL_thread.o $(SDLOPATH)/SDL_timer.o $(SDLOPATH)/SDL_RLEaccel.o $(SDLOPATH)/SDL_blit.o $(SDLOPATH)/SDL_blit_0.o $(SDLOPATH)/SDL_blit_1.o $(SDLOPATH)/SDL_blit_A.o $(SDLOPATH)/SDL_blit_N.o $(SDLOPATH)/SDL_bmp.o $(SDLOPATH)/SDL_cursor.o $(SDLOPATH)/SDL_gamma.o $(SDLOPATH)/SDL_pixels.o $(SDLOPATH)/SDL_stretch.o $(SDLOPATH)/SDL_surface.o $(SDLOPATH)/SDL_video.o $(SDLOPATH)/SDL_yuv.o $(SDLOPATH)/SDL_yuv_mmx.o $(SDLOPATH)/SDL_yuv_sw.o $(SDLOPATH)/SDL_joystick.o $(SDLOPATH)/SDL_nullevents.o $(SDLOPATH)/SDL_nullmouse.o $(SDLOPATH)/SDL_nullvideo.o $(SDLOPATH)/SDL_diskaudio.o $(SDLOPATH)/SDL_dummyaudio.o $(SDLOPATH)/SDL_sysevents.o $(SDLOPATH)/SDL_sysmouse.o $(SDLOPATH)/SDL_syswm.o $(SDLOPATH)/SDL_wingl.o $(SDLOPATH)/SDL_dibevents.o $(SDLOPATH)/SDL_dibvideo.o $(SDLOPATH)/SDL_dx5events.o $(SDLOPATH)/SDL_dx5video.o $(SDLOPATH)/SDL_dx5yuv.o $(SDLOPATH)/SDL_dibaudio.o $(SDLOPATH)/SDL_dx5audio.o $(SDLOPATH)/SDL_mmjoystick.o $(SDLOPATH)/SDL_syscdrom.o $(SDLOPATH)/SDL_sysmutex.o $(SDLOPATH)/SDL_syssem.o $(SDLOPATH)/SDL_systhread.o $(SDLOPATH)/SDL_syscond.o $(SDLOPATH)/SDL_systimer.o $(SDLOPATH)/SDL_sysloadso.o $(SDLUTILOBJECTS) sdl-win32-main.o

LINKSDL=  -lm -mthreads -luser32 -lgdi32 -lwinmm -ldxguid

CLINCLUDES="-I$(AMDSDK)/include"
CLLIBS='-L${AMDSDK}/lib/${AMD_ARCH}'

RE2_OBJECTS=$(CC_LIB)/re2/bitstate.o $(CC_LIB)/re2/compile.o $(CC_LIB)/re2/dfa.o $(CC_LIB)/re2/filtered_re2.o $(CC_LIB)/re2/mimics_pcre.o $(CC_LIB)/re2/nfa.o $(CC_LIB)/re2/onepass.o $(CC_LIB)/re2/parse.o $(CC_LIB)/re2/perl_groups.o $(CC_LIB)/re2/prefilter.o $(CC_LIB)/re2/prefilter_tree.o $(CC_LIB)/re2/prog.o $(CC_LIB)/re2/re2.o $(CC_LIB)/re2/regexp.o $(CC_LIB)/re2/set.o $(CC_LIB)/re2/simplify.o $(CC_LIB)/re2/stringpiece.o $(CC_LIB)/re2/tostring.o $(CC_LIB)/re2/unicode_casefold.o $(CC_LIB)/re2/unicode_groups.o $(CC_LIB)/re2/util/rune.o $(CC_LIB)/re2/util/strutil.o

UTIL_OBJECTS=$(CC_LIB)/util.o $(CC_LIB)/arcfour.o $(CC_LIB)/base/stringprintf.o $(CC_LIB)/base/logging.o $(CC_LIB)/stb_image.o $(CC_LIB)/stb_image_write.o $(CC_LIB)/stb_truetype.o $(CC_LIB)/color-util.o $(CC_LIB)/image.o $(CC_LIB)/edit-distance.o $(CC_LIB)/city/city.o $(CC_LIB)/opt/opt.o

CPPFLAGS= -DDISABLE_SOUND=1 -DPSS_STYLE=1 -DDUMMY_UI -DHAVE_ASPRINTF -Wno-write-strings -m64 $(OPT) -D__MINGW32__ -DHAVE_ALLOCA -DNOWINSTUFF $(SDLINCLUDES) $(PROFILE) $(FLTO) $(CLINCLUDES) -I $(CC_LIB)/ --std=c++17

%.o : %.cc *.h makefile
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	@echo -n "."

OBJECTS=$(UTIL_OBJECTS) $(RE2_OBJECTS) ttf.o ttfops.o fontdb.o network.o font-problem.o error-history.o modelinfo.o bezier.o

# without static, can't find lz or lstdcxx maybe?
LFLAGS= -L. $(LINKSDL) -m64 -Wl,--subsystem,console $(CLLIBS) -lz -lOpenCL $(OPT) $(FLTO) $(PROFILE) -lpsapi -static

ui.exe : $(OBJECTS) $(SDLOBJECTS) ui.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

train.exe : $(OBJECTS) $(SDLOBJECTS) clutil.o train.o loadfonts.o top.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

makefeatures.exe : $(OBJECTS) makefeatures.o loadfonts.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

vacuum.exe : $(OBJECTS) vacuum.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

cull.exe : $(OBJECTS) cull.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

widen.exe : $(OBJECTS) widen.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

deepen.exe : $(OBJECTS) deepen.o
	@$(CXX) $^ -o $@ $(LFLAGS)
	@echo -n "!"

cleandb.exe : cleandb.o ttfarchive.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

samecase.exe : samecase.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

area.exe : area.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

vector-info.exe : vector-info.o $(OBJECTS) loadfonts.o
	$(CXX) $^ -o $@ $(LFLAGS)

sdf-info.exe : sdf-info.o $(OBJECTS) loadfonts.o
	$(CXX) $^ -o $@ $(LFLAGS)

vector-eval.exe : $(OBJECTS) vector-eval.o
	$(CXX) $^ -o $@ $(LFLAGS)

sdf-eval.exe : $(OBJECTS) sdf-eval.o
	$(CXX) $^ -o $@ $(LFLAGS)

get-modelinfo.exe : $(OBJECTS) get-modelinfo.o
	$(CXX) $^ -o $@ $(LFLAGS)

get-layerweights.exe : $(OBJECTS) get-layerweights.o
	$(CXX) $^ -o $@ $(LFLAGS)

plot.exe : $(OBJECTS) plot.o
	$(CXX) $^ -o $@ $(LFLAGS)

top_test.exe : top.o top_test.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

font-problem_test.exe : font-problem.o font-problem_test.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

ptxtest.exe : $(OBJECTS) ptxtest.o clutil.o
	$(CXX) $^ -o $@ $(LFLAGS)

modelinfo%.png : net%.val get-modelinfo.exe
	./get-modelinfo.exe $< $@

weights%-layer0.png : net%.val get-layerweights.exe
	./get-layerweights.exe $< weights$*

collate-errors.exe : collate-errors.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

bitmap-sdf.exe : bitmap-sdf.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

sdf-figure.exe : sdf-figure.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

trace-sdf.exe : trace-sdf.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

makefont.exe : makefont.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

vector-makefont.exe : vector-makefont.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

bitmap-makefont.exe : bitmap-makefont.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

hallucinate.exe : hallucinate.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o $(UTIL_OBJECTS) $(CHESS_OBJECTS) $(RE2_OBJECTS) *.exe
