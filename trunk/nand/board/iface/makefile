default : nandy-graph.exe
all : testnand.exe test-nandy.exe piface.exe test-nandy.exe


CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O2 --std=c++14 -DDISABLE_I2C -I ../../../cc-lib   -I ../../../cc-lib/pi

# XXX for windows
# PLATFORMCFLAGS= -D__MINGW32__ -DHAVE_ALLOCA  -D_WIN32_WINNT=0x0602 -D_GLIBCXX_HAS_GTHREADS -mthreads
# PLATFORMLINK=-Wl,--subsystem,console -L.
# CXX=x86_64-w64-mingw32-g++
# CC=x86_64-w64-mingw32-gcc


# CPPFLAGS=-m64 $(INCLUDES) $(PLATFORMCFLAGS) $(SYMBOLS)
# LFLAGS=-m64 -mthreads -lwinpthread -lpsapi -static $(PLATFORMLINK)

%.o : %.cc *.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
#	bash -c "echo -n '.'"

%.s : %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -S -c -o $@ $<

CCLIB_OBJECTS= ../../../cc-lib/base/logging.o ../../../cc-lib/stb_image.o  ../../../cc-lib/arcfour.o ../../../cc-lib/util.o ../../../cc-lib/color-util.o ../../../cc-lib/base/stringprintf.o
ALL_CCLIB_OBJECTS=$(CCLIB_OBJECTS) ../../../cc-lib/stb_image_write.o ../../../cc-lib/image.o ../../../cc-lib/crypt/md5.o

CCLIB_PI_OBJECTS=../../../cc-lib/pi/bcm2835.o

testnand.exe : testnand.o $(CCLIB_PI_OBJECTS) $(CCLIB_OBJECTS)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $^ -o $@ $(LFLAGS)

test-nandy.exe : test-nandy.o nandy.o $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

nandy-graph.exe : nandy-graph.o nandy.o $(ALL_CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

piface.exe : piface.o $(CCLIB_PI_OBJECTS) $(CCLIB_OBJECTS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LFLAGS)

nandyface.exe : nandyface.o nandy.o $(CCLIB_PI_OBJECTS) $(CCLIB_OBJECTS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LFLAGS)

nandwork.dot : nandy-graph.exe
	./nandy-graph.exe

clean :
	rm -f *.o *.exe core $(ALL_CCLIB_OBJECTS) $(CCLIB_PI_OBJECTS) $(FCEULIB_OBJECTS)

veryclean : clean
	rm -f *~
