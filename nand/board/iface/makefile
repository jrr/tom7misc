all : testnand.exe

CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O2 --std=c++14 -I ../../../cc-lib  -DPSS_STYLE=1 -I ../../../cc-lib/pi
# XXX for windows
LFLAGS=-static -lpthread

%.o : %.cc *.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
#	bash -c "echo -n '.'"

%.s : %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -S -c -o $@ $<

CCLIB_OBJECTS= ../../../cc-lib/base/logging.o ../../../cc-lib/stb_image.o  ../../../cc-lib/arcfour.o ../../../cc-lib/util.o ../../../cc-lib/color-util.o ../../../cc-lib/base/stringprintf.o
ALL_CCLIB_OBJECTS=$(CCLIB_OBJECTS) ../../../cc-lib/stb_image_write.o ../../../cc-lib/image.o

CCLIB_PI_OBJECTS=../../../cc-lib/pi/bcm2835.o

testnand.exe : testnand.o $(CCLIB_PI_OBJECTS) $(CCLIB_OBJECTS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe core $(ALL_CCLIB_OBJECTS) $(CCLIB_PI_OBJECTS) $(FCEULIB_OBJECTS)

veryclean : clean
	rm -f *~
