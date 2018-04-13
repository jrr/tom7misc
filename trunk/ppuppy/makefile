all : ppuppy.exe

CXX=g++
# consider O2 for releases...
CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O2 --std=c++14 -I ../cc-lib
LFLAGS=

%.o : %.cc *.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
#	bash -c "echo -n '.'"

%.s : %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -S -c -o $@ $<

# ../cc-lib/util.o  ../cc-lib/base/stringprintf.o ../cc-lib/arcfour.o
CCLIB_OBJECTS= ../cc-lib/base/logging.o 

ppuppy.exe : ppuppy.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

gpio.exe : gpio.o
	$(CXX) $^ -o $@ $(LFLAGS)

clear.exe : clear.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

set.exe : set.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

read.exe : read.o bcm2835.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe core $(CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
