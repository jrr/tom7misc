all : tempo.exe

CXX=g++
CXXFLAGS=-Wall -Wno-sign-compare -O --std=c++14 -I ../cc-lib

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o

tempo.exe : tempo.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe core $(CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
