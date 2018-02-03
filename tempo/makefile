all : tempo.exe

CXX=g++
CXXFLAGS=-Wall -O2 --std=c++14

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

CCLIB_OBJECTS=../cc-lib/util.o

tempo.exe : tempo.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe core $(CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
