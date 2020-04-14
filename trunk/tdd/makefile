
default : tdd.exe

CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc

tdd.o : tdd.cc tdd.h
	$(CXX) -c --std=c++17 tdd.cc -O -o $@

tdd.exe : tdd.o
	$(CXX) --std=c++17 $^ -o $@

clean :
	rm -f *.o

veryclean : clean
	rm -f *.exe core *~
