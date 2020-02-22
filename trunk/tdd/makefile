
default : tdd.exe

tdd.o : tdd.cc tdd.h
	g++ -c --std=c++2a tdd.cc -O -o $@

tdd.exe : tdd.o
	g++ --std=c++2a $^ -o $@
