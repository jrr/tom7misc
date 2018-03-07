default: findxor.exe

CXXFLAGS=-O3 --std=c++14

findxor.exe : findxor.o ../cc-lib/arcfour.o
	$(CXX) -o $@ $^ -O3
