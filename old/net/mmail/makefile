all : mmail deldups nmail

CC = g++
CXX = g++
CPP = g++
CPPFLAGS = -Wall -O2
CXXFLAGS = -Wall -O2

mmail : mmail.o
	g++ -o $@ $^

deldups : deldups.o
	g++ -o $@ $^


nmail : nmail.cm nmail.sml
	mlton nmail.cm
