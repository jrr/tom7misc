default : gproxy

CC=g++
CPP=g++
CPPFLAGS=-Wall -O -I../tsocket/

gproxy : ../tsocket/tsocket.o gproxy.o
	g++ $^ -o gproxy -O -Wall -s
