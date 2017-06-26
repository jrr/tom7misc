CCFLAGS = -g -Wall -O
CC = gcc $(CCFLAGS)
TARGET = dmg.exe

default : $(TARGET)
$(TARGET) : dmg.o midi.o bass.o
	$(CC) -o $(TARGET) dmg.o midi.o bass.o -lstdcx
dmg.o : dmg.cc midi.h dmg.h bass.h
	$(CC) -o dmg.o dmg.cc -c
midi.o : midi.cc midi.h
	$(CC) -o midi.o midi.cc -c
bass.o : bass.cc bass.h dmg.h
	$(CC) -o bass.o bass.cc -c