
# Note, this weird makefile must be called Makefile
# and works by deferring through another one (ships with linux headers).
# This seems to be the idiomatic way to do it.

obj-m += flags.o

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

