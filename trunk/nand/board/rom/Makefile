# Makefile for STM32F30x
# Based on f303_blinky Makefile by E. Brombaugh

default: main.bin

# Object files
OBJECTS = 	startup_stm32f30x.o system_stm32f30x.o main.o \
			stm32f30x_gpio.o stm32f30x_misc.o stm32f30x_rcc.o 

# Linker script
LDSCRIPT = STM32_FLASH.ld

CFLAGS = -g -Os -mlittle-endian -mthumb
CFLAGS +=  -I. -DARM_MATH_CM4 -D'__FPU_PRESENT=1' -D'__FPU_USED=1' -DUSE_STDPERIPH_DRIVER
CFLAGS += -mcpu=cortex-m4 -mfloat-abi=hard
AFLAGS  = -mlittle-endian -mthumb -mcpu=cortex-m4 
LFLAGS  = -Map main.map -nostartfiles -T $(LDSCRIPT)

# Executables
ARCH = arm-none-eabi
CC = $(ARCH)-gcc
LD = $(ARCH)-ld -v
AS = $(ARCH)-as
OBJCPY = $(ARCH)-objcopy
OBJDMP = $(ARCH)-objdump
GDB = $(ARCH)-gdb

#CPFLAGS = --output-target=binary -j .text -j .data
CPFLAGS = --output-target=binary
ODFLAGS	= -x --syms

# Targets
all: main.bin

clean:
	-rm -f $(OBJECTS) *.lst *.elf *.bin *.map *.dmp

disassemble: main.elf
	$(OBJDMP) -dS main.elf > main.dis

main.ihex: main.elf
	$(OBJCPY) --output-target=ihex main.elf main.ihex

main.bin: main.elf 
	$(OBJCPY) $(CPFLAGS) main.elf main.bin
	$(OBJDMP) $(ODFLAGS) main.elf > main.dmp
	ls -l main.elf main.bin

main.elf: $(OBJECTS) $(LDSCRIPT)
	$(LD) $(LFLAGS) -o main.elf $(OBJECTS)

startup_stm32f37x.o: startup_stm32f37x.s
	$(AS) $(AFLAGS) startup_stm32f37x.s -o startup_stm32f37x.o > startup_stm32f37x.lst

%.o: %.c %.h Makefile
	$(CC) $(CFLAGS) -c -o $@ $<

