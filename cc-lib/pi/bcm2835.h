/* bcm2835.h
  
   C and C++ support for Broadcom BCM 2835 as used in Raspberry Pi
  
   Author: Mike McCauley
   Copyright (C) 2011-2013 Mike McCauley
   Modified by Tom 7 in 2018+.
   See README.bcm2835 for docs that I moved out of this header.   

   Note: I removed some functionality I didn't need (e.g. SPI)
   since it was pretty big.
   Better idea: Remove batches of functionality at compile time. Try:
     -DDISABLE_I2C
     -DDISABLE_SPI

   TODO: Merge back in other removed support, guarded by defines.
   TODO: Raspberry Pi 4 has different peripheral base address and
   maybe other differences. Merge them in from newer version of this
   lib.

   I also added "support" for the interrupt controller. You can
   manually disable or enable interrupts. Since there's no way to
   safely handle them through this interface, the most useful thing
   is to disable interrupts--but this is currently a one-way street.
*/

/* Defines for BCM2835 */
#ifndef _CC_LIB_PI_BCM2835_H
#define _CC_LIB_PI_BCM2835_H

#include <stdint.h>
#include <string.h>

#define BCM2835_VERSION 10055 /* Version 1.55 */

/* RPi 2 is ARM v7, and has DMB instruction for memory barriers. Older
   RPis are ARM v6 and don't, so a coprocessor instruction must be
   used instead. However, not all versions of gcc in all distros
   support the dmb assembler instruction even on conmpatible
   processors. This test is so any ARMv7 or higher processors with
   suitable GCC will use DMB. */
#if __ARM_ARCH >= 7
#define BCM2835_HAVE_DMB
#endif

/* \defgroup constants Constants for passing to and from library functions
  The values here are designed to be passed to various functions in the bcm2835 library.
*/

/* Speed of the core clock core_clk */
#define BCM2835_CORE_CLK_HZ		250000000	/* 250 MHz */

/* On RPi2 with BCM2836, and all recent OSs, the base of the peripherals is read from a /proc file */
#define BMC2835_RPI2_DT_FILENAME "/proc/device-tree/soc/ranges"
/* Offset into BMC2835_RPI2_DT_FILENAME for the peripherals base address */
#define BMC2835_RPI2_DT_PERI_BASE_ADDRESS_OFFSET 4
/* Offset into BMC2835_RPI2_DT_FILENAME for the peripherals size address */
#define BMC2835_RPI2_DT_PERI_SIZE_OFFSET 8

/* Physical addresses for various peripheral register sets
  Base Physical Address of the BCM 2835 peripheral registers
  Note this is different for the RPi2 BCM2836, where this is derived from /proc/device-tree/soc/ranges
  If /proc/device-tree/soc/ranges exists on a RPi 1 OS, it would be expected to contain the
  following numbers:
*/
/* Peripherals block base address on RPi 1 */
#define BCM2835_PERI_BASE               0x20000000
/* Size of the peripherals block on RPi 1 */
#define BCM2835_PERI_SIZE               0x01000000

// In Broadcom docs, GPIO is at "7E200000" and GPIO_BASE is 0x200000, so
// with INT at "0x7E00B000", we have
#define BCM2835_INT_BASE   0x00B000

#define BCM2835_IRQ_BASIC_PENDING 0x200
#define BCM2835_IRQ_PENDING_1 0x204
#define BCM2835_IRQ_PENDING_2 0x208
#define BCM2835_FIQ_CONTROL 0x20C
#define BCM2835_ENABLE_IRQ_1 0x210
#define BCM2835_ENABLE_IRQ_2 0x214
#define BCM2835_ENABLE_BASIC_IRQ 0x218
#define BCM2835_DISABLE_IRQ_1 0x21C
#define BCM2835_DISABLE_IRQ_2 0x220
#define BCM2835_DISABLE_BASIC_IRQ 0x224

/* Offsets for the bases of various peripherals within the peripherals block
  /   Base Address of the System Timer registers
*/
#define BCM2835_ST_BASE			0x3000
/* Base Address of the Pads registers */
#define BCM2835_GPIO_PADS               0x100000
/* Base Address of the Clock/timer registers */
#define BCM2835_CLOCK_BASE              0x101000
/* Base Address of the GPIO registers */
#define BCM2835_GPIO_BASE               0x200000
/* Base Address of the SPI0 registers */
#define BCM2835_SPI0_BASE               0x204000
/* Base Address of the BSC0 registers */
#define BCM2835_BSC0_BASE 		0x205000
/* Base Address of the PWM registers */
#define BCM2835_GPIO_PWM                0x20C000
/* Base Address of the AUX registers */
#define BCM2835_AUX_BASE		0x215000
/* Base Address of the AUX_SPI1 registers */
#define BCM2835_SPI1_BASE		0x215080
/* Base Address of the AUX_SPI2 registers */
#define BCM2835_SPI2_BASE		0x2150C0
/* Base Address of the BSC1 registers */
#define BCM2835_BSC1_BASE		0x804000


/* Physical address and size of the peripherals block
  May be overridden on RPi2
*/
extern uint32_t *bcm2835_peripherals_base;
/* Size of the peripherals block to be mapped */
extern uint32_t bcm2835_peripherals_size;

/* Virtual memory address of the mapped peripherals block */
extern uint32_t *bcm2835_peripherals;

/* Base of the ST (System Timer) registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_st;

/* Base of the GPIO registers.
  Available after bcm2835_init has been called
*/
extern volatile uint32_t *bcm2835_gpio;

extern volatile uint32_t *bcm2835_int;

/* Base of the PWM registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_pwm;

/* Base of the CLK registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_clk;

/* Base of the PADS registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_pads;

/* Base of the SPI0 registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_spi0;

/* Base of the BSC0 registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_bsc0;

/* Base of the BSC1 registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_bsc1;

/* Base of the AUX registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_aux;

/* Base of the SPI1 registers.
  Available after bcm2835_init has been called (as root)
*/
extern volatile uint32_t *bcm2835_spi1;


/* bcm2835RegisterBase
  Register bases for bcm2835_regbase()
*/
typedef enum
{
    BCM2835_REGBASE_ST   = 1, /* Base of the ST (System Timer) registers. */
    BCM2835_REGBASE_GPIO = 2, /* Base of the GPIO registers. */
    BCM2835_REGBASE_INT  = 11, /* Interrupt controller */
    BCM2835_REGBASE_PWM  = 3, /* Base of the PWM registers. */
    BCM2835_REGBASE_CLK  = 4, /* Base of the CLK registers. */
    BCM2835_REGBASE_PADS = 5, /* Base of the PADS registers. */
    BCM2835_REGBASE_SPI0 = 6, /* Base of the SPI0 registers. */
    BCM2835_REGBASE_BSC0 = 7, /* Base of the BSC0 registers. */
    BCM2835_REGBASE_BSC1 = 8,  /* Base of the BSC1 registers. */
    BCM2835_REGBASE_AUX  = 9,  /* Base of the AUX registers. */
    BCM2835_REGBASE_SPI1 = 10  /* Base of the SPI1 registers. */
} bcm2835RegisterBase;

/* Size of memory page on RPi */
#define BCM2835_PAGE_SIZE               (4*1024)
/* Size of memory block on RPi */
#define BCM2835_BLOCK_SIZE              (4*1024)


/* Defines for GPIO
   The BCM2835 has 54 GPIO pins.
   BCM2835 data sheet, Page 90 onwards.
*/
/* GPIO register offsets from BCM2835_GPIO_BASE. 
  Offsets into the GPIO Peripheral block in bytes per 6.1 Register View 
*/
#define BCM2835_GPFSEL0    0x0000 /* GPIO Function Select 0 */
#define BCM2835_GPFSEL1    0x0004 /* GPIO Function Select 1 */
#define BCM2835_GPFSEL2    0x0008 /* GPIO Function Select 2 */
#define BCM2835_GPFSEL3    0x000c /* GPIO Function Select 3 */
#define BCM2835_GPFSEL4    0x0010 /* GPIO Function Select 4 */
#define BCM2835_GPFSEL5    0x0014 /* GPIO Function Select 5 */
#define BCM2835_GPSET0     0x001c /* GPIO Pin Output Set 0 */
#define BCM2835_GPSET1     0x0020 /* GPIO Pin Output Set 1 */
#define BCM2835_GPCLR0     0x0028 /* GPIO Pin Output Clear 0 */
#define BCM2835_GPCLR1     0x002c /* GPIO Pin Output Clear 1 */
#define BCM2835_GPLEV0     0x0034 /* GPIO Pin Level 0 */
#define BCM2835_GPLEV1     0x0038 /* GPIO Pin Level 1 */
#define BCM2835_GPEDS0     0x0040 /* GPIO Pin Event Detect Status 0 */
#define BCM2835_GPEDS1     0x0044 /* GPIO Pin Event Detect Status 1 */
#define BCM2835_GPREN0     0x004c /* GPIO Pin Rising Edge Detect Enable 0 */
#define BCM2835_GPREN1     0x0050 /* GPIO Pin Rising Edge Detect Enable 1 */
#define BCM2835_GPFEN0     0x0058 /* GPIO Pin Falling Edge Detect Enable 0 */
#define BCM2835_GPFEN1     0x005c /* GPIO Pin Falling Edge Detect Enable 1 */
#define BCM2835_GPHEN0     0x0064 /* GPIO Pin High Detect Enable 0 */
#define BCM2835_GPHEN1     0x0068 /* GPIO Pin High Detect Enable 1 */
#define BCM2835_GPLEN0     0x0070 /* GPIO Pin Low Detect Enable 0 */
#define BCM2835_GPLEN1     0x0074 /* GPIO Pin Low Detect Enable 1 */
#define BCM2835_GPAREN0    0x007c /* GPIO Pin Async. Rising Edge Detect 0 */
#define BCM2835_GPAREN1    0x0080 /* GPIO Pin Async. Rising Edge Detect 1 */
#define BCM2835_GPAFEN0    0x0088 /* GPIO Pin Async. Falling Edge Detect 0 */
#define BCM2835_GPAFEN1    0x008c /* GPIO Pin Async. Falling Edge Detect 1 */
#define BCM2835_GPPUD      0x0094 /* GPIO Pin Pull-up/down Enable */
#define BCM2835_GPPUDCLK0  0x0098 /* GPIO Pin Pull-up/down Enable Clock 0 */
#define BCM2835_GPPUDCLK1  0x009c /* GPIO Pin Pull-up/down Enable Clock 1 */

/*   bcm2835PortFunction
  Port function select modes for bcm2835_gpio_fsel()
*/
typedef enum
{
    BCM2835_GPIO_FSEL_INPT  = 0x00,   /* Input 0b000 */
    BCM2835_GPIO_FSEL_OUTP  = 0x01,   /* Output 0b001 */
    BCM2835_GPIO_FSEL_ALT0  = 0x04,   /* Alternate function 0 0b100 */
    BCM2835_GPIO_FSEL_ALT1  = 0x05,   /* Alternate function 1 0b101 */
    BCM2835_GPIO_FSEL_ALT2  = 0x06,   /* Alternate function 2 0b110, */
    BCM2835_GPIO_FSEL_ALT3  = 0x07,   /* Alternate function 3 0b111 */
    BCM2835_GPIO_FSEL_ALT4  = 0x03,   /* Alternate function 4 0b011 */
    BCM2835_GPIO_FSEL_ALT5  = 0x02,   /* Alternate function 5 0b010 */
    BCM2835_GPIO_FSEL_MASK  = 0x07    /* Function select bits mask 0b111 */
} bcm2835FunctionSelect;

/* bcm2835PUDControl
  Pullup/Pulldown defines for bcm2835_gpio_pud()
*/
typedef enum
{
    BCM2835_GPIO_PUD_OFF     = 0x00,   /* Off ? disable pull-up/down 0b00 */
    BCM2835_GPIO_PUD_DOWN    = 0x01,   /* Enable Pull Down control 0b01 */
    BCM2835_GPIO_PUD_UP      = 0x02    /* Enable Pull Up control 0b10  */
} bcm2835PUDControl;

/* Pad control register offsets from BCM2835_GPIO_PADS */
#define BCM2835_PADS_GPIO_0_27               0x002c /* Pad control register for pads 0 to 27 */
#define BCM2835_PADS_GPIO_28_45              0x0030 /* Pad control register for pads 28 to 45 */
#define BCM2835_PADS_GPIO_46_53              0x0034 /* Pad control register for pads 46 to 53 */

/* Pad Control masks */
#define BCM2835_PAD_PASSWRD                  (0x5A << 24)  /* Password to enable setting pad mask */
#define BCM2835_PAD_SLEW_RATE_UNLIMITED      0x10 /* Slew rate unlimited */
#define BCM2835_PAD_HYSTERESIS_ENABLED       0x08 /* Hysteresis enabled */
#define BCM2835_PAD_DRIVE_2mA                0x00 /* 2mA drive current */
#define BCM2835_PAD_DRIVE_4mA                0x01 /* 4mA drive current */
#define BCM2835_PAD_DRIVE_6mA                0x02 /* 6mA drive current */
#define BCM2835_PAD_DRIVE_8mA                0x03 /* 8mA drive current */
#define BCM2835_PAD_DRIVE_10mA               0x04 /* 10mA drive current */
#define BCM2835_PAD_DRIVE_12mA               0x05 /* 12mA drive current */
#define BCM2835_PAD_DRIVE_14mA               0x06 /* 14mA drive current */
#define BCM2835_PAD_DRIVE_16mA               0x07 /* 16mA drive current */

/* bcm2835PadGroup
  Pad group specification for bcm2835_gpio_pad()
*/
typedef enum
{
    BCM2835_PAD_GROUP_GPIO_0_27  = 0, /* Pad group for GPIO pads 0 to 27 */
    BCM2835_PAD_GROUP_GPIO_28_45 = 1, /* Pad group for GPIO pads 28 to 45 */
    BCM2835_PAD_GROUP_GPIO_46_53 = 2  /* Pad group for GPIO pads 46 to 53 */
} bcm2835PadGroup;

/* GPIO Pin Numbers
  
  Here we define Raspberry Pin GPIO pins on P1 in terms of the underlying BCM GPIO pin numbers.
  These can be passed as a pin number to any function requiring a pin.
  Not all pins on the RPi 26 bin IDE plug are connected to GPIO pins
  and some can adopt an alternate function.
  RPi version 2 has some slightly different pinouts, and these are values RPI_V2_*.
  RPi B+ has yet differnet pinouts and these are defined in RPI_BPLUS_*.
  At bootup, pins 8 and 10 are set to UART0_TXD, UART0_RXD (ie the alt0 function) respectively
  When SPI0 is in use (ie after bcm2835_spi_begin()), SPI0 pins are dedicated to SPI
  and cant be controlled independently.
  If you are using the RPi Compute Module, just use the GPIO number: there is no need to use one of these
  symbolic names
*/
typedef enum
{
    RPI_GPIO_P1_03        =  0,  /* Version 1, Pin P1-03 */
    RPI_GPIO_P1_05        =  1,  /* Version 1, Pin P1-05 */
    RPI_GPIO_P1_07        =  4,  /* Version 1, Pin P1-07 */
    RPI_GPIO_P1_08        = 14,  /* Version 1, Pin P1-08, defaults to alt function 0 UART0_TXD */
    RPI_GPIO_P1_10        = 15,  /* Version 1, Pin P1-10, defaults to alt function 0 UART0_RXD */
    RPI_GPIO_P1_11        = 17,  /* Version 1, Pin P1-11 */
    RPI_GPIO_P1_12        = 18,  /* Version 1, Pin P1-12, can be PWM channel 0 in ALT FUN 5 */
    RPI_GPIO_P1_13        = 21,  /* Version 1, Pin P1-13 */
    RPI_GPIO_P1_15        = 22,  /* Version 1, Pin P1-15 */
    RPI_GPIO_P1_16        = 23,  /* Version 1, Pin P1-16 */
    RPI_GPIO_P1_18        = 24,  /* Version 1, Pin P1-18 */
    RPI_GPIO_P1_19        = 10,  /* Version 1, Pin P1-19, MOSI when SPI0 in use */
    RPI_GPIO_P1_21        =  9,  /* Version 1, Pin P1-21, MISO when SPI0 in use */
    RPI_GPIO_P1_22        = 25,  /* Version 1, Pin P1-22 */
    RPI_GPIO_P1_23        = 11,  /* Version 1, Pin P1-23, CLK when SPI0 in use */
    RPI_GPIO_P1_24        =  8,  /* Version 1, Pin P1-24, CE0 when SPI0 in use */
    RPI_GPIO_P1_26        =  7,  /* Version 1, Pin P1-26, CE1 when SPI0 in use */

    /* RPi Version 2 */
    RPI_V2_GPIO_P1_03     =  2,  /* Version 2, Pin P1-03 */
    RPI_V2_GPIO_P1_05     =  3,  /* Version 2, Pin P1-05 */
    RPI_V2_GPIO_P1_07     =  4,  /* Version 2, Pin P1-07 */
    RPI_V2_GPIO_P1_08     = 14,  /* Version 2, Pin P1-08, defaults to alt function 0 UART0_TXD */
    RPI_V2_GPIO_P1_10     = 15,  /* Version 2, Pin P1-10, defaults to alt function 0 UART0_RXD */
    RPI_V2_GPIO_P1_11     = 17,  /* Version 2, Pin P1-11 */
    RPI_V2_GPIO_P1_12     = 18,  /* Version 2, Pin P1-12, can be PWM channel 0 in ALT FUN 5 */
    RPI_V2_GPIO_P1_13     = 27,  /* Version 2, Pin P1-13 */
    RPI_V2_GPIO_P1_15     = 22,  /* Version 2, Pin P1-15 */
    RPI_V2_GPIO_P1_16     = 23,  /* Version 2, Pin P1-16 */
    RPI_V2_GPIO_P1_18     = 24,  /* Version 2, Pin P1-18 */
    RPI_V2_GPIO_P1_19     = 10,  /* Version 2, Pin P1-19, MOSI when SPI0 in use */
    RPI_V2_GPIO_P1_21     =  9,  /* Version 2, Pin P1-21, MISO when SPI0 in use */
    RPI_V2_GPIO_P1_22     = 25,  /* Version 2, Pin P1-22 */
    RPI_V2_GPIO_P1_23     = 11,  /* Version 2, Pin P1-23, CLK when SPI0 in use */
    RPI_V2_GPIO_P1_24     =  8,  /* Version 2, Pin P1-24, CE0 when SPI0 in use */
    RPI_V2_GPIO_P1_26     =  7,  /* Version 2, Pin P1-26, CE1 when SPI0 in use */
    RPI_V2_GPIO_P1_29     =  5,  /* Version 2, Pin P1-29 */
    RPI_V2_GPIO_P1_31     =  6,  /* Version 2, Pin P1-31 */
    RPI_V2_GPIO_P1_32     = 12,  /* Version 2, Pin P1-32 */
    RPI_V2_GPIO_P1_33     = 13,  /* Version 2, Pin P1-33 */
    RPI_V2_GPIO_P1_35     = 19,  /* Version 2, Pin P1-35, can be PWM channel 1 in ALT FUN 5  */
    RPI_V2_GPIO_P1_36     = 16,  /* Version 2, Pin P1-36 */
    RPI_V2_GPIO_P1_37     = 26,  /* Version 2, Pin P1-37 */
    RPI_V2_GPIO_P1_38     = 20,  /* Version 2, Pin P1-38 */
    RPI_V2_GPIO_P1_40     = 21,  /* Version 2, Pin P1-40 */

    /* RPi Version 2, new plug P5 */
    RPI_V2_GPIO_P5_03     = 28,  /* Version 2, Pin P5-03 */
    RPI_V2_GPIO_P5_04     = 29,  /* Version 2, Pin P5-04 */
    RPI_V2_GPIO_P5_05     = 30,  /* Version 2, Pin P5-05 */
    RPI_V2_GPIO_P5_06     = 31,  /* Version 2, Pin P5-06 */

    /* RPi B+ J8 header, also RPi 2 40 pin GPIO header */
    RPI_BPLUS_GPIO_J8_03     =  2,  /* B+, Pin J8-03 */
    RPI_BPLUS_GPIO_J8_05     =  3,  /* B+, Pin J8-05 */
    RPI_BPLUS_GPIO_J8_07     =  4,  /* B+, Pin J8-07 */
    RPI_BPLUS_GPIO_J8_08     = 14,  /* B+, Pin J8-08, defaults to alt function 0 UART0_TXD */
    RPI_BPLUS_GPIO_J8_10     = 15,  /* B+, Pin J8-10, defaults to alt function 0 UART0_RXD */
    RPI_BPLUS_GPIO_J8_11     = 17,  /* B+, Pin J8-11 */
    RPI_BPLUS_GPIO_J8_12     = 18,  /* B+, Pin J8-12, can be PWM channel 0 in ALT FUN 5 */
    RPI_BPLUS_GPIO_J8_13     = 27,  /* B+, Pin J8-13 */
    RPI_BPLUS_GPIO_J8_15     = 22,  /* B+, Pin J8-15 */
    RPI_BPLUS_GPIO_J8_16     = 23,  /* B+, Pin J8-16 */
    RPI_BPLUS_GPIO_J8_18     = 24,  /* B+, Pin J8-18 */
    RPI_BPLUS_GPIO_J8_19     = 10,  /* B+, Pin J8-19, MOSI when SPI0 in use */
    RPI_BPLUS_GPIO_J8_21     =  9,  /* B+, Pin J8-21, MISO when SPI0 in use */
    RPI_BPLUS_GPIO_J8_22     = 25,  /* B+, Pin J8-22 */
    RPI_BPLUS_GPIO_J8_23     = 11,  /* B+, Pin J8-23, CLK when SPI0 in use */
    RPI_BPLUS_GPIO_J8_24     =  8,  /* B+, Pin J8-24, CE0 when SPI0 in use */
    RPI_BPLUS_GPIO_J8_26     =  7,  /* B+, Pin J8-26, CE1 when SPI0 in use */
    RPI_BPLUS_GPIO_J8_29     =  5,  /* B+, Pin J8-29,  */
    RPI_BPLUS_GPIO_J8_31     =  6,  /* B+, Pin J8-31,  */
    RPI_BPLUS_GPIO_J8_32     = 12,  /* B+, Pin J8-32,  */
    RPI_BPLUS_GPIO_J8_33     = 13,  /* B+, Pin J8-33,  */
    RPI_BPLUS_GPIO_J8_35     = 19,  /* B+, Pin J8-35, can be PWM channel 1 in ALT FUN 5 */
    RPI_BPLUS_GPIO_J8_36     = 16,  /* B+, Pin J8-36,  */
    RPI_BPLUS_GPIO_J8_37     = 26,  /* B+, Pin J8-37,  */
    RPI_BPLUS_GPIO_J8_38     = 20,  /* B+, Pin J8-38,  */
    RPI_BPLUS_GPIO_J8_40     = 21   /* B+, Pin J8-40,  */
} RPiGPIOPin;

/* Defines for AUX
  GPIO register offsets from BCM2835_AUX_BASE.
*/
#define BCM2835_AUX_IRQ			0x0000  /* xxx */
#define BCM2835_AUX_ENABLE		0x0004  /* */

#define BCM2835_AUX_ENABLE_UART1	0x01    /*  */
#define BCM2835_AUX_ENABLE_SPI0		0x02	/* SPI0 (SPI1 in the device) */
#define BCM2835_AUX_ENABLE_SPI1		0x04	/* SPI1 (SPI2 in the device) */


#define BCM2835_AUX_SPI_CNTL0		0x0000  /* */
#define BCM2835_AUX_SPI_CNTL1 		0x0004  /* */
#define BCM2835_AUX_SPI_STAT 		0x0008  /* */
#define BCM2835_AUX_SPI_PEEK		0x000C  /* Read but do not take from FF */
#define BCM2835_AUX_SPI_IO		0x0020  /* Write = TX, read=RX */
#define BCM2835_AUX_SPI_TXHOLD		0x0030  /* Write = TX keep CS, read=RX */

#define BCM2835_AUX_SPI_CLOCK_MIN	30500		/* 30,5kHz */
#define BCM2835_AUX_SPI_CLOCK_MAX	125000000 	/* 125Mhz */

#define BCM2835_AUX_SPI_CNTL0_SPEED	0xFFF00000  /* */
#define BCM2835_AUX_SPI_CNTL0_SPEED_MAX	0xFFF      /* */
#define BCM2835_AUX_SPI_CNTL0_SPEED_SHIFT 20        /* */

#define BCM2835_AUX_SPI_CNTL0_CS0_N     0x000C0000 /* CS 0 low */
#define BCM2835_AUX_SPI_CNTL0_CS1_N     0x000A0000 /* CS 1 low */
#define BCM2835_AUX_SPI_CNTL0_CS2_N 	0x00060000 /* CS 2 low */

#define BCM2835_AUX_SPI_CNTL0_POSTINPUT	0x00010000  /* */
#define BCM2835_AUX_SPI_CNTL0_VAR_CS	0x00008000  /* */
#define BCM2835_AUX_SPI_CNTL0_VAR_WIDTH	0x00004000  /* */
#define BCM2835_AUX_SPI_CNTL0_DOUTHOLD	0x00003000  /* */
#define BCM2835_AUX_SPI_CNTL0_ENABLE	0x00000800  /* */
#define BCM2835_AUX_SPI_CNTL0_CPHA_IN	0x00000400  /* */
#define BCM2835_AUX_SPI_CNTL0_CLEARFIFO	0x00000200  /* */
#define BCM2835_AUX_SPI_CNTL0_CPHA_OUT	0x00000100  /* */
#define BCM2835_AUX_SPI_CNTL0_CPOL	0x00000080  /* */
#define BCM2835_AUX_SPI_CNTL0_MSBF_OUT	0x00000040  /* */
#define BCM2835_AUX_SPI_CNTL0_SHIFTLEN	0x0000003F  /* */

#define BCM2835_AUX_SPI_CNTL1_CSHIGH	0x00000700  /* */
#define BCM2835_AUX_SPI_CNTL1_IDLE	0x00000080  /* */
#define BCM2835_AUX_SPI_CNTL1_TXEMPTY	0x00000040  /* */
#define BCM2835_AUX_SPI_CNTL1_MSBF_IN	0x00000002  /* */
#define BCM2835_AUX_SPI_CNTL1_KEEP_IN	0x00000001  /* */

#define BCM2835_AUX_SPI_STAT_TX_LVL	0xFF000000  /* */
#define BCM2835_AUX_SPI_STAT_RX_LVL	0x00FF0000  /* */
#define BCM2835_AUX_SPI_STAT_TX_FULL	0x00000400  /* */
#define BCM2835_AUX_SPI_STAT_TX_EMPTY	0x00000200  /* */
#define BCM2835_AUX_SPI_STAT_RX_FULL	0x00000100  /* */
#define BCM2835_AUX_SPI_STAT_RX_EMPTY	0x00000080  /* */
#define BCM2835_AUX_SPI_STAT_BUSY	0x00000040  /* */
#define BCM2835_AUX_SPI_STAT_BITCOUNT	0x0000003F  /* */

/* Defines for SPI
   GPIO register offsets from BCM2835_SPI0_BASE. 
   Offsets into the SPI Peripheral block in bytes per 10.5 SPI Register Map
*/
#define BCM2835_SPI0_CS                 0x0000 /* SPI Master Control and Status */
#define BCM2835_SPI0_FIFO               0x0004 /* SPI Master TX and RX FIFOs */
#define BCM2835_SPI0_CLK                0x0008 /* SPI Master Clock Divider */
#define BCM2835_SPI0_DLEN               0x000c /* SPI Master Data Length */
#define BCM2835_SPI0_LTOH               0x0010 /* SPI LOSSI mode TOH */
#define BCM2835_SPI0_DC                 0x0014 /* SPI DMA DREQ Controls */

/* Register masks for SPI0_CS */
#define BCM2835_SPI0_CS_LEN_LONG        0x02000000 /* Enable Long data word in Lossi mode if DMA_LEN is set */
#define BCM2835_SPI0_CS_DMA_LEN         0x01000000 /* Enable DMA mode in Lossi mode */
#define BCM2835_SPI0_CS_CSPOL2          0x00800000 /* Chip Select 2 Polarity */
#define BCM2835_SPI0_CS_CSPOL1          0x00400000 /* Chip Select 1 Polarity */
#define BCM2835_SPI0_CS_CSPOL0          0x00200000 /* Chip Select 0 Polarity */
#define BCM2835_SPI0_CS_RXF             0x00100000 /* RXF - RX FIFO Full */
#define BCM2835_SPI0_CS_RXR             0x00080000 /* RXR RX FIFO needs Reading (full) */
#define BCM2835_SPI0_CS_TXD             0x00040000 /* TXD TX FIFO can accept Data */
#define BCM2835_SPI0_CS_RXD             0x00020000 /* RXD RX FIFO contains Data */
#define BCM2835_SPI0_CS_DONE            0x00010000 /* Done transfer Done */
#define BCM2835_SPI0_CS_TE_EN           0x00008000 /* Unused */
#define BCM2835_SPI0_CS_LMONO           0x00004000 /* Unused */
#define BCM2835_SPI0_CS_LEN             0x00002000 /* LEN LoSSI enable */
#define BCM2835_SPI0_CS_REN             0x00001000 /* REN Read Enable */
#define BCM2835_SPI0_CS_ADCS            0x00000800 /* ADCS Automatically Deassert Chip Select */
#define BCM2835_SPI0_CS_INTR            0x00000400 /* INTR Interrupt on RXR */
#define BCM2835_SPI0_CS_INTD            0x00000200 /* INTD Interrupt on Done */
#define BCM2835_SPI0_CS_DMAEN           0x00000100 /* DMAEN DMA Enable */
#define BCM2835_SPI0_CS_TA              0x00000080 /* Transfer Active */
#define BCM2835_SPI0_CS_CSPOL           0x00000040 /* Chip Select Polarity */
#define BCM2835_SPI0_CS_CLEAR           0x00000030 /* Clear FIFO Clear RX and TX */
#define BCM2835_SPI0_CS_CLEAR_RX        0x00000020 /* Clear FIFO Clear RX  */
#define BCM2835_SPI0_CS_CLEAR_TX        0x00000010 /* Clear FIFO Clear TX  */
#define BCM2835_SPI0_CS_CPOL            0x00000008 /* Clock Polarity */
#define BCM2835_SPI0_CS_CPHA            0x00000004 /* Clock Phase */
#define BCM2835_SPI0_CS_CS              0x00000003 /* Chip Select */

/* bcm2835SPIBitOrder SPI Bit order
  Specifies the SPI data bit ordering for bcm2835_spi_setBitOrder()
*/
typedef enum
{
    BCM2835_SPI_BIT_ORDER_LSBFIRST = 0,  /* LSB First */
    BCM2835_SPI_BIT_ORDER_MSBFIRST = 1   /* MSB First */
}bcm2835SPIBitOrder;

/* SPI Data mode
  Specify the SPI data mode to be passed to bcm2835_spi_setDataMode()
*/
typedef enum
{
    BCM2835_SPI_MODE0 = 0,  /* CPOL = 0, CPHA = 0 */
    BCM2835_SPI_MODE1 = 1,  /* CPOL = 0, CPHA = 1 */
    BCM2835_SPI_MODE2 = 2,  /* CPOL = 1, CPHA = 0 */
    BCM2835_SPI_MODE3 = 3   /* CPOL = 1, CPHA = 1 */
}bcm2835SPIMode;

/* bcm2835SPIChipSelect
  Specify the SPI chip select pin(s)
*/
typedef enum
{
    BCM2835_SPI_CS0 = 0,    /* Chip Select 0 */
    BCM2835_SPI_CS1 = 1,    /* Chip Select 1 */
    BCM2835_SPI_CS2 = 2,    /* Chip Select 2 (ie pins CS1 and CS2 are asserted) */
    BCM2835_SPI_CS_NONE = 3 /* No CS, control it yourself */
} bcm2835SPIChipSelect;

/* bcm2835SPIClockDivider
  Specifies the divider used to generate the SPI clock from the system clock.
  Figures below give the divider, clock period and clock frequency.
  Clock divided is based on nominal core clock rate of 250MHz on RPi1 and RPi2, and 400MHz on RPi3.
  It is reported that (contrary to the documentation) any even divider may used.
  The frequencies shown for each divider have been confirmed by measurement on RPi1 and RPi2.
  The system clock frequency on RPi3 is different, so the frequency you get from a given divider will be different.
  See comments in 'SPI Pins' for information about reliable SPI speeds.
  Note: it is possible to change the core clock rate of the RPi 3 back to 250MHz, by putting 
  \code
  core_freq=250
  \endcode
  in the config.txt
*/
typedef enum
{
    BCM2835_SPI_CLOCK_DIVIDER_65536 = 0,       /* 65536 = 3.814697260kHz on Rpi2, 6.1035156kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_32768 = 32768,   /* 32768 = 7.629394531kHz on Rpi2, 12.20703125kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_16384 = 16384,   /* 16384 = 15.25878906kHz on Rpi2, 24.4140625kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_8192  = 8192,    /* 8192 = 30.51757813kHz on Rpi2, 48.828125kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_4096  = 4096,    /* 4096 = 61.03515625kHz on Rpi2, 97.65625kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_2048  = 2048,    /* 2048 = 122.0703125kHz on Rpi2, 195.3125kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_1024  = 1024,    /* 1024 = 244.140625kHz on Rpi2, 390.625kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_512   = 512,     /* 512 = 488.28125kHz on Rpi2, 781.25kHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_256   = 256,     /* 256 = 976.5625kHz on Rpi2, 1.5625MHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_128   = 128,     /* 128 = 1.953125MHz on Rpi2, 3.125MHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_64    = 64,      /* 64 = 3.90625MHz on Rpi2, 6.250MHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_32    = 32,      /* 32 = 7.8125MHz on Rpi2, 12.5MHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_16    = 16,      /* 16 = 15.625MHz on Rpi2, 25MHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_8     = 8,       /* 8 = 31.25MHz on Rpi2, 50MHz on RPI3 */
    BCM2835_SPI_CLOCK_DIVIDER_4     = 4,       /* 4 = 62.5MHz on Rpi2, 100MHz on RPI3. Dont expect this speed to work reliably. */
    BCM2835_SPI_CLOCK_DIVIDER_2     = 2,       /* 2 = 125MHz on Rpi2, 200MHz on RPI3, fastest you can get. Dont expect this speed to work reliably.*/
    BCM2835_SPI_CLOCK_DIVIDER_1     = 1        /* 1 = 3.814697260kHz on Rpi2, 6.1035156kHz on RPI3, same as 0/65536 */
} bcm2835SPIClockDivider;

/* Defines for I2C
   GPIO register offsets from BCM2835_BSC*_BASE.
   Offsets into the BSC Peripheral block in bytes per 3.1 BSC Register Map
*/
#define BCM2835_BSC_C 			0x0000 /* BSC Master Control */
#define BCM2835_BSC_S 			0x0004 /* BSC Master Status */
#define BCM2835_BSC_DLEN		0x0008 /* BSC Master Data Length */
#define BCM2835_BSC_A 			0x000c /* BSC Master Slave Address */
#define BCM2835_BSC_FIFO		0x0010 /* BSC Master Data FIFO */
#define BCM2835_BSC_DIV			0x0014 /* BSC Master Clock Divider */
#define BCM2835_BSC_DEL			0x0018 /* BSC Master Data Delay */
#define BCM2835_BSC_CLKT		0x001c /* BSC Master Clock Stretch Timeout */

/* Register masks for BSC_C */
#define BCM2835_BSC_C_I2CEN 		0x00008000 /* I2C Enable, 0 = disabled, 1 = enabled */
#define BCM2835_BSC_C_INTR 		0x00000400 /* Interrupt on RX */
#define BCM2835_BSC_C_INTT 		0x00000200 /* Interrupt on TX */
#define BCM2835_BSC_C_INTD 		0x00000100 /* Interrupt on DONE */
#define BCM2835_BSC_C_ST 		0x00000080 /* Start transfer, 1 = Start a new transfer */
#define BCM2835_BSC_C_CLEAR_1 		0x00000020 /* Clear FIFO Clear */
#define BCM2835_BSC_C_CLEAR_2 		0x00000010 /* Clear FIFO Clear */
#define BCM2835_BSC_C_READ 		0x00000001 /*	Read transfer */

/* Register masks for BSC_S */
#define BCM2835_BSC_S_CLKT 		0x00000200 /* Clock stretch timeout */
#define BCM2835_BSC_S_ERR 		0x00000100 /* ACK error */
#define BCM2835_BSC_S_RXF 		0x00000080 /* RXF FIFO full, 0 = FIFO is not full, 1 = FIFO is full */
#define BCM2835_BSC_S_TXE 		0x00000040 /* TXE FIFO full, 0 = FIFO is not full, 1 = FIFO is full */
#define BCM2835_BSC_S_RXD 		0x00000020 /* RXD FIFO contains data */
#define BCM2835_BSC_S_TXD 		0x00000010 /* TXD FIFO can accept data */
#define BCM2835_BSC_S_RXR 		0x00000008 /* RXR FIFO needs reading (full) */
#define BCM2835_BSC_S_TXW 		0x00000004 /* TXW FIFO needs writing (full) */
#define BCM2835_BSC_S_DONE 		0x00000002 /* Transfer DONE */
#define BCM2835_BSC_S_TA 		0x00000001 /* Transfer Active */

#define BCM2835_BSC_FIFO_SIZE   	16 /* BSC FIFO size */

/* bcm2835I2CClockDivider
  Specifies the divider used to generate the I2C clock from the system clock.
  Clock divided is based on nominal base clock rate of 250MHz
*/
typedef enum
{
    BCM2835_I2C_CLOCK_DIVIDER_2500   = 2500,      /* 2500 = 10us = 100 kHz */
    BCM2835_I2C_CLOCK_DIVIDER_626    = 626,       /* 622 = 2.504us = 399.3610 kHz */
    BCM2835_I2C_CLOCK_DIVIDER_150    = 150,       /* 150 = 60ns = 1.666 MHz (default at reset) */
    BCM2835_I2C_CLOCK_DIVIDER_148    = 148        /* 148 = 59ns = 1.689 MHz */
} bcm2835I2CClockDivider;

/* bcm2835I2CReasonCodes
  Specifies the reason codes for the bcm2835_i2c_write and bcm2835_i2c_read functions.
*/
typedef enum
{
    BCM2835_I2C_REASON_OK   	     = 0x00,      /* Success */
    BCM2835_I2C_REASON_ERROR_NACK    = 0x01,      /* Received a NACK */
    BCM2835_I2C_REASON_ERROR_CLKT    = 0x02,      /* Received Clock Stretch Timeout */
    BCM2835_I2C_REASON_ERROR_DATA    = 0x04       /* Not all data is sent / received */
} bcm2835I2CReasonCodes;

/* Defines for ST
   GPIO register offsets from BCM2835_ST_BASE.
   Offsets into the ST Peripheral block in bytes per 12.1 System Timer Registers
   The System Timer peripheral provides four 32-bit timer channels and a single 64-bit free running counter.
   BCM2835_ST_CLO is the System Timer Counter Lower bits register.
   The system timer free-running counter lower register is a read-only register that returns the current value
   of the lower 32-bits of the free running counter.
   BCM2835_ST_CHI is the System Timer Counter Upper bits register.
   The system timer free-running counter upper register is a read-only register that returns the current value
   of the upper 32-bits of the free running counter.
*/
#define BCM2835_ST_CS 			0x0000 /* System Timer Control/Status */
#define BCM2835_ST_CLO 			0x0004 /* System Timer Counter Lower 32 bits */
#define BCM2835_ST_CHI 			0x0008 /* System Timer Counter Upper 32 bits */


/* Defines for PWM, word offsets (ie 4 byte multiples) */
#define BCM2835_PWM_CONTROL 0
#define BCM2835_PWM_STATUS  1
#define BCM2835_PWM_DMAC    2
#define BCM2835_PWM0_RANGE  4
#define BCM2835_PWM0_DATA   5
#define BCM2835_PWM_FIF1    6
#define BCM2835_PWM1_RANGE  8
#define BCM2835_PWM1_DATA   9

/* Defines for PWM Clock, word offsets (ie 4 byte multiples) */
#define BCM2835_PWMCLK_CNTL     40
#define BCM2835_PWMCLK_DIV      41
#define BCM2835_PWM_PASSWRD     (0x5A << 24)  /* Password to enable setting PWM clock */

#define BCM2835_PWM1_MS_MODE    0x8000  /* Run in Mark/Space mode */
#define BCM2835_PWM1_USEFIFO    0x2000  /* Data from FIFO */
#define BCM2835_PWM1_REVPOLAR   0x1000  /* Reverse polarity */
#define BCM2835_PWM1_OFFSTATE   0x0800  /* Ouput Off state */
#define BCM2835_PWM1_REPEATFF   0x0400  /* Repeat last value if FIFO empty */
#define BCM2835_PWM1_SERIAL     0x0200  /* Run in serial mode */
#define BCM2835_PWM1_ENABLE     0x0100  /* Channel Enable */

#define BCM2835_PWM0_MS_MODE    0x0080  /* Run in Mark/Space mode */
#define BCM2835_PWM_CLEAR_FIFO  0x0040  /* Clear FIFO */
#define BCM2835_PWM0_USEFIFO    0x0020  /* Data from FIFO */
#define BCM2835_PWM0_REVPOLAR   0x0010  /* Reverse polarity */
#define BCM2835_PWM0_OFFSTATE   0x0008  /* Ouput Off state */
#define BCM2835_PWM0_REPEATFF   0x0004  /* Repeat last value if FIFO empty */
#define BCM2835_PWM0_SERIAL     0x0002  /* Run in serial mode */
#define BCM2835_PWM0_ENABLE     0x0001  /* Channel Enable */

/* bcm2835PWMClockDivider
  Specifies the divider used to generate the PWM clock from the system clock.
  Figures below give the divider, clock period and clock frequency.
  Clock divided is based on nominal PWM base clock rate of 19.2MHz
  The frequencies shown for each divider have been confirmed by measurement
*/
typedef enum {
  BCM2835_PWM_CLOCK_DIVIDER_2048  = 2048,    /* 2048 = 9.375kHz */
  BCM2835_PWM_CLOCK_DIVIDER_1024  = 1024,    /* 1024 = 18.75kHz */
  BCM2835_PWM_CLOCK_DIVIDER_512   = 512,     /* 512 = 37.5kHz */
  BCM2835_PWM_CLOCK_DIVIDER_256   = 256,     /* 256 = 75kHz */
  BCM2835_PWM_CLOCK_DIVIDER_128   = 128,     /* 128 = 150kHz */
  BCM2835_PWM_CLOCK_DIVIDER_64    = 64,      /* 64 = 300kHz */
  BCM2835_PWM_CLOCK_DIVIDER_32    = 32,      /* 32 = 600.0kHz */
  BCM2835_PWM_CLOCK_DIVIDER_16    = 16,      /* 16 = 1.2MHz */
  BCM2835_PWM_CLOCK_DIVIDER_8     = 8,       /* 8 = 2.4MHz */
  BCM2835_PWM_CLOCK_DIVIDER_4     = 4,       /* 4 = 4.8MHz */
  BCM2835_PWM_CLOCK_DIVIDER_2     = 2,       /* 2 = 9.6MHz, fastest you can get */
  BCM2835_PWM_CLOCK_DIVIDER_1     = 1        /* 1 = 4.6875kHz, same as divider 4096 */
} bcm2835PWMClockDivider;

  /* \defgroup init Library initialisation and management
    These functions allow you to intialise and control the bcm2835 library
  */

  /* Initialise the library by opening /dev/mem (if you are root) 
    or /dev/gpiomem (if you are not)
    and getting pointers to the 
    internal memory for BCM 2835 device registers. You must call this (successfully)
    before calling any other 
    functions in this library (except bcm2835_set_debug). 
    If bcm2835_init() fails by returning 0, 
    calling any other function may result in crashes or other failures.
    If bcm2835_init() succeeds but you are not running as root, then only gpio operations
    are permitted, and calling any other functions may result in crashes or other failures. .
    Prints messages to stderr in case of errors.
    \return 1 if successful else 0
  */
  extern int bcm2835_init(void);

  /* Close the library, deallocating any allocated memory and closing /dev/mem
    \return 1 if successful else 0
  */
  extern int bcm2835_close(void);

  /* Sets the debug level of the library.
    A value of 1 prevents mapping to /dev/mem, and makes the library print out
    what it would do, rather than accessing the GPIO registers.
    A value of 0, the default, causes normal operation.
    Call this before calling bcm2835_init();
    \param[in] debug The new debug level. 1 means debug
  */
  extern void  bcm2835_set_debug(uint8_t debug);

  /* Returns the version number of the library, same as BCM2835_VERSION
     \return the current library version number
  */
  extern unsigned int bcm2835_version(void);

  /* \defgroup lowlevel Low level register access
    These functions provide low level register access, and should not generally
    need to be used 
  */

  /* Gets the base of a register
    \param[in] regbase You can use one of the common values BCM2835_REGBASE_*
    in \ref bcm2835RegisterBase
    \return the register base
    \sa Physical Addresses
  */
  extern uint32_t* bcm2835_regbase(uint8_t regbase);

  /* Reads 32 bit value from a peripheral address WITH a memory barrier before and after each read.
    This is safe, but slow.  The MB before protects this read from any in-flight reads that didn't
    use a MB.  The MB after protects subsequent reads from another peripheral.

    \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    \return the value read from the 32 bit register
    \sa Physical Addresses
  */
  inline uint32_t bcm2835_peri_read(volatile uint32_t* paddr) {
    uint32_t ret;
    __sync_synchronize();
    ret = *paddr;
    __sync_synchronize();
    return ret;
  }

  /* Reads 32 bit value from a peripheral address WITHOUT the read barriers
    You should only use this when:
    o your code has previously called bcm2835_peri_read() for a register
    within the same peripheral, and no read or write to another peripheral has occurred since.
    o your code has called bcm2835_memory_barrier() since the last access to ANOTHER peripheral.

    \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    \return the value read from the 32 bit register
    \sa Physical Addresses
  */
  inline uint32_t bcm2835_peri_read_nb(volatile uint32_t* paddr) {
    return *paddr;
  }


  /* Writes 32 bit value from a peripheral address WITH a memory barrier before and after each write
    This is safe, but slow.  The MB before ensures that any in-flight write to another peripheral
    completes before this write is issued.  The MB after ensures that subsequent reads and writes
    to another peripheral will see the effect of this write.

    This is a tricky optimization; if you aren't sure, use the barrier version.

    \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    \param[in] value The 32 bit value to write
    \sa Physical Addresses
  */
  inline void bcm2835_peri_write(volatile uint32_t* paddr, uint32_t value) {
    __sync_synchronize();
    *paddr = value;
    __sync_synchronize();
  }

  /* Writes 32 bit value from a peripheral address without the write barrier
    You should only use this when:
    o your code has previously called bcm2835_peri_write() for a register
    within the same peripheral, and no other peripheral access has occurred since.
    o your code has called bcm2835_memory_barrier() since the last access to ANOTHER peripheral.

    This is a tricky optimization; if you aren't sure, use the barrier version.

    \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    \param[in] value The 32 bit value to write
    \sa Physical Addresses
  */
  inline void bcm2835_peri_write_nb(volatile uint32_t* paddr, uint32_t value) {
    *paddr = value;
  }

  /* Alters a number of bits in a 32 peripheral regsiter.
    It reads the current valu and then alters the bits defines as 1 in mask, 
    according to the bit value in value. 
    All other bits that are 0 in the mask are unaffected.
    Use this to alter a subset of the bits in a register.
    Memory barriers are used.  Note that this is not atomic; an interrupt
    routine can cause unexpected results.
    \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    \param[in] value The 32 bit value to write, masked in by mask.
    \param[in] mask Bitmask that defines the bits that will be altered in the register.
    \sa Physical Addresses
  */
  extern void bcm2835_peri_set_bits(volatile uint32_t* paddr, uint32_t value, uint32_t mask);

  // Hacky/experimental: disable all interrupts. At best, the OS will never
  // come back!
  extern void bcm2835_int_disable_all();
  
  /* \defgroup gpio GPIO register access
    These functions allow you to control the GPIO interface. You can set the 
    function of each GPIO pin, read the input state and set the output state.
  */

  /* Sets the Function Select register for the given pin, which configures
    the pin as Input, Output or one of the 6 alternate functions.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \param[in] mode Mode to set the pin to, one of BCM2835_GPIO_FSEL_* from \ref bcm2835FunctionSelect
  */
  extern void bcm2835_gpio_fsel(uint8_t pin, uint8_t mode);

  /* Sets the specified pin output to 
    HIGH.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \sa bcm2835_gpio_write()
  */
  extern void bcm2835_gpio_set(uint8_t pin);

  /* Sets the specified pin output to 
    LOW.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \sa bcm2835_gpio_write()
  */
  extern void bcm2835_gpio_clr(uint8_t pin);

  /* Sets any of the first 32 GPIO output pins specified in the mask to 
     HIGH.
     \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
     \sa bcm2835_gpio_write_multi()
  */
  inline void bcm2835_gpio_set_multi(uint32_t mask) {
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPSET0/4;
    bcm2835_peri_write(paddr, mask);
  }

  inline void bcm2835_gpio_set_multi_nb(uint32_t mask) {
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPSET0/4;
    *paddr = mask;
  }

  /* Sets any of the first 32 GPIO output pins specified in the mask to 
     LOW.
     \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
     \sa bcm2835_gpio_write_multi()
  */
  inline void bcm2835_gpio_clr_multi(uint32_t mask) {
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPCLR0/4;
    bcm2835_peri_write(paddr, mask);
  }

  /* Reads the current level on the specified 
    pin and returns either HIGH or LOW. Works whether or not the pin
    is an input or an output.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \return the current level  either HIGH or LOW
  */
  extern uint8_t bcm2835_gpio_lev(uint8_t pin);

  /* Read pins 0-32 all at once.
     Not all bits are guaranteed to have meaningful values, but bit offsets
     for "BCM" pins are a-OK! */
  inline uint32_t bcm2835_gpio_lev_multi() {
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPLEV0/4;
    uint32_t ret;
    ret = *paddr;
    // Sync after the last read
    __sync_synchronize();
    return ret;
  }

  inline uint32_t bcm2835_gpio_lev_multi_nb() {
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPLEV0/4;
    uint32_t ret;
    ret = *paddr;
    return ret;
  }

  /* Event Detect Status.
    Tests whether the specified pin has detected a level or edge
    as requested by bcm2835_gpio_ren(), bcm2835_gpio_fen(), bcm2835_gpio_hen(), 
    bcm2835_gpio_len(), bcm2835_gpio_aren(), bcm2835_gpio_afen().
    Clear the flag for a given pin by calling bcm2835_gpio_set_eds(pin);
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \return HIGH if the event detect status for the given pin is true.
  */
  extern uint8_t bcm2835_gpio_eds(uint8_t pin);

  /* Same as bcm2835_gpio_eds() but checks if any of the pins specified in
    the mask have detected a level or edge.
    \param[in] mask Mask of pins to check. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    \return Mask of pins HIGH if the event detect status for the given pin is true.
  */
  extern uint32_t bcm2835_gpio_eds_multi(uint32_t mask);

  /* Sets the Event Detect Status register for a given pin to 1, 
    which has the effect of clearing the flag. Use this afer seeing
    an Event Detect Status on the pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_set_eds(uint8_t pin);

  /* Same as bcm2835_gpio_set_eds() but clears the flag for any pin which
    is set in the mask.
    \param[in] mask Mask of pins to clear. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
  */
  extern void bcm2835_gpio_set_eds_multi(uint32_t mask);

  /* Enable Rising Edge Detect Enable for the specified pin.
    When a rising edge is detected, sets the appropriate pin in Event Detect Status.
    The GPRENn registers use
    synchronous edge detection. This means the input signal is sampled using the
    system clock and then it is looking for a ?011? pattern on the sampled signal. This
    has the effect of suppressing glitches.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_ren(uint8_t pin);

  /* Disable Rising Edge Detect Enable for the specified pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_clr_ren(uint8_t pin);

  /* Enable Falling Edge Detect Enable for the specified pin.
    When a falling edge is detected, sets the appropriate pin in Event Detect Status.
    The GPRENn registers use
    synchronous edge detection. This means the input signal is sampled using the
    system clock and then it is looking for a ?100? pattern on the sampled signal. This
    has the effect of suppressing glitches.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_fen(uint8_t pin);

  /* Disable Falling Edge Detect Enable for the specified pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_clr_fen(uint8_t pin);

  /* Enable High Detect Enable for the specified pin.
    When a HIGH level is detected on the pin, sets the appropriate pin in Event Detect Status.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_hen(uint8_t pin);

  /* Disable High Detect Enable for the specified pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_clr_hen(uint8_t pin);

  /* Enable Low Detect Enable for the specified pin.
    When a LOW level is detected on the pin, sets the appropriate pin in Event Detect Status.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_len(uint8_t pin);

  /* Disable Low Detect Enable for the specified pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_clr_len(uint8_t pin);

  /* Enable Asynchronous Rising Edge Detect Enable for the specified pin.
    When a rising edge is detected, sets the appropriate pin in Event Detect Status.
    Asynchronous means the incoming signal is not sampled by the system clock. As such
    rising edges of very short duration can be detected.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_aren(uint8_t pin);

  /* Disable Asynchronous Rising Edge Detect Enable for the specified pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_clr_aren(uint8_t pin);

  /* Enable Asynchronous Falling Edge Detect Enable for the specified pin.
    When a falling edge is detected, sets the appropriate pin in Event Detect Status.
    Asynchronous means the incoming signal is not sampled by the system clock. As such
    falling edges of very short duration can be detected.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_afen(uint8_t pin);

  /* Disable Asynchronous Falling Edge Detect Enable for the specified pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  */
  extern void bcm2835_gpio_clr_afen(uint8_t pin);

  /* Sets the Pull-up/down register for the given pin. This is
    used with bcm2835_gpio_pudclk() to set the Pull-up/down resistor for the given pin.
    However, it is usually more convenient to use bcm2835_gpio_set_pud().
    \param[in] pud The desired Pull-up/down mode. One of BCM2835_GPIO_PUD_* from bcm2835PUDControl
    \sa bcm2835_gpio_set_pud()
  */
  extern void bcm2835_gpio_pud(uint8_t pud);

  /* Clocks the Pull-up/down value set earlier by bcm2835_gpio_pud() into the pin.
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \param[in] on HIGH to clock the value from bcm2835_gpio_pud() into the pin. 
    LOW to remove the clock. 
    \sa bcm2835_gpio_set_pud()
  */
  extern void bcm2835_gpio_pudclk(uint8_t pin, uint8_t on);

  /* Reads and returns the Pad Control for the given GPIO group.
    Caution: requires root access.
    \param[in] group The GPIO pad group number, one of BCM2835_PAD_GROUP_GPIO_*
    \return Mask of bits from BCM2835_PAD_* from \ref bcm2835PadGroup
  */
  extern uint32_t bcm2835_gpio_pad(uint8_t group);

  /* Sets the Pad Control for the given GPIO group.
    Caution: requires root access.
    \param[in] group The GPIO pad group number, one of BCM2835_PAD_GROUP_GPIO_*
    \param[in] control Mask of bits from BCM2835_PAD_* from \ref bcm2835PadGroup. Note 
    that it is not necessary to include BCM2835_PAD_PASSWRD in the mask as this
    is automatically included.
  */
  extern void bcm2835_gpio_set_pad(uint8_t group, uint32_t control);

  /* Delays for the specified number of milliseconds.
    Uses nanosleep(), and therefore does not use CPU until the time is up.
    However, you are at the mercy of nanosleep(). From the manual for nanosleep():
    If the interval specified in req is not an exact multiple of the granularity  
    underlying clock (see time(7)), then the interval will be
    rounded up to the next multiple. Furthermore, after the sleep completes, 
    there may still be a delay before the CPU becomes free to once
    again execute the calling thread.
    \param[in] millis Delay in milliseconds
  */
  extern void bcm2835_delay(unsigned int millis);

  /* Delays for the specified number of microseconds.
    Uses a combination of nanosleep() and a busy wait loop on the BCM2835 system timers,
    However, you are at the mercy of nanosleep(). From the manual for nanosleep():
    If the interval specified in req is not an exact multiple of the granularity  
    underlying clock (see time(7)), then the interval will be
    rounded up to the next multiple. Furthermore, after the sleep completes, 
    there may still be a delay before the CPU becomes free to once
    again execute the calling thread.
    For times less than about 450 microseconds, uses a busy wait on the System Timer.
    It is reported that a delay of 0 microseconds on RaspberryPi will in fact
    result in a delay of about 80 microseconds. Your mileage may vary.
    \param[in] micros Delay in microseconds
  */
  extern void bcm2835_delayMicroseconds(uint64_t micros);

  /* Sets the output state of the specified pin
    \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    \param[in] on HIGH sets the output to HIGH and LOW to LOW.
  */
  extern void bcm2835_gpio_write(uint8_t pin, uint8_t on);

  /* Sets any of the first 32 GPIO output pins specified in the mask to the state given by on
    \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    \param[in] on HIGH sets the output to HIGH and LOW to LOW.
  */
  extern void bcm2835_gpio_write_multi(uint32_t mask, uint8_t on);

    /* Sets the first 32 GPIO output pins specified in the mask to the value given by value
      \param[in] value values required for each bit masked in by mask, eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
      \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    */
  inline void bcm2835_gpio_write_mask_old(uint32_t value, uint32_t mask) {
    // PERF: I think the second one can be without memory barrier, but
    // is it the "same peripheral"? -tom7
    bcm2835_gpio_set_multi(value & mask);
    bcm2835_gpio_clr_multi((~value) & mask);
  }

  inline void bcm2835_gpio_write_mask(uint32_t value, uint32_t mask) {
    // PERF: I think the second one can be without memory barrier, but
    // is it the "same peripheral"? -tom7
    // According to the Broadcom doc, "the GPIO peripheral" appears to be
    // a single peripheral.
    // "a memory write barrier before the first write to a peripheral"
    const uint32_t ons = value & mask;
    volatile uint32_t* paddr_s = bcm2835_gpio + BCM2835_GPSET0/4;
    volatile uint32_t* paddr_c = bcm2835_gpio + BCM2835_GPCLR0/4;
    const uint32_t offs = ~value & mask;
    // Sync before the first write
    __sync_synchronize();
    *paddr_s = ons;
    *paddr_c = offs;
    // PERF: Are these both necessary? there's some asymmetry in docs between
    // read/write before/after
    // __sync_synchronize();
  }

  inline void bcm2835_gpio_write_mask_nb(uint32_t value, uint32_t mask) {
    const uint32_t ons = value & mask;
    volatile uint32_t* paddr_s = bcm2835_gpio + BCM2835_GPSET0/4;
    volatile uint32_t* paddr_c = bcm2835_gpio + BCM2835_GPCLR0/4;
    const uint32_t offs = ~value & mask;
    *paddr_s = ons;
    *paddr_c = offs;
  }

  // Clear without barrier; must follow e.g. write_mask.
  inline void bcm2835_gpio_clr_multi_nb(uint32_t mask) {
    // bcm2835_gpio_write_multi(0, mask);
    volatile uint32_t* paddr_c = bcm2835_gpio + BCM2835_GPCLR0/4;
    const uint32_t offs = mask;
    *paddr_c = offs;
  }
  
  /* Sets the Pull-up/down mode for the specified pin. This is more 
     convenient than clocking the mode in with bcm2835_gpio_pud() and 
     bcm2835_gpio_pudclk().
     \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
     \param[in] pud The desired Pull-up/down mode. One of 
     BCM2835_GPIO_PUD_* from bcm2835PUDControl
  */
  extern void bcm2835_gpio_set_pud(uint8_t pin, uint8_t pud);

#ifndef DISABLE_I2C

  // I2C support.
  
  /* Start I2C operations.

    Forces RPi I2C pins P1-03 (SDA) and P1-05 (SCL) to alternate
    function ALT0, which enables those pins for I2C interface. You
    should call bcm2835_i2c_end() when all I2C functions are
    complete to return the pins to their default functions

    \return 1 if successful, 0 otherwise (perhaps because you are
    not running as root)
  */
  extern int bcm2835_i2c_begin(void);

  /* End I2C operations.
    I2C pins P1-03 (SDA) and P1-05 (SCL)
    are returned to their default INPUT behaviour.
  */
  extern void bcm2835_i2c_end(void);

  /* Sets the I2C slave address.
    \param[in] addr The I2C slave address.
  */
  extern void bcm2835_i2c_setSlaveAddress(uint8_t addr);

  /* Sets the I2C clock divider and therefore the I2C clock speed.
    \param[in] divider The desired I2C clock divider, one of
    BCM2835_I2C_CLOCK_DIVIDER_*, see \ref bcm2835I2CClockDivider
  */
  extern void bcm2835_i2c_setClockDivider(uint16_t divider);

  /* Sets the I2C clock divider by converting the baudrate parameter to
    the equivalent I2C clock divider. (see bcm2835_i2c_setClockDivider)
    For the I2C standard 100khz you would set baudrate to 100000
    The use of baudrate corresponds to its use in the I2C kernel device
    driver. (Of course, bcm2835 has nothing to do with the kernel driver)
  */
  extern void bcm2835_i2c_set_baudrate(uint32_t baudrate);

  /* Transfers any number of bytes to the currently selected I2C slave.
    (as previously set by \sa bcm2835_i2c_setSlaveAddress)
    \param[in] buf Buffer of bytes to send.
    \param[in] len Number of bytes in the buf buffer, and the number of 
    bytes to send.
    \return reason see \ref bcm2835I2CReasonCodes
  */
  extern uint8_t bcm2835_i2c_write(const char *buf, uint32_t len);

  /* Transfers any number of bytes from the currently selected I2C slave.
    (as previously set by \sa bcm2835_i2c_setSlaveAddress)
    \param[in] buf Buffer of bytes to receive.
    \param[in] len Number of bytes in the buf buffer, and the number of 
    bytes to received.
    \return reason see \ref bcm2835I2CReasonCodes
  */
  extern uint8_t bcm2835_i2c_read(char *buf, uint32_t len);

  /* Allows reading from I2C slaves that require a repeated start (without 
    any prior stop) to read after the required slave register has been set. 
    For example, the popular MPL3115A2 pressure and temperature sensor. 
    Note that your device must support or require this mode. If your 
    device does not require this mode then the standard combined:
    \sa bcm2835_i2c_write
    \sa bcm2835_i2c_read
    are a better choice.
    Will read from the slave previously set by bcm2835_i2c_setSlaveAddress.
    \param[in] regaddr Buffer containing the slave register you wish 
    to read from.
    \param[in] buf Buffer of bytes to receive.
    \param[in] len Number of bytes in the buf buffer, and the number of
    bytes to received.
    \return reason see \ref bcm2835I2CReasonCodes
  */
  extern uint8_t bcm2835_i2c_read_register_rs(char* regaddr, char* buf,
					      uint32_t len);

  /* Allows sending an arbitrary number of bytes to I2C slaves
    before issuing a repeated start (with no prior stop) and reading
    a response. Necessary for devices that require such behavior,
    such as the MLX90620. Will write to and read from the slave
    previously set by bcm2835_i2c_setSlaveAddress.

    \param[in] cmds Buffer containing the bytes to send before the 
    repeated start condition.
    \param[in] cmds_len Number of bytes to send from cmds buffer
    \param[in] buf Buffer of bytes to receive.
    \param[in] buf_len Number of bytes to receive in the buf buffer.
    \return reason see \ref bcm2835I2CReasonCodes
  */
  extern uint8_t bcm2835_i2c_write_read_rs(char* cmds,
					   uint32_t cmds_len,
					   char* buf,
					   uint32_t buf_len);

#endif  // DISABLE_I2C

#ifndef DISABLE_SPI

  /* Start SPI operations.
    Forces RPi SPI0 pins P1-19 (MOSI), P1-21 (MISO), P1-23 (CLK), 
    P1-24 (CE0) and P1-26 (CE1) to alternate function ALT0, which 
    enables those pins for SPI interface. You should call 
    bcm2835_spi_end() when all SPI funcitons are complete to 
    return the pins to their default functions.
    \sa  bcm2835_spi_end()
    \return 1 if successful, 0 otherwise (perhaps because you are not 
    running as root)
  */
  extern int bcm2835_spi_begin(void);

  /* End SPI operations.
    SPI0 pins P1-19 (MOSI), P1-21 (MISO), P1-23 (CLK), P1-24 (CE0) 
    and P1-26 (CE1) are returned to their default INPUT behavior.
  */
  extern void bcm2835_spi_end(void);

  /* Sets the SPI bit order
    Set the bit order to be used for transmit and receive. The 
    bcm2835 SPI0 only supports BCM2835_SPI_BIT_ORDER_MSBFIRST,
    so if you select BCM2835_SPI_BIT_ORDER_LSBFIRST, the bytes will 
    be reversed in software.
    The library defaults to BCM2835_SPI_BIT_ORDER_MSBFIRST.
    \param[in] order The desired bit order, one of 
    BCM2835_SPI_BIT_ORDER_*, see \ref bcm2835SPIBitOrder
  */
  extern void bcm2835_spi_setBitOrder(uint8_t order);

  /* Sets the SPI clock divider and therefore the SPI clock speed. 
    \param[in] divider The desired SPI clock divider, one of 
    BCM2835_SPI_CLOCK_DIVIDER_*, see \ref bcm2835SPIClockDivider
  */
  extern void bcm2835_spi_setClockDivider(uint16_t divider);

  /* Sets the SPI clock divider by converting the speed parameter to
    the equivalent SPI clock divider. (see bcm2835_spi_setClockDivider)
    \param[in] speed_hz The desired SPI clock speed in Hz
  */
 extern void bcm2835_spi_set_speed_hz(uint32_t speed_hz);

  /* Sets the SPI data mode.
     Sets the clock polarity and phase.
     \param[in] mode The desired data mode, one of BCM2835_SPI_MODE*, 
     see \ref bcm2835SPIMode
  */
  extern void bcm2835_spi_setDataMode(uint8_t mode);

  /* Sets the chip select pin(s).
     When an bcm2835_spi_transfer() is made, the selected pin(s) will be 
     asserted during the transfer.
     \param[in] cs Specifies the CS pins(s) that are used to activate 
     the desired slave. 
     One of BCM2835_SPI_CS*, see \ref bcm2835SPIChipSelect
  */
  extern void bcm2835_spi_chipSelect(uint8_t cs);

  /* Sets the chip select pin polarity for a given pin
    When an bcm2835_spi_transfer() occurs, the currently selected chip 
    select pin(s) will be asserted to the value given by active. 
    When transfers are not happening, the chip select pin(s) return to 
    the complement (inactive) value.
    \param[in] cs The chip select pin to affect (e.g. BCM2835_SPI_CS0 -tom7)
    \param[in] active Whether the chip select pin is to be active HIGH.
    (If 0, then low means select, like for /CE setups.)
  */
  extern void bcm2835_spi_setChipSelectPolarity(uint8_t cs, uint8_t active);

  /* Transfers one byte to and from the currently selected SPI slave.
    Asserts the currently selected CS pins (as previously set by 
    bcm2835_spi_chipSelect) during the transfer.
    Clocks the 8 bit value out on MOSI, and simultaneously clocks in 
    data from MISO. 
    Returns the read data byte from the slave.
    Uses polled transfer as per section 10.6.1 of the BCM 2835 ARM 
    Peripherals manual.
    \param[in] value The 8 bit data byte to write to MOSI
    \return The 8 bit byte simultaneously read from MISO
    \sa bcm2835_spi_transfern()
  */
  extern uint8_t bcm2835_spi_transfer(uint8_t value);

  /* Transfers any number of bytes to and from the currently selected 
     SPI slave.
     Asserts the currently selected CS pins (as previously set by 
     bcm2835_spi_chipSelect) during the transfer.
    Clocks the len 8 bit bytes out on MOSI, and simultaneously clocks in
    data from MISO. 
    The data read from the slave is placed into rbuf. rbuf must be 
    at least len bytes long. Uses polled transfer as per section 10.6.1
    of the BCM 2835 ARM Peripherals manual.
    \param[in] tbuf Buffer of bytes to send. 
    \param[out] rbuf Received bytes will by put in this buffer
    \param[in] len Number of bytes in the tbuf buffer, and the number 
    of bytes to send/received
    \sa bcm2835_spi_transfer()
  */
  extern void bcm2835_spi_transfernb(char* tbuf, char* rbuf, uint32_t len);

  /* Transfers any number of bytes to and from the currently selected SPI
     slave using bcm2835_spi_transfernb.
    The returned data from the slave replaces the transmitted data in 
    the buffer.
    \param[in,out] buf Buffer of bytes to send. Received bytes will 
    replace the contents
    \param[in] len Number of bytes in teh buffer, and the number of 
    bytes to send/received
    \sa bcm2835_spi_transfer()
  */
  extern void bcm2835_spi_transfern(char* buf, uint32_t len);

  /* Transfers any number of bytes to the currently selected SPI slave.
    Asserts the currently selected CS pins (as previously set by 
    bcm2835_spi_chipSelect) during the transfer.
    \param[in] buf Buffer of bytes to send.
    \param[in] len Number of bytes in the buf buffer, and the number 
    of bytes to send.
  */
  extern void bcm2835_spi_writenb(const char* buf, uint32_t len);

  /* Transfers half-word to and from the currently selected SPI slave.
    Asserts the currently selected CS pins (as previously set by 
    bcm2835_spi_chipSelect) during the transfer.
    Clocks the 8 bit value out on MOSI, and simultaneously clocks in 
    data from MISO.
    Returns the read data byte from the slave.
    Uses polled transfer as per section 10.6.1 of the BCM 2835 ARM 
    Peripherals manual
    \param[in] data The 8 bit data byte to write to MOSI
    \sa bcm2835_spi_writenb()
  */
  extern void bcm2835_spi_write(uint16_t data);

  /* Start AUX SPI operations.
    Forces RPi AUX SPI pins P1-36 (MOSI), P1-38 (MISO), P1-40 (CLK) 
    and P1-36 (CE2) to alternate function ALT4, which enables those
    pins for SPI interface.
    \return 1 if successful, 0 otherwise (perhaps because you are 
    not running as root)
  */
  extern int bcm2835_aux_spi_begin(void);

  /* End AUX SPI operations.
     SPI1 pins P1-36 (MOSI), P1-38 (MISO), P1-40 (CLK) and P1-36 (CE2)
     are returned to their default INPUT behaviour.
   */
  extern void bcm2835_aux_spi_end(void);

  /* Sets the AUX SPI clock divider and therefore the AUX SPI clock speed.
    \param[in] divider The desired AUX SPI clock divider.
  */
  extern void bcm2835_aux_spi_setClockDivider(uint16_t divider);

  /*
   * Calculates the input for bcm2835_aux_spi_setClockDivider
   * @param speed_hz A value between BCM2835_AUX_SPI_CLOCK_MIN 
   and BCM2835_AUX_SPI_CLOCK_MAX
   * @return Input for bcm2835_aux_spi_setClockDivider
   */
  extern uint16_t bcm2835_aux_spi_CalcClockDivider(uint32_t speed_hz);

  /* Transfers half-word to and from the AUX SPI slave.
     Asserts the currently selected CS pins during the transfer.
     \param[in] data The 8 bit data byte to write to MOSI
     \return The 8 bit byte simultaneously read from MISO
     \sa bcm2835_spi_transfern()
  */
  extern void bcm2835_aux_spi_write(uint16_t data);

  /* Transfers any number of bytes to the AUX SPI slave.
    Asserts the CE2 pin during the transfer.
    \param[in] buf Buffer of bytes to send.
    \param[in] len Number of bytes in the tbuf buffer, and the number
    of bytes to send.
  */
  extern void bcm2835_aux_spi_writenb(const char *buf, uint32_t len);

  /* Transfers any number of bytes to and from the AUX SPI slave
    using bcm2835_aux_spi_transfernb.
    The returned data from the slave replaces the transmitted data
    in the buffer.
    \param[in,out] buf Buffer of bytes to send. Received bytes will 
    replace the contents.
    \param[in] len Number of bytes in teh buffer, and the number of 
    bytes to send/received.
    \sa bcm2835_aux_spi_transfer()
  */
  extern void bcm2835_aux_spi_transfern(char *buf, uint32_t len);

  /* Transfers any number of bytes to and from the AUX SPI slave.
    Asserts the CE2 pin during the transfer.
    Clocks the len 8 bit bytes out on MOSI, and simultaneously clocks 
    in data from MISO.
    The data read read from the slave is placed into rbuf. rbuf must
    be at least len bytes long
    \param[in] tbuf Buffer of bytes to send.
    \param[out] rbuf Received bytes will by put in this buffer
    \param[in] len Number of bytes in the tbuf buffer, and the number
    of bytes to send/received.
  */
  extern void bcm2835_aux_spi_transfernb(const char *tbuf,
					 char *rbuf, uint32_t len);

  
#endif  // DISABLE_SPI

  
  /* \defgroup st System Timer access
    Allows access to and delays using the System Timer Counter.
  */

  /* Read the System Timer Counter register.
    \return the value read from the System Timer Counter Lower 32 bits register
  */
  extern uint64_t bcm2835_st_read(void);

  /* Delays for the specified number of microseconds with offset.
    \param[in] offset_micros Offset in microseconds
    \param[in] micros Delay in microseconds
  */
  extern void bcm2835_st_delay(uint64_t offset_micros, uint64_t micros);

  /* \defgroup pwm Pulse Width Modulation
    Allows control of 2 independent PWM channels. A limited subset of GPIO pins
    can be connected to one of these 2 channels, allowing PWM control of GPIO pins.
    You have to set the desired pin into a particular Alt Fun to PWM output. See the PWM
    documentation on the Main Page.
  */

  /* Sets the PWM clock divisor, 
    to control the basic PWM pulse widths.
    \param[in] divisor Divides the basic 19.2MHz PWM clock. You can use one of the common
    values BCM2835_PWM_CLOCK_DIVIDER_* in \ref bcm2835PWMClockDivider
  */
  extern void bcm2835_pwm_set_clock(uint32_t divisor);

  /* Sets the mode of the given PWM channel,
    allowing you to control the PWM mode and enable/disable that channel
    \param[in] channel The PWM channel. 0 or 1.
    \param[in] markspace Set true if you want Mark-Space mode. 0 for Balanced mode.
    \param[in] enabled Set true to enable this channel and produce PWM pulses.
  */
  extern void bcm2835_pwm_set_mode(uint8_t channel, uint8_t markspace,
				   uint8_t enabled);

  /* Sets the maximum range of the PWM output.
    The data value can vary between 0 and this range to control PWM output
    \param[in] channel The PWM channel. 0 or 1.
    \param[in] range The maximum value permitted for DATA.
  */
  extern void bcm2835_pwm_set_range(uint8_t channel, uint32_t range);

  /* Sets the PWM pulse ratio to emit to DATA/RANGE, 
    where RANGE is set by bcm2835_pwm_set_range().
    \param[in] channel The PWM channel. 0 or 1.
    \param[in] data Controls the PWM output ratio as a fraction of the range. 
    Can vary from 0 to RANGE.
  */
  extern void bcm2835_pwm_set_data(uint8_t channel, uint32_t data);

#endif /* BCM2835_H */
