/* bcm2835.c
// C and C++ support for Broadcom BCM 2835 as used in Raspberry Pi
// http://elinux.org/RPi_Low-level_peripherals
// http://www.raspberrypi.org/wp-content/uploads/2012/02/BCM2835-ARM-Peripherals.pdf
//
// Author: Mike McCauley
// Copyright (C) 2011-2013 Mike McCauley
// Modified by tom7 in 2018+
*/


#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>

#define BCK2835_LIBRARY_BUILD
#include "bcm2835.h"

/* Physical address and size of the peripherals block
// May be overridden on RPi2
*/
uint32_t *bcm2835_peripherals_base = (uint32_t *)BCM2835_PERI_BASE;
uint32_t bcm2835_peripherals_size = BCM2835_PERI_SIZE;

/* Virtual memory address of the mapped peripherals block 
 */
uint32_t *bcm2835_peripherals = (uint32_t *)MAP_FAILED;

/* And the register bases within the peripherals block
 */
volatile uint32_t *bcm2835_gpio        = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_pwm         = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_clk         = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_pads        = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_spi0        = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_bsc0        = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_bsc1        = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_st	       = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_aux	       = (uint32_t *)MAP_FAILED;
volatile uint32_t *bcm2835_spi1        = (uint32_t *)MAP_FAILED;


/* This variable allows us to test on hardware other than RPi.
// It prevents access to the kernel memory, and does not do any peripheral access
// Instead it prints out what it _would_ do if debug were 0
*/
// tom7 disabled this at compile time; it hurts performance
static constexpr uint8_t debug = 0;

/*
// Low level register access functions
*/

/* Function to return the pointers to the hardware register bases */
uint32_t* bcm2835_regbase(uint8_t regbase)
{
    switch (regbase)
    {
	case BCM2835_REGBASE_ST:
	    return (uint32_t *)bcm2835_st;
	case BCM2835_REGBASE_GPIO:
	    return (uint32_t *)bcm2835_gpio;
	case BCM2835_REGBASE_PWM:
	    return (uint32_t *)bcm2835_pwm;
	case BCM2835_REGBASE_CLK:
	    return (uint32_t *)bcm2835_clk;
	case BCM2835_REGBASE_PADS:
	    return (uint32_t *)bcm2835_pads;
	case BCM2835_REGBASE_SPI0:
	    return (uint32_t *)bcm2835_spi0;
	case BCM2835_REGBASE_BSC0:
	    return (uint32_t *)bcm2835_bsc0;
	case BCM2835_REGBASE_BSC1:
	    return (uint32_t *)bcm2835_st;
	case BCM2835_REGBASE_AUX:
	    return (uint32_t *)bcm2835_aux;
	case BCM2835_REGBASE_SPI1:
	    return (uint32_t *)bcm2835_spi1;

    }
    return (uint32_t *)MAP_FAILED;
}

void  bcm2835_set_debug(uint8_t d)
{
  if (d) {
    printf("Debugging disabled for performance -tom7\n");
    abort();
  }
  // debug = d;
}

unsigned int bcm2835_version(void) 
{
    return BCM2835_VERSION;
}

/* Read with memory barriers from peripheral
 *
 */
uint32_t bcm2835_peri_read(volatile uint32_t* paddr)
{
    uint32_t ret;
    if (debug)
    {
		printf("bcm2835_peri_read  paddr %p\n", (void *) paddr);
		return 0;
    }
    else
    {
       __sync_synchronize();
       ret = *paddr;
       __sync_synchronize();
       return ret;
    }
}

/* read from peripheral without the read barrier
 * This can only be used if more reads to THE SAME peripheral
 * will follow.  The sequence must terminate with memory barrier
 * before any read or write to another peripheral can occur.
 * The MB can be explicit, or one of the barrier read/write calls.
 */
uint32_t bcm2835_peri_read_nb(volatile uint32_t* paddr)
{
    if (debug)
    {
	printf("bcm2835_peri_read_nb  paddr %p\n", paddr);
	return 0;
    }
    else
    {
	return *paddr;
    }
}

/* Write with memory barriers to peripheral
 */

#if 0   // inlined in header
void bcm2835_peri_write(volatile uint32_t* paddr, uint32_t value)
{
  if (debug)  // XXX
    {
      printf("bcm2835_peri_write paddr %p, value %08X\n", paddr, value);
    }
  else
    {
      __sync_synchronize();
      *paddr = value;
      __sync_synchronize();
    }
}
#endif

/* write to peripheral without the write barrier */
#if 0
void bcm2835_peri_write_nb(volatile uint32_t* paddr, uint32_t value)
{
    if (debug)
    {
	printf("bcm2835_peri_write_nb paddr %p, value %08X\n",
                paddr, value);
    }
    else
    {
	*paddr = value;
    }
}
#endif

/* Set/clear only the bits in value covered by the mask
 * This is not atomic - can be interrupted.
 */
void bcm2835_peri_set_bits(volatile uint32_t* paddr, uint32_t value, uint32_t mask)
{
    uint32_t v = bcm2835_peri_read(paddr);
    v = (v & ~mask) | (value & mask);
    bcm2835_peri_write(paddr, v);
}

/*
// Low level convenience functions
*/

/* Function select
// pin is a BCM2835 GPIO pin number NOT RPi pin number
//      There are 6 control registers, each control the functions of a block
//      of 10 pins.
//      Each control register has 10 sets of 3 bits per GPIO pin:
//
//      000 = GPIO Pin X is an input
//      001 = GPIO Pin X is an output
//      100 = GPIO Pin X takes alternate function 0
//      101 = GPIO Pin X takes alternate function 1
//      110 = GPIO Pin X takes alternate function 2
//      111 = GPIO Pin X takes alternate function 3
//      011 = GPIO Pin X takes alternate function 4
//      010 = GPIO Pin X takes alternate function 5
//
// So the 3 bits for port X are:
//      X / 10 + ((X % 10) * 3)
*/
void bcm2835_gpio_fsel(uint8_t pin, uint8_t mode)
{
    /* Function selects are 10 pins per 32 bit word, 3 bits per pin */
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPFSEL0/4 + (pin/10);
    uint8_t   shift = (pin % 10) * 3;
    uint32_t  mask = BCM2835_GPIO_FSEL_MASK << shift;
    uint32_t  value = mode << shift;
    bcm2835_peri_set_bits(paddr, value, mask);
}

/* Set output pin */
void bcm2835_gpio_set(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPSET0/4 + pin/32;
    uint8_t shift = pin % 32;
    bcm2835_peri_write(paddr, 1 << shift);
}

/* Clear output pin */
void bcm2835_gpio_clr(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPCLR0/4 + pin/32;
    uint8_t shift = pin % 32;
    bcm2835_peri_write(paddr, 1 << shift);
}

/* Set all output pins in the mask */
#if 0  // inlined
void bcm2835_gpio_set_multi(uint32_t mask)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPSET0/4;
    bcm2835_peri_write(paddr, mask);
}
#endif

/* Clear all output pins in the mask */
#if 0 // inlined
void bcm2835_gpio_clr_multi(uint32_t mask)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPCLR0/4;
    bcm2835_peri_write(paddr, mask);
}
#endif

/* Read input pin */
uint8_t bcm2835_gpio_lev(uint8_t pin) {
  volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPLEV0/4 + pin/32;
  uint8_t shift = pin % 32;
  uint32_t value = bcm2835_peri_read(paddr);
  return (value & (1 << shift)) ? HIGH : LOW;
}

uint32_t bcm2835_gpio_lev_multi() {
  volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPLEV0/4;
  return bcm2835_peri_read(paddr);
}

/* See if an event detection bit is set
// Sigh cant support interrupts yet
*/
uint8_t bcm2835_gpio_eds(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPEDS0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = bcm2835_peri_read(paddr);
    return (value & (1 << shift)) ? HIGH : LOW;
}

uint32_t bcm2835_gpio_eds_multi(uint32_t mask)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPEDS0/4;
    uint32_t value = bcm2835_peri_read(paddr);
    return (value & mask);
}

/* Write a 1 to clear the bit in EDS */
void bcm2835_gpio_set_eds(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPEDS0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_write(paddr, value);
}

void bcm2835_gpio_set_eds_multi(uint32_t mask)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPEDS0/4;
    bcm2835_peri_write(paddr, mask);
}

/* Rising edge detect enable */
void bcm2835_gpio_ren(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPREN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, value, value);
}
void bcm2835_gpio_clr_ren(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPREN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, 0, value);
}

/* Falling edge detect enable */
void bcm2835_gpio_fen(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPFEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, value, value);
}
void bcm2835_gpio_clr_fen(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPFEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, 0, value);
}

/* High detect enable */
void bcm2835_gpio_hen(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPHEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, value, value);
}
void bcm2835_gpio_clr_hen(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPHEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, 0, value);
}

/* Low detect enable */
void bcm2835_gpio_len(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPLEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, value, value);
}
void bcm2835_gpio_clr_len(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPLEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, 0, value);
}

/* Async rising edge detect enable */
void bcm2835_gpio_aren(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPAREN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, value, value);
}
void bcm2835_gpio_clr_aren(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPAREN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, 0, value);
}

/* Async falling edge detect enable */
void bcm2835_gpio_afen(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPAFEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, value, value);
}
void bcm2835_gpio_clr_afen(uint8_t pin)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPAFEN0/4 + pin/32;
    uint8_t shift = pin % 32;
    uint32_t value = 1 << shift;
    bcm2835_peri_set_bits(paddr, 0, value);
}

/* Set pullup/down */
void bcm2835_gpio_pud(uint8_t pud)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPPUD/4;
    bcm2835_peri_write(paddr, pud);
}

/* Pullup/down clock
// Clocks the value of pud into the GPIO pin
*/
void bcm2835_gpio_pudclk(uint8_t pin, uint8_t on)
{
    volatile uint32_t* paddr = bcm2835_gpio + BCM2835_GPPUDCLK0/4 + pin/32;
    uint8_t shift = pin % 32;
    bcm2835_peri_write(paddr, (on ? 1 : 0) << shift);
}

/* Read GPIO pad behaviour for groups of GPIOs */
uint32_t bcm2835_gpio_pad(uint8_t group)
{
    if (bcm2835_pads == MAP_FAILED)
      return 0;
  
    volatile uint32_t* paddr = bcm2835_pads + BCM2835_PADS_GPIO_0_27/4 + group;
    return bcm2835_peri_read(paddr);
}

/* Set GPIO pad behaviour for groups of GPIOs
// powerup value for all pads is
// BCM2835_PAD_SLEW_RATE_UNLIMITED | BCM2835_PAD_HYSTERESIS_ENABLED | BCM2835_PAD_DRIVE_8mA
*/
void bcm2835_gpio_set_pad(uint8_t group, uint32_t control)
{
    if (bcm2835_pads == MAP_FAILED)
      return;
  
    volatile uint32_t* paddr = bcm2835_pads + BCM2835_PADS_GPIO_0_27/4 + group;
    bcm2835_peri_write(paddr, control | BCM2835_PAD_PASSWRD);
}

/* Some convenient arduino-like functions
// milliseconds
*/
void bcm2835_delay(unsigned int millis)
{
    struct timespec sleeper;
    
    sleeper.tv_sec  = (time_t)(millis / 1000);
    sleeper.tv_nsec = (long)(millis % 1000) * 1000000;
    nanosleep(&sleeper, NULL);
}

/* microseconds */
void bcm2835_delayMicroseconds(uint64_t micros)
{
    struct timespec t1;
    uint64_t        start;
	
    if (debug)
    {
	/* Cant access sytem timers in debug mode */
	printf("bcm2835_delayMicroseconds %lld\n", (long long int) micros);
	return;
    }

    /* Calling nanosleep() takes at least 100-200 us, so use it for
    // long waits and use a busy wait on the System Timer for the rest.
    */
    start =  bcm2835_st_read();
   
    /* Not allowed to access timer registers (result is not as precise)*/
    if (start==0)
    {
	t1.tv_sec = 0;
	t1.tv_nsec = 1000 * (long)(micros);
	nanosleep(&t1, NULL);
	return;
    }

    if (micros > 450)
    {
	t1.tv_sec = 0;
	t1.tv_nsec = 1000 * (long)(micros - 200);
	nanosleep(&t1, NULL);
    }    
  
    bcm2835_st_delay(start, micros);
}

/*
// Higher level convenience functions
*/

/* Set the state of an output */
void bcm2835_gpio_write(uint8_t pin, uint8_t on)
{
    if (on)
	bcm2835_gpio_set(pin);
    else
	bcm2835_gpio_clr(pin);
}

/* Set the state of a all 32 outputs in the mask to on or off */
void bcm2835_gpio_write_multi(uint32_t mask, uint8_t on)
{
    if (on)
	bcm2835_gpio_set_multi(mask);
    else
	bcm2835_gpio_clr_multi(mask);
}

/* Set the state of a all 32 outputs in the mask to the values in value */
#if 0 // inlined for performance
void bcm2835_gpio_write_mask(uint32_t value, uint32_t mask)
{
    bcm2835_gpio_set_multi(value & mask);
    bcm2835_gpio_clr_multi((~value) & mask);
}
#endif

/* Set the pullup/down resistor for a pin
//
// The GPIO Pull-up/down Clock Registers control the actuation of internal pull-downs on
// the respective GPIO pins. These registers must be used in conjunction with the GPPUD
// register to effect GPIO Pull-up/down changes. The following sequence of events is
// required:
// 1. Write to GPPUD to set the required control signal (i.e. Pull-up or Pull-Down or neither
// to remove the current Pull-up/down)
// 2. Wait 150 cycles ? this provides the required set-up time for the control signal
// 3. Write to GPPUDCLK0/1 to clock the control signal into the GPIO pads you wish to
// modify ? NOTE only the pads which receive a clock will be modified, all others will
// retain their previous state.
// 4. Wait 150 cycles ? this provides the required hold time for the control signal
// 5. Write to GPPUD to remove the control signal
// 6. Write to GPPUDCLK0/1 to remove the clock
//
// RPi has P1-03 and P1-05 with 1k8 pullup resistor
*/
void bcm2835_gpio_set_pud(uint8_t pin, uint8_t pud)
{
    bcm2835_gpio_pud(pud);
    delayMicroseconds(10);
    bcm2835_gpio_pudclk(pin, 1);
    delayMicroseconds(10);
    bcm2835_gpio_pud(BCM2835_GPIO_PUD_OFF);
    bcm2835_gpio_pudclk(pin, 0);
}

/* Read the System Timer Counter (64-bits) */
uint64_t bcm2835_st_read(void)
{
    volatile uint32_t* paddr;
    uint32_t hi, lo;
    uint64_t st;

    if (bcm2835_st == MAP_FAILED)
	return 0;

    paddr = bcm2835_st + BCM2835_ST_CHI/4;
    hi = bcm2835_peri_read(paddr);

    paddr = bcm2835_st + BCM2835_ST_CLO/4;
    lo = bcm2835_peri_read(paddr);
    
    paddr = bcm2835_st + BCM2835_ST_CHI/4;
    st = bcm2835_peri_read(paddr);
    
    /* Test for overflow */
    if (st == hi)
    {
        st <<= 32;
        st += lo;
    }
    else
    {
        st <<= 32;
        paddr = bcm2835_st + BCM2835_ST_CLO/4;
        st += bcm2835_peri_read(paddr);
    }
    return st;
}

/* Delays for the specified number of microseconds with offset */
void bcm2835_st_delay(uint64_t offset_micros, uint64_t micros)
{
    uint64_t compare = offset_micros + micros;

    while(bcm2835_st_read() < compare)
	;
}

/* PWM */

void bcm2835_pwm_set_clock(uint32_t divisor)
{
    if (   bcm2835_clk == MAP_FAILED
        || bcm2835_pwm == MAP_FAILED)
      return; /* bcm2835_init() failed or not root */
  
    /* From Gerts code */
    divisor &= 0xfff;
    /* Stop PWM clock */
    bcm2835_peri_write(bcm2835_clk + BCM2835_PWMCLK_CNTL, BCM2835_PWM_PASSWRD | 0x01);
    bcm2835_delay(110); /* Prevents clock going slow */
    /* Wait for the clock to be not busy */
    while ((bcm2835_peri_read(bcm2835_clk + BCM2835_PWMCLK_CNTL) & 0x80) != 0)
	bcm2835_delay(1); 
    /* set the clock divider and enable PWM clock */
    bcm2835_peri_write(bcm2835_clk + BCM2835_PWMCLK_DIV, BCM2835_PWM_PASSWRD | (divisor << 12));
    bcm2835_peri_write(bcm2835_clk + BCM2835_PWMCLK_CNTL, BCM2835_PWM_PASSWRD | 0x11); /* Source=osc and enable */
}

void bcm2835_pwm_set_mode(uint8_t channel, uint8_t markspace, uint8_t enabled)
{
  if (   bcm2835_clk == MAP_FAILED
       || bcm2835_pwm == MAP_FAILED)
    return; /* bcm2835_init() failed or not root */

  uint32_t control = bcm2835_peri_read(bcm2835_pwm + BCM2835_PWM_CONTROL);

  if (channel == 0)
    {
      if (markspace)
	control |= BCM2835_PWM0_MS_MODE;
      else
	control &= ~BCM2835_PWM0_MS_MODE;
      if (enabled)
	control |= BCM2835_PWM0_ENABLE;
      else
	control &= ~BCM2835_PWM0_ENABLE;
    }
  else if (channel == 1)
    {
      if (markspace)
	control |= BCM2835_PWM1_MS_MODE;
      else
	control &= ~BCM2835_PWM1_MS_MODE;
      if (enabled)
	control |= BCM2835_PWM1_ENABLE;
      else
	control &= ~BCM2835_PWM1_ENABLE;
    }

  /* If you use the barrier here, wierd things happen, and the commands dont work */
  bcm2835_peri_write_nb(bcm2835_pwm + BCM2835_PWM_CONTROL, control);
  /*  bcm2835_peri_write_nb(bcm2835_pwm + BCM2835_PWM_CONTROL, BCM2835_PWM0_ENABLE | BCM2835_PWM1_ENABLE | BCM2835_PWM0_MS_MODE | BCM2835_PWM1_MS_MODE); */

}

void bcm2835_pwm_set_range(uint8_t channel, uint32_t range)
{
  if (   bcm2835_clk == MAP_FAILED
       || bcm2835_pwm == MAP_FAILED)
    return; /* bcm2835_init() failed or not root */

  if (channel == 0)
      bcm2835_peri_write_nb(bcm2835_pwm + BCM2835_PWM0_RANGE, range);
  else if (channel == 1)
      bcm2835_peri_write_nb(bcm2835_pwm + BCM2835_PWM1_RANGE, range);
}

void bcm2835_pwm_set_data(uint8_t channel, uint32_t data)
{
  if (   bcm2835_clk == MAP_FAILED
       || bcm2835_pwm == MAP_FAILED)
    return; /* bcm2835_init() failed or not root */

  if (channel == 0)
      bcm2835_peri_write_nb(bcm2835_pwm + BCM2835_PWM0_DATA, data);
  else if (channel == 1)
      bcm2835_peri_write_nb(bcm2835_pwm + BCM2835_PWM1_DATA, data);
}

/* Allocate page-aligned memory. */
void *malloc_aligned(size_t size)
{
    void *mem;
    errno = posix_memalign(&mem, BCM2835_PAGE_SIZE, size);
    return (errno ? NULL : mem);
}

/* Map 'size' bytes starting at 'off' in file 'fd' to memory.
// Return mapped address on success, MAP_FAILED otherwise.
// On error print message.
*/
static void *mapmem(const char *msg, size_t size, int fd, off_t off)
{
    void *map = mmap(NULL, size, (PROT_READ | PROT_WRITE), MAP_SHARED, fd, off);
    if (map == MAP_FAILED)
	fprintf(stderr, "bcm2835_init: %s mmap failed: %s\n", msg, strerror(errno));
    return map;
}

static void unmapmem(void **pmem, size_t size)
{
    if (*pmem == MAP_FAILED) return;
    munmap(*pmem, size);
    *pmem = MAP_FAILED;
}

/* Initialise this library. */
int bcm2835_init(void)
{
    int  memfd;
    int  ok;
    FILE *fp;

    if (debug) 
    {
        bcm2835_peripherals = (uint32_t*)BCM2835_PERI_BASE;

	bcm2835_pads = bcm2835_peripherals + BCM2835_GPIO_PADS/4;
	bcm2835_clk  = bcm2835_peripherals + BCM2835_CLOCK_BASE/4;
	bcm2835_gpio = bcm2835_peripherals + BCM2835_GPIO_BASE/4;
	bcm2835_pwm  = bcm2835_peripherals + BCM2835_GPIO_PWM/4;
	bcm2835_spi0 = bcm2835_peripherals + BCM2835_SPI0_BASE/4;
	bcm2835_bsc0 = bcm2835_peripherals + BCM2835_BSC0_BASE/4;
	bcm2835_bsc1 = bcm2835_peripherals + BCM2835_BSC1_BASE/4;
	bcm2835_st   = bcm2835_peripherals + BCM2835_ST_BASE/4;
	bcm2835_aux  = bcm2835_peripherals + BCM2835_AUX_BASE/4;
	bcm2835_spi1 = bcm2835_peripherals + BCM2835_SPI1_BASE/4;

	return 1; /* Success */
    }

    /* Figure out the base and size of the peripheral address block
    // using the device-tree. Required for RPi2, optional for RPi 1
    */
    if ((fp = fopen(BMC2835_RPI2_DT_FILENAME , "rb")))
    {
        unsigned char buf[4];
	fseek(fp, BMC2835_RPI2_DT_PERI_BASE_ADDRESS_OFFSET, SEEK_SET);
	if (fread(buf, 1, sizeof(buf), fp) == sizeof(buf))
	    bcm2835_peripherals_base = (uint32_t *)((long)buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3] << 0);
	fseek(fp, BMC2835_RPI2_DT_PERI_SIZE_OFFSET, SEEK_SET);
	if (fread(buf, 1, sizeof(buf), fp) == sizeof(buf))
	  bcm2835_peripherals_size = (buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3] << 0);
	fclose(fp);
    }
    /* else we are prob on RPi 1 with BCM2835, and use the hardwired defaults */

    /* Now get ready to map the peripherals block 
     * If we are not root, try for the new /dev/gpiomem interface and accept
     * the fact that we can only access GPIO
     * else try for the /dev/mem interface and get access to everything
     */
    memfd = -1;
    ok = 0;
    if (geteuid() == 0)
    {
      /* Open the master /dev/mem device */
      if ((memfd = open("/dev/mem", O_RDWR | O_SYNC) ) < 0) 
	{
	  fprintf(stderr, "bcm2835_init: Unable to open /dev/mem: %s\n",
		  strerror(errno)) ;
	  goto exit;
	}
      
      /* Base of the peripherals block is mapped to VM */
      bcm2835_peripherals = (uint32_t *)mapmem("gpio", bcm2835_peripherals_size, memfd, (off_t)bcm2835_peripherals_base);
      if (bcm2835_peripherals == MAP_FAILED) goto exit;
      
      /* Now compute the base addresses of various peripherals, 
      // which are at fixed offsets within the mapped peripherals block
      // Caution: bcm2835_peripherals is uint32_t*, so divide offsets by 4
      */
      bcm2835_gpio = bcm2835_peripherals + BCM2835_GPIO_BASE/4;
      bcm2835_pwm  = bcm2835_peripherals + BCM2835_GPIO_PWM/4;
      bcm2835_clk  = bcm2835_peripherals + BCM2835_CLOCK_BASE/4;
      bcm2835_pads = bcm2835_peripherals + BCM2835_GPIO_PADS/4;
      bcm2835_spi0 = bcm2835_peripherals + BCM2835_SPI0_BASE/4;
      bcm2835_bsc0 = bcm2835_peripherals + BCM2835_BSC0_BASE/4; /* I2C */
      bcm2835_bsc1 = bcm2835_peripherals + BCM2835_BSC1_BASE/4; /* I2C */
      bcm2835_st   = bcm2835_peripherals + BCM2835_ST_BASE/4;
      bcm2835_aux  = bcm2835_peripherals + BCM2835_AUX_BASE/4;
      bcm2835_spi1 = bcm2835_peripherals + BCM2835_SPI1_BASE/4;

      ok = 1;
    }
    else
    {
      /* Not root, try /dev/gpiomem */
      /* Open the master /dev/mem device */
      if ((memfd = open("/dev/gpiomem", O_RDWR | O_SYNC) ) < 0) 
	{
	  fprintf(stderr, "bcm2835_init: Unable to open /dev/gpiomem: %s\n",
		  strerror(errno)) ;
	  goto exit;
	}
      
      /* Base of the peripherals block is mapped to VM */
      bcm2835_peripherals_base = 0;
      bcm2835_peripherals = (uint32_t *)mapmem("gpio", bcm2835_peripherals_size, memfd, (off_t)bcm2835_peripherals_base);
      if (bcm2835_peripherals == MAP_FAILED) goto exit;
      bcm2835_gpio = bcm2835_peripherals;
      ok = 1;
    }

exit:
    if (memfd >= 0)
        close(memfd);

    if (!ok)
	bcm2835_close();

    return ok;
}

/* Close this library and deallocate everything */
int bcm2835_close(void)
{
    if (debug) return 1; /* Success */

    unmapmem((void**) &bcm2835_peripherals, bcm2835_peripherals_size);
    bcm2835_peripherals = (uint32_t *)MAP_FAILED;
    bcm2835_gpio = (uint32_t *)MAP_FAILED;
    bcm2835_pwm  = (uint32_t *)MAP_FAILED;
    bcm2835_clk  = (uint32_t *)MAP_FAILED;
    bcm2835_pads = (uint32_t *)MAP_FAILED;
    bcm2835_spi0 = (uint32_t *)MAP_FAILED;
    bcm2835_bsc0 = (uint32_t *)MAP_FAILED;
    bcm2835_bsc1 = (uint32_t *)MAP_FAILED;
    bcm2835_st   = (uint32_t *)MAP_FAILED;
    bcm2835_aux  = (uint32_t *)MAP_FAILED;
    bcm2835_spi1 = (uint32_t *)MAP_FAILED;
    return 1; /* Success */
}    
