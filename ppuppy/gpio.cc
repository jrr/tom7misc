// Benchmark direct gpio register access.
// Based on "How to access GPIO registers from C-code on the Raspberry-Pi"
// example program, by Dom and Gert,
//  15-January-2012
//  Revised: 15-Feb-2013


#define BCM2708_PERI_BASE 0x20000000
#define GPIO_BASE (BCM2708_PERI_BASE + 0x200000) /* GPIO controller */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

#define PAGE_SIZE (4*1024)
#define BLOCK_SIZE (4*1024)

// I/O access
volatile unsigned *gpio;

// GPIO setup macros. Always use INP_GPIO(x) before using OUT_GPIO(x) or SET_GPIO_ALT(x,y)
#define INP_GPIO(g) *(gpio+((g)/10)) &= ~(7<<(((g)%10)*3))
#define OUT_GPIO(g) *(gpio+((g)/10)) |=  (1<<(((g)%10)*3))
#define SET_GPIO_ALT(g,a) *(gpio+(((g)/10))) |= (((a)<=3?(a)+4:(a)==4?3:2)<<(((g)%10)*3))

#define GPIO_SET *(gpio+7)  // sets   bits which are 1 ignores bits which are 0
#define GPIO_CLR *(gpio+10) // clears bits which are 1 ignores bits which are 0

#define GET_GPIO(g) (*(gpio+13)&(1<<g)) // 0 if LOW, (1<<g) if HIGH

#define GPIO_PULL *(gpio+37) // Pull up/pull down
#define GPIO_PULLCLK0 *(gpio+38) // Pull up/pull down clock

void setup_io();

#define POUT 26

int main(int argc, char **argv) {
  int g,rep;

  // Set up gpi pointer for direct register access
  setup_io();

  // Apparently need to set in before out.
  INP_GPIO(POUT);
  OUT_GPIO(POUT);

  for (;;) {
    usleep(500);
    for (int i = 0; i < 0x7FFFFF; i++) {
      GPIO_SET = 1 << POUT;
      GPIO_CLR = 1 << POUT;
    }
  }

  return 0;
}


void setup_io() {
  int mem_fd = 0;
  void *gpio_map = 0;

  /* open /dev/mem */
  if ((mem_fd = open("/dev/mem", O_RDWR|O_SYNC) ) < 0) {
    printf("can't open /dev/mem \n");
    exit(-1);
  }

  /* mmap GPIO */
  gpio_map =
    mmap(
         NULL,             // Any address in our space will do
         BLOCK_SIZE,       // Map length
         PROT_READ|PROT_WRITE,// Enable reading & writting to mapped memory
         MAP_SHARED,       // Shared with other processes
         mem_fd,           // File to map
         GPIO_BASE         // Offset to GPIO peripheral
         );

  close(mem_fd); // No need to keep mem_fd open after mmap

  if (gpio_map == MAP_FAILED) {
    perror("mmap error: ");
    printf("mmap error %d\n", (int)gpio_map);
    exit(-1);
  }

  // Always use volatile pointer!
  gpio = (volatile unsigned *)gpio_map;
}
