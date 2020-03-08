#include <stdio.h>
// #include <stdint.h>

typedef unsigned long long int uint64_t;

// Grab the current value of FLAGS (32-bit). Test that the stack
// pointer is restored. Expects 64-bit mode.
int main(int argc, char **argv) {
  uint64_t f = 0x1;
  for (int i = 0; i < 64; i++) {
    if (f == 0x0100 || i == 18) {
      f <<= 1;
      continue;
    }
    printf("Set flag %d = %llu\n", i, f);
    __asm__ ("pushf\n"
             "popq %%rax\n"
             "orq %0, %%rax\n"
             "pushq %%rax\n"
             "popf\n" :
             /* output */ :
             "r"(f) /* input */  :
             "%rax", "cc" /* clobbered */);

    uint64_t flags = 0;
    int osp, nsp;
    __asm__ (// "xorq %%rax, %%rax\n"
             "mov %%esp, %1\n"
             "pushf\n"
             "popq %%rax\n"
             "mov %%esp, %2\n"
             "mov %%rax, %0\n"
             : "=r"(flags), "=r"(osp), "=r"(nsp) /* output */
             : /* input */
             : "%rax" /* clobbered */
             );

    printf("osp: %d, nsp: %d, flags: %llu\n", osp, nsp, flags);
    f <<= 1;
  }
  return 0;
}
