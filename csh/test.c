#include <stdio.h>

// Grab the current value of FLAGS (32-bit). Test that the stack
// pointer is restored. Expects 64-bit mode.
int main(int argc, char **argv) {
  int flags = 0;
  int osp, nsp;
  __asm__ (// "xorq %%rax, %%rax\n"
           "mov %%esp, %1\n"
           "pushf\n"
           "xorq %%rax, %%rax\n"
           "popq %%rax\n"
           "mov %%esp, %2\n"
           "mov %%eax, %0\n"
           : "=r"(flags), "=r"(osp), "=r"(nsp) /* output */
           : /* input */
           : "%rax" /* clobbered */
           );

  printf("osp: %d, nsp: %d, flags: %d\n", osp, nsp, flags);
  return 0;
}
