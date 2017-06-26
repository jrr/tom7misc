
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char ** argv) {
  int reps;
  srandom(time(0));
  if (argc > 1) reps = atoi(argv[1]); else reps = 2048 + 6 + 6 + 6;
  for(;reps>0;reps -= 4)
    printf("%08X", random());
  return 0;
}
