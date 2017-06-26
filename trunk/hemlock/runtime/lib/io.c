#include "poptypes.h"

int hemrunt_write(int fd, string s) {

  char * chars = s->chars;
  int size = s->size;

  int a;

  do {
    a = write(fd, chars, size);
    if (a < 0) return -1;
    
    size -= a;
    chars += a;

  } while (size);

  return 0;
}
