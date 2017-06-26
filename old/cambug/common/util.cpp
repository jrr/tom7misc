
#include <stdio.h>
#include <stdarg.h>
// #include <varargs.h>

#include "common/stringhack.h"

string stringf(const char * fmt, ...) {
  static char p[8192];
  va_list ap;
  va_start(ap, fmt);
  (void) _vsnprintf(p, 8191, fmt, ap);
  va_end(ap);
  return string(p);
}
