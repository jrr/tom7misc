#include <stdio.h>
#include <cstdint>

#include "base/logging.h"
#include "cryptrand.h"

using uint64 = uint64_t;

int main(int argc, char **argv) {
  CryptRand cr;
  uint64 w = cr.Word64();
  printf("%llx\n", w);
  return 0;
}
