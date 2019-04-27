
#ifndef __CRYPTRAND_H
#define __CRYPTRAND_H

#include <cstdint>

struct CryptRand {
  CryptRand();

  uint8_t Byte();
  uint64_t Word64();
};

#endif
