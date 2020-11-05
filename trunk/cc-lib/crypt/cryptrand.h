
#ifndef _CC_LIB_CRYPT_CRYPTRAND_H
#define _CC_LIB_CRYPT_CRYPTRAND_H

#include <cstdint>

struct CryptRand {
  CryptRand();

  uint8_t Byte();
  uint64_t Word64();
};

#endif
