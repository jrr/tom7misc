
#include "cryptrand.h"
#include "base/logging.h"

// TODO: In the future, std::random_device may be a good way to avoid
// the differences per platform. But note that as of early 2019, this
// is actually a *deterministic* sequence on mingw-x64!

#if defined(__MINGW32__) || defined(__MINGW64__)
#  include <stdio.h>
#  include <windows.h>
#  include <wincrypt.h>

BOOLEAN RtlGenRandom(
  PVOID RandomBuffer,
  ULONG RandomBufferLength
);

// TODO: Other platforms may need to keep state, in which case this
// maybe needs Create() and private implementation. (Even on Windows
// it's dumb for us to keep creating crypto contexts.)
CryptRand::CryptRand() {}

uint64_t CryptRand::Word64() {
  HCRYPTPROV hCryptProv;

  CHECK(CryptAcquireContext(    
            &hCryptProv,
            NULL,
            NULL,
            PROV_RSA_FULL,
            0));

  uint64_t data = 0ULL;
  CHECK(CryptGenRandom(hCryptProv, sizeof (data), (uint8_t*)&data));

  if (hCryptProv) {
    CHECK(CryptReleaseContext(hCryptProv, 0));
  }

  return data;
  
  /*
  uint64_t data;
  CHECK(RtlGenRandom((void*)&data, sizeof (data)));
  return data;
  */
}

#else

CryptRand::CryptRand() {}

uint64_t CryptRand::Word64() {
  FILE *f = fopen("/dev/urandom", "rb");
  CHECK(f) << "/dev/urandom not available?";
  uint64_t data = 0ULL;
  for (int i = 0; i < 8; i++) {
    int c = fgetc(f);
    CHECK(c != EOF);
    data <<= 8;
    data |= (c & 0xFF);
  }
  
  fclose(f);
  return data;
}

#endif

uint8_t CryptRand::Byte() {
  return Word64() & 0xFF;
}
