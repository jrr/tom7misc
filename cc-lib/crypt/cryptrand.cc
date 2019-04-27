
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
// maybe needs Create() and private implementation.
CryptRand::CryptRand() {}

uint64_t CryptRand::Word64() {
  
  HCRYPTPROV   hCryptProv;
  //  HCRYPTKEY    hOriginalKey;
  //  HCRYPTKEY    hDuplicateKey;
  //  DWORD        dwMode;
  //  BYTE         pbData[16];

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

uint8_t CryptRand::Byte() {
  return Word64() & 0xFF;
}

#else

# error TODO: Implement this for other platforms (e.g. with /dev/random)

#endif

