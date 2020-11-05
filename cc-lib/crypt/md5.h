// Note! MD5 is not suitable for cryptographic applications; it can be
// attacked IN PRACTICE. Consider SHA256 unless you need to specifically
// be compatible with some legacy MD5 hashes.
//
// GPL.

#ifndef _CC_LIB_CRYPT_MD5_H
#define _CC_LIB_CRYPT_MD5_H

#include <stdio.h>
#include <string>
#include <vector>
#include <cstdint>

// Hashes are returned as 16-byte binary data strings.
struct MD5 {
  // (There used to be Init() here, but it is no longer
  // necessary. You can just delete the call.)
  
  static std::string Hash(const std::string &s);
  static std::string Hashv(const std::vector<uint8_t> &v);
  // Hashes the remainder of the file.
  static std::string Hashf(FILE *f);
  // Converts the input string into lowercase hex ascii.
  static std::string Ascii(const std::string &s);

  // Convert from mixed-case ascii to a 16-byte binary string
  // (inverse of Ascii). Returns true on success.
  static bool UnAscii(const std::string &s, std::string &out);
};

#endif
