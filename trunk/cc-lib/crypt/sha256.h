/*
  The following code is based on OpenSSL, licensed under the Apache 2.0
  license; see sha256.cc.
*/

#ifndef __SHA256_H
#define __SHA256_H

#include <cstdint>
#include <vector>
#include <string>

struct SHA256 {

  static constexpr int DIGEST_LENGTH = 32;
  
  // SHA-256 treats input data as a contiguous array of 32 bit wide
  // big-endian values.
  static constexpr int SHA_LBLOCK = 16;
  static constexpr int SHA_CBLOCK = 4 * SHA_LBLOCK;

  struct Ctx {
    uint32_t h[8];
    uint32_t Nl, Nh;
    uint32_t data[SHA_LBLOCK];
    unsigned int num;
  };

  static void Init(Ctx *c);
  static void Update(Ctx *c, const uint8_t *data, size_t len);
  // out should point to a 32-byte buffer.
  static void Finalize(Ctx *c, unsigned char *out);

  // Convert from 32-byte digest to lowercase hex string.
  static std::string Ascii(const std::vector<uint8_t> &v);
  // Convert from mixed-case ascii to SHA256 digest. true on success.
  static bool UnAscii(const std::string &s, std::vector<uint8_t> *out);

  // Convenience methods.
  static std::vector<uint8_t> HashString(const std::string &s);

};
  
#endif
