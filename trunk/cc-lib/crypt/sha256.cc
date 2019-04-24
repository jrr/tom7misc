/*

 * Copyright 2004-2016 The OpenSSL Project Authors. All Rights Reserved.
 *
 * Licensed under the Apache License 2.0 (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

#include "sha256.h"

#include <cstdint>
#include <string.h>
#include <string>
#include <vector>

using uint8 = uint8_t;
using uint32 = uint32_t;

using namespace std;

// Avoid various optimizations that might cause the compiler
// to not actually run memset.
typedef void *(*memset_t)(void *,int,size_t);
static volatile memset_t memset_func = memset;
static void Cleanse(void *v, size_t len) {
  (*memset_func)(v, 0, len);
}

void SHA256::Init(SHA256::Ctx *c) {
  memset(c, 0, sizeof(*c));
  c->h[0] = 0x6a09e667UL;
  c->h[1] = 0xbb67ae85UL;
  c->h[2] = 0x3c6ef372UL;
  c->h[3] = 0xa54ff53aUL;
  c->h[4] = 0x510e527fUL;
  c->h[5] = 0x9b05688cUL;
  c->h[6] = 0x1f83d9abUL;
  c->h[7] = 0x5be0cd19UL;
}

/*
static string ResultAsString(SHA256::Ctx *ctx) {
  uint8 result[32];
  SHA256::Finalize(ctx, result);
  string r = "0123456789ABCDEF0123456789ABCDEF";
  for (int i = 0; i < 32; i++) r[i] = (char)result[i];
  return r;
}
*/

std::vector<uint8> SHA256::FinalVector(SHA256::Ctx *ctx) {
  std::vector<uint8> result(32);
  SHA256::Finalize(ctx, result.data());
  return result;
}

std::vector<uint8> SHA256::HashString(const string &s) {
  SHA256::Ctx c;
  SHA256::Init(&c);
  SHA256::UpdateString(&c, s);
  return FinalVector(&c);
}

void SHA256::UpdateString(Ctx *c, const string &s) {
  SHA256::Update(c, (const uint8 *)s.data(), s.size());
}

static void sha256_block_data_order(
    SHA256::Ctx *ctx, const uint8 *data, size_t num);

#define ROTATE(a,n)     (((a)<<(n))|(((a)&0xffffffff)>>(32-(n))))

#define HOST_c2l(c,l)  (l =(((unsigned long)(*((c)++)))<<24),		\
			l|=(((unsigned long)(*((c)++)))<<16),		\
			l|=(((unsigned long)(*((c)++)))<< 8),		\
			l|=(((unsigned long)(*((c)++)))    ))

#define HOST_l2c(l,c)  (*((c)++)=(unsigned char)(((l)>>24)&0xff),	\
			*((c)++)=(unsigned char)(((l)>>16)&0xff),	\
			*((c)++)=(unsigned char)(((l)>> 8)&0xff),	\
			*((c)++)=(unsigned char)(((l)    )&0xff),	\
			l)

void SHA256::Update(Ctx *c, const uint8 *data, size_t len) {
  unsigned char *p;
  uint32 l;
  size_t n;

  if (len == 0)
    return;

  l = (c->Nl + (((uint32) len) << 3)) & 0xffffffffUL;
  // overflow?
  if (l < c->Nl)
    c->Nh++;
  // might cause compiler warning on 16-bit
  c->Nh += (uint32) (len >> 29);
  c->Nl = l;

  n = c->num;
  if (n != 0) {
    p = (unsigned char *)c->data;

    if (len >= SHA_CBLOCK || len + n >= SHA_CBLOCK) {
      memcpy(p + n, data, SHA_CBLOCK - n);
      sha256_block_data_order(c, p, 1);
      n = SHA_CBLOCK - n;
      data += n;
      len -= n;
      c->num = 0;
      // Keep it zeroed. (Via OpenSSL we avoid cleanse() here; it gets
      // cleansed in finalization.)
      memset(p, 0, SHA_CBLOCK);
    } else {
      memcpy(p + n, data, len);
      c->num += (unsigned int)len;
      return;
    }
  }

  n = len / SHA_CBLOCK;
  if (n > 0) {
    sha256_block_data_order(c, data, n);
    n *= SHA_CBLOCK;
    data += n;
    len -= n;
  }

  if (len != 0) {
    p = (unsigned char *)c->data;
    c->num = (unsigned int)len;
    memcpy(p, data, len);
  }
}

void SHA256::Finalize(Ctx *c, unsigned char *md) {
  unsigned char *p = (unsigned char *)c->data;
  size_t n = c->num;

  /* there is always room for one */
  p[n] = 0x80;
  n++;

  if (n > (SHA_CBLOCK - 8)) {
    memset(p + n, 0, SHA_CBLOCK - n);
    n = 0;
    sha256_block_data_order(c, p, 1);
  }
  memset(p + n, 0, SHA_CBLOCK - 8 - n);

  p += SHA_CBLOCK - 8;

  (void)HOST_l2c(c->Nh, p);
  (void)HOST_l2c(c->Nl, p);

  p -= SHA_CBLOCK;
  sha256_block_data_order(c, p, 1);
  c->num = 0;
  Cleanse(p, SHA_CBLOCK);

  for (int nn = 0; nn < SHA256::DIGEST_LENGTH / 4; nn++) {
    uint32 ll = c->h[nn];
    (void)HOST_l2c(ll, md);
  }
}

static const uint32 K256[64] = {
  0x428a2f98UL, 0x71374491UL, 0xb5c0fbcfUL, 0xe9b5dba5UL,
  0x3956c25bUL, 0x59f111f1UL, 0x923f82a4UL, 0xab1c5ed5UL,
  0xd807aa98UL, 0x12835b01UL, 0x243185beUL, 0x550c7dc3UL,
  0x72be5d74UL, 0x80deb1feUL, 0x9bdc06a7UL, 0xc19bf174UL,
  0xe49b69c1UL, 0xefbe4786UL, 0x0fc19dc6UL, 0x240ca1ccUL,
  0x2de92c6fUL, 0x4a7484aaUL, 0x5cb0a9dcUL, 0x76f988daUL,
  0x983e5152UL, 0xa831c66dUL, 0xb00327c8UL, 0xbf597fc7UL,
  0xc6e00bf3UL, 0xd5a79147UL, 0x06ca6351UL, 0x14292967UL,
  0x27b70a85UL, 0x2e1b2138UL, 0x4d2c6dfcUL, 0x53380d13UL,
  0x650a7354UL, 0x766a0abbUL, 0x81c2c92eUL, 0x92722c85UL,
  0xa2bfe8a1UL, 0xa81a664bUL, 0xc24b8b70UL, 0xc76c51a3UL,
  0xd192e819UL, 0xd6990624UL, 0xf40e3585UL, 0x106aa070UL,
  0x19a4c116UL, 0x1e376c08UL, 0x2748774cUL, 0x34b0bcb5UL,
  0x391c0cb3UL, 0x4ed8aa4aUL, 0x5b9cca4fUL, 0x682e6ff3UL,
  0x748f82eeUL, 0x78a5636fUL, 0x84c87814UL, 0x8cc70208UL,
  0x90befffaUL, 0xa4506cebUL, 0xbef9a3f7UL, 0xc67178f2UL
};

// Note that FIPS specifies a right rightation, but ROTATE here is
// actually left rotation.
#define Sigma0(x) (ROTATE((x),30) ^ ROTATE((x),19) ^ ROTATE((x),10))
#define Sigma1(x) (ROTATE((x),26) ^ ROTATE((x),21) ^ ROTATE((x),7))
#define sigma0(x) (ROTATE((x),25) ^ ROTATE((x),14) ^ ((x)>>3))
#define sigma1(x) (ROTATE((x),15) ^ ROTATE((x),13) ^ ((x)>>10))

#define Ch(x, y, z) (((x) & (y)) ^ ((~(x)) & (z)))
#define Maj(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))

static void sha256_block_data_order(SHA256::Ctx *ctx, const uint8 *data,
                                    size_t num) {
  uint32 s0, s1, T1, T2;
  uint32 X[16], l;

  while (num--) {
    uint32 a = ctx->h[0];
    uint32 b = ctx->h[1];
    uint32 c = ctx->h[2];
    uint32 d = ctx->h[3];
    uint32 e = ctx->h[4];
    uint32 f = ctx->h[5];
    uint32 g = ctx->h[6];
    uint32 h = ctx->h[7];

    for (int i = 0; i < 16; i++) {
      (void)HOST_c2l(data, l);
      T1 = X[i] = l;
      T1 += h + Sigma1(e) + Ch(e, f, g) + K256[i];
      T2 = Sigma0(a) + Maj(a, b, c);
      h = g;
      g = f;
      f = e;
      e = d + T1;
      d = c;
      c = b;
      b = a;
      a = T1 + T2;
    }

    for (int i = 16; i < 64; i++) {
      s0 = X[(i + 1) & 0x0f];
      s0 = sigma0(s0);
      s1 = X[(i + 14) & 0x0f];
      s1 = sigma1(s1);

      T1 = X[i & 0xf] += s0 + s1 + X[(i + 9) & 0xf];
      T1 += h + Sigma1(e) + Ch(e, f, g) + K256[i];
      T2 = Sigma0(a) + Maj(a, b, c);
      h = g;
      g = f;
      f = e;
      e = d + T1;
      d = c;
      c = b;
      b = a;
      a = T1 + T2;
    }

    ctx->h[0] += a;
    ctx->h[1] += b;
    ctx->h[2] += c;
    ctx->h[3] += d;
    ctx->h[4] += e;
    ctx->h[5] += f;
    ctx->h[6] += g;
    ctx->h[7] += h;
  }
}


string SHA256::Ascii(const std::vector<uint8> &s) {
  static constexpr char hd[] = "0123456789abcdef";
  string ret;
  size_t sz = s.size();
  ret.reserve(sz * 2);

  for (size_t i = 0; i < sz; i++) {
    ret.push_back(hd[(s[i] >> 4) & 0xF]);
    ret.push_back(hd[ s[i]       & 0xF]);
  }

  return ret;
}

/* XXX doesn't check each char is 0-9a-fA-f */
bool SHA256::UnAscii(const string &s, std::vector<uint8> *out) {
  size_t sz = s.length();
  if (sz != 64) return false;

  out->clear();
  out->reserve(32);

  for (size_t i = 0; i < 32; i++) {
    out->push_back((((s[i * 2] | 4400) % 55) << 4) |
		   ((s[i * 2 + 1] | 4400) % 55));
  }

  return true;
}
