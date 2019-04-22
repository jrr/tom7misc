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
  c->md_len = DIGEST_LENGTH;
}

#if 0
// TODO: hash function on string/vector/etc.
unsigned char *SHA256(const unsigned char *d, size_t n, unsigned char *md)
{
    SHA256::Ctx c;
    static unsigned char m[DIGEST_LENGTH];

    if (md == NULL)
        md = m;
    SHA256_Init(&c);
    SHA256_Update(&c, d, n);
    SHA256_Final(md, &c);
    Cleanse(&c, sizeof(c));
    return md;
}
#endif

/*
static string ResultAsString(SHA256::Ctx *ctx) {
  uint8 result[32];
  SHA256::Finalize(ctx, result);
  string r = "0123456789ABCDEF0123456789ABCDEF";
  for (int i = 0; i < 32; i++) r[i] = (char)result[i];
  return r;
}
*/

static std::vector<uint8> ResultAsVector(SHA256::Ctx *ctx) {
  std::vector<uint8> result(32);
  SHA256::Finalize(ctx, result.data());
  return result;
}

std::vector<uint8> SHA256::HashString(const string &s) {
  SHA256::Ctx c;
  SHA256::Init(&c);
  SHA256::Update(&c, (const uint8 *)s.data(), s.size());
  return ResultAsVector(&c);
}

#define DATA_ORDER_IS_BIG_ENDIAN

#define HASH_LONG               uint32
#define HASH_CTX                SHA256::Ctx
#define HASH_CBLOCK             SHA256::SHA_CBLOCK

/*
 * Note that FIPS180-2 discusses "Truncation of the Hash Function Output."
 * default: case below covers for it. It's not clear however if it's
 * permitted to truncate to amount of bytes not divisible by 4. I bet not,
 * but if it is, then default: case shall be extended. For reference.
 * Idea behind separate cases for pre-defined lengths is to let the
 * compiler decide if it's appropriate to unroll small loops.
 */
#define HASH_MAKE_STRING(c,s)   do {    \
        unsigned long ll;               \
        unsigned int  nn;               \
	for (nn=0; nn < SHA256::DIGEST_LENGTH / 4; nn++)	\
	  {   ll=(c)->h[nn]; (void)HOST_l2c(ll,(s));   }	\
        } while (0)

#define HASH_TRANSFORM          SHA256_Transform
#define HASH_FINAL              SHA256_Final
#define HASH_BLOCK_DATA_ORDER   sha256_block_data_order

static void sha256_block_data_order(
    SHA256::Ctx *ctx, const uint8 *data, size_t num);

// ---------------------------------------------------------------
// was #include internal/md32_common.h

/*
 * Copyright 1999-2018 The OpenSSL Project Authors. All Rights Reserved.
 *
 * Licensed under the Apache License 2.0 (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

/*-
 * This is a generic 32 bit "collector" for message digest algorithms.
 * Whenever needed it collects input character stream into chunks of
 * 32 bit values and invokes a block function that performs actual hash
 * calculations.
 *
 * Porting guide.
 *
 * Obligatory macros:
 *
 * DATA_ORDER_IS_BIG_ENDIAN or DATA_ORDER_IS_LITTLE_ENDIAN
 *      this macro defines byte order of input stream.
 * HASH_CBLOCK
 *      size of a unit chunk HASH_BLOCK operates on.
 * HASH_LONG
 *      has to be at least 32 bit wide.
 * HASH_CTX
 *      context structure that at least contains following
 *      members:
 *              typedef struct {
 *                      ...
 *                      HASH_LONG       Nl,Nh;
 *                      either {
 *                      HASH_LONG       data[HASH_LBLOCK];
 *                      unsigned char   data[HASH_CBLOCK];
 *                      };
 *                      unsigned int    num;
 *                      ...
 *                      } HASH_CTX;
 *      data[] vector is expected to be zeroed upon first call to
 *      Update.
 * HASH_TRANSFORM
 *      name of "Transform" function, implemented here.
 * HASH_FINAL
 *      name of "Final" function, implemented here.
 * HASH_BLOCK_DATA_ORDER
 *      name of "block" function capable of treating *unaligned* input
 *      message in original (data) byte order, implemented externally.
 * HASH_MAKE_STRING
 *      macro converting context variables to an ASCII hash string.
 *
 * MD5 example:
 *
 *      #define DATA_ORDER_IS_LITTLE_ENDIAN
 *
 *      #define HASH_LONG               MD5_LONG
 *      #define HASH_CTX                MD5_CTX
 *      #define HASH_CBLOCK             MD5_CBLOCK
 *      #define HASH_TRANSFORM          MD5_Transform
 *      #define HASH_FINAL              MD5_Final
 *      #define HASH_BLOCK_DATA_ORDER   md5_block_data_order
 */

// #include <openssl/crypto.h>

#if !defined(DATA_ORDER_IS_BIG_ENDIAN) && !defined(DATA_ORDER_IS_LITTLE_ENDIAN)
# error "DATA_ORDER must be defined!"
#endif

#ifndef HASH_CBLOCK
# error "HASH_CBLOCK must be defined!"
#endif
#ifndef HASH_LONG
# error "HASH_LONG must be defined!"
#endif
#ifndef HASH_CTX
# error "HASH_CTX must be defined!"
#endif

#ifndef HASH_TRANSFORM
# error "HASH_TRANSFORM must be defined!"
#endif
#ifndef HASH_FINAL
# error "HASH_FINAL must be defined!"
#endif

#ifndef HASH_BLOCK_DATA_ORDER
# error "HASH_BLOCK_DATA_ORDER must be defined!"
#endif

#define ROTATE(a,n)     (((a)<<(n))|(((a)&0xffffffff)>>(32-(n))))

#if defined(DATA_ORDER_IS_BIG_ENDIAN)

# define HOST_c2l(c,l)  (l =(((unsigned long)(*((c)++)))<<24),          \
                         l|=(((unsigned long)(*((c)++)))<<16),          \
                         l|=(((unsigned long)(*((c)++)))<< 8),          \
                         l|=(((unsigned long)(*((c)++)))    )           )
# define HOST_l2c(l,c)  (*((c)++)=(unsigned char)(((l)>>24)&0xff),      \
                         *((c)++)=(unsigned char)(((l)>>16)&0xff),      \
                         *((c)++)=(unsigned char)(((l)>> 8)&0xff),      \
                         *((c)++)=(unsigned char)(((l)    )&0xff),      \
                         l)

#elif defined(DATA_ORDER_IS_LITTLE_ENDIAN)

# define HOST_c2l(c,l)  (l =(((unsigned long)(*((c)++)))    ),          \
                         l|=(((unsigned long)(*((c)++)))<< 8),          \
                         l|=(((unsigned long)(*((c)++)))<<16),          \
                         l|=(((unsigned long)(*((c)++)))<<24)           )
# define HOST_l2c(l,c)  (*((c)++)=(unsigned char)(((l)    )&0xff),      \
                         *((c)++)=(unsigned char)(((l)>> 8)&0xff),      \
                         *((c)++)=(unsigned char)(((l)>>16)&0xff),      \
                         *((c)++)=(unsigned char)(((l)>>24)&0xff),      \
                         l)

#endif

/*
 * Time for some action :-)
 */

int SHA256::Update(HASH_CTX *c, const uint8 *data, size_t len) {
  unsigned char *p;
  HASH_LONG l;
  size_t n;

  if (len == 0)
    return 1;

  l = (c->Nl + (((HASH_LONG) len) << 3)) & 0xffffffffUL;
  if (l < c->Nl)              /* overflow */
    c->Nh++;
  c->Nh += (HASH_LONG) (len >> 29); /* might cause compiler warning on
				     * 16-bit */
  c->Nl = l;

  n = c->num;
  if (n != 0) {
    p = (unsigned char *)c->data;

    if (len >= HASH_CBLOCK || len + n >= HASH_CBLOCK) {
      memcpy(p + n, data, HASH_CBLOCK - n);
      HASH_BLOCK_DATA_ORDER(c, p, 1);
      n = HASH_CBLOCK - n;
      data += n;
      len -= n;
      c->num = 0;
      /*
       * We use memset rather than OPENSSL_cleanse() here deliberately.
       * Using OPENSSL_cleanse() here could be a performance issue. It
       * will get properly cleansed on finalisation so this isn't a
       * security problem.
       */
      memset(p, 0, HASH_CBLOCK); /* keep it zeroed */
    } else {
      memcpy(p + n, data, len);
      c->num += (unsigned int)len;
      return 1;
    }
  }

  n = len / HASH_CBLOCK;
  if (n > 0) {
    HASH_BLOCK_DATA_ORDER(c, data, n);
    n *= HASH_CBLOCK;
    data += n;
    len -= n;
  }

  if (len != 0) {
    p = (unsigned char *)c->data;
    c->num = (unsigned int)len;
    memcpy(p, data, len);
  }
  return 1;
}

void HASH_TRANSFORM(HASH_CTX *c, const unsigned char *data) {
    HASH_BLOCK_DATA_ORDER(c, data, 1);
}

void SHA256::Finalize(Ctx *c, unsigned char *md  ) {
  unsigned char *p = (unsigned char *)c->data;
  size_t n = c->num;

  /* there is always room for one */
  p[n] = 0x80;
  n++;

  if (n > (HASH_CBLOCK - 8)) {
    memset(p + n, 0, HASH_CBLOCK - n);
    n = 0;
    HASH_BLOCK_DATA_ORDER(c, p, 1);
  }
  memset(p + n, 0, HASH_CBLOCK - 8 - n);

  p += HASH_CBLOCK - 8;
#if defined(DATA_ORDER_IS_BIG_ENDIAN)
  (void)HOST_l2c(c->Nh, p);
  (void)HOST_l2c(c->Nl, p);
#elif defined(DATA_ORDER_IS_LITTLE_ENDIAN)
  (void)HOST_l2c(c->Nl, p);
  (void)HOST_l2c(c->Nh, p);
#endif
  p -= HASH_CBLOCK;
  HASH_BLOCK_DATA_ORDER(c, p, 1);
  c->num = 0;
  Cleanse(p, HASH_CBLOCK);

#ifndef HASH_MAKE_STRING
# error "HASH_MAKE_STRING must be defined!"
#else
  HASH_MAKE_STRING(c, md);
#endif
}

#ifndef MD32_REG_T
# if defined(__alpha) || defined(__sparcv9) || defined(__mips)
#  define MD32_REG_T long
/*
 * This comment was originally written for MD5, which is why it
 * discusses A-D. But it basically applies to all 32-bit digests,
 * which is why it was moved to common header file.
 *
 * In case you wonder why A-D are declared as long and not
 * as MD5_LONG. Doing so results in slight performance
 * boost on LP64 architectures. The catch is we don't
 * really care if 32 MSBs of a 64-bit register get polluted
 * with eventual overflows as we *save* only 32 LSBs in
 * *either* case. Now declaring 'em long excuses the compiler
 * from keeping 32 MSBs zeroed resulting in 13% performance
 * improvement under SPARC Solaris7/64 and 5% under AlphaLinux.
 * Well, to be honest it should say that this *prevents*
 * performance degradation.
 */
# else
/*
 * Above is not absolute and there are LP64 compilers that
 * generate better code if MD32_REG_T is defined int. The above
 * pre-processor condition reflects the circumstances under which
 * the conclusion was made and is subject to further extension.
 */
#  define MD32_REG_T int
# endif
#endif

// ---------------------------------------------------------------

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

/*
 * FIPS specification refers to right rotations, while our ROTATE macro
 * is left one. This is why you might notice that rotation coefficients
 * differ from those observed in FIPS document by 32-N...
 */
# define Sigma0(x) (ROTATE((x),30) ^ ROTATE((x),19) ^ ROTATE((x),10))
# define Sigma1(x) (ROTATE((x),26) ^ ROTATE((x),21) ^ ROTATE((x),7))
# define sigma0(x) (ROTATE((x),25) ^ ROTATE((x),14) ^ ((x)>>3))
# define sigma1(x) (ROTATE((x),15) ^ ROTATE((x),13) ^ ((x)>>10))

# define Ch(x, y, z) (((x) & (y)) ^ ((~(x)) & (z)))
# define Maj(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))

static void sha256_block_data_order(SHA256::Ctx *ctx, const uint8 *data,
                                    size_t num) {
  unsigned MD32_REG_T a, b, c, d, e, f, g, h, s0, s1, T1, T2;
  uint32 X[16], l;
  int i;

  while (num--) {

    a = ctx->h[0];
    b = ctx->h[1];
    c = ctx->h[2];
    d = ctx->h[3];
    e = ctx->h[4];
    f = ctx->h[5];
    g = ctx->h[6];
    h = ctx->h[7];

    for (i = 0; i < 16; i++) {
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

    for (; i < 64; i++) {
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
