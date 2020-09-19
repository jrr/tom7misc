
/* Based on the MD5 implementation from FCEUltra,
   which is GPL:

   "RFC 1321 compliant MD5 implementation,
   by Christophe Devine <devine@cr0.net>;
   this program is licensed under the GPL."

   See ../COPYING for the GPL.

   Tom 7 cleaned it up and provided some
   more conveniences in the interface.

*/

#include "md5.h"

#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

using uint8 = uint8_t;
using uint32 = uint32_t;
using namespace std;


namespace {
struct MD5Context {
  uint32 total[2];
  uint32 state[4];
  uint8 buffer[64];
};
}

#define GET_UINT32(b, i) \
  (( (uint32) (b)[(i) + 3] << 24 )		\
   | ( (uint32) (b)[(i) + 2] << 16 )		\
   | ( (uint32) (b)[(i) + 1] <<  8 )		\
   | ( (uint32) (b)[(i)    ]       ))


#define PUT_UINT32(n, b, i) do {		\
    (b)[(i)    ] = (uint8) ( (n)       );       \
    (b)[(i) + 1] = (uint8) ( (n) >>  8 );       \
    (b)[(i) + 2] = (uint8) ( (n) >> 16 );       \
    (b)[(i) + 3] = (uint8) ( (n) >> 24 );       \
  } while (0)

static void md5_starts(MD5Context *ctx) {
  ctx->total[0] = 0;
  ctx->total[1] = 0;
  ctx->state[0] = 0x67452301;
  ctx->state[1] = 0xEFCDAB89;
  ctx->state[2] = 0x98BADCFE;
  ctx->state[3] = 0x10325476;
}

static void md5_process( MD5Context *ctx, const uint8 *data ) {
  uint32 A, B, C, D, X[16];

   X[0] =  GET_UINT32( data,  0 );
   X[1] =  GET_UINT32( data,  4 );
   X[2] =  GET_UINT32( data,  8 );
   X[3] =  GET_UINT32( data, 12 );
   X[4] =  GET_UINT32( data, 16 );
   X[5] =  GET_UINT32( data, 20 );
   X[6] =  GET_UINT32( data, 24 );
   X[7] =  GET_UINT32( data, 28 );
   X[8] =  GET_UINT32( data, 32 );
   X[9] =  GET_UINT32( data, 36 );
   X[10] = GET_UINT32( data, 40 );
   X[11] = GET_UINT32( data, 44 );
   X[12] = GET_UINT32( data, 48 );
   X[13] = GET_UINT32( data, 52 );
   X[14] = GET_UINT32( data, 56 );
   X[15] = GET_UINT32( data, 60 );

#define S(x,n) ((x << n) | ((x & 0xFFFFFFFF) >> (32 - n)))

#define P(a, b, c, d, k, s, t) do {			\
     a += F(b,c,d) + X[k] + t; a = S(a,s) + b;		\
   } while (0)

  A = ctx->state[0];
  B = ctx->state[1];
  C = ctx->state[2];
  D = ctx->state[3];

#define F(x,y,z) (z ^ (x & (y ^ z)))

  P( A, B, C, D,  0,  7, 0xD76AA478 );
  P( D, A, B, C,  1, 12, 0xE8C7B756 );
  P( C, D, A, B,  2, 17, 0x242070DB );
  P( B, C, D, A,  3, 22, 0xC1BDCEEE );
  P( A, B, C, D,  4,  7, 0xF57C0FAF );
  P( D, A, B, C,  5, 12, 0x4787C62A );
  P( C, D, A, B,  6, 17, 0xA8304613 );
  P( B, C, D, A,  7, 22, 0xFD469501 );
  P( A, B, C, D,  8,  7, 0x698098D8 );
  P( D, A, B, C,  9, 12, 0x8B44F7AF );
  P( C, D, A, B, 10, 17, 0xFFFF5BB1 );
  P( B, C, D, A, 11, 22, 0x895CD7BE );
  P( A, B, C, D, 12,  7, 0x6B901122 );
  P( D, A, B, C, 13, 12, 0xFD987193 );
  P( C, D, A, B, 14, 17, 0xA679438E );
  P( B, C, D, A, 15, 22, 0x49B40821 );

#undef F

#define F(x,y,z) (y ^ (z & (x ^ y)))

  P( A, B, C, D,  1,  5, 0xF61E2562 );
  P( D, A, B, C,  6,  9, 0xC040B340 );
  P( C, D, A, B, 11, 14, 0x265E5A51 );
  P( B, C, D, A,  0, 20, 0xE9B6C7AA );
  P( A, B, C, D,  5,  5, 0xD62F105D );
  P( D, A, B, C, 10,  9, 0x02441453 );
  P( C, D, A, B, 15, 14, 0xD8A1E681 );
  P( B, C, D, A,  4, 20, 0xE7D3FBC8 );
  P( A, B, C, D,  9,  5, 0x21E1CDE6 );
  P( D, A, B, C, 14,  9, 0xC33707D6 );
  P( C, D, A, B,  3, 14, 0xF4D50D87 );
  P( B, C, D, A,  8, 20, 0x455A14ED );
  P( A, B, C, D, 13,  5, 0xA9E3E905 );
  P( D, A, B, C,  2,  9, 0xFCEFA3F8 );
  P( C, D, A, B,  7, 14, 0x676F02D9 );
  P( B, C, D, A, 12, 20, 0x8D2A4C8A );

#undef F

#define F(x,y,z) (x ^ y ^ z)

  P( A, B, C, D,  5,  4, 0xFFFA3942 );
  P( D, A, B, C,  8, 11, 0x8771F681 );
  P( C, D, A, B, 11, 16, 0x6D9D6122 );
  P( B, C, D, A, 14, 23, 0xFDE5380C );
  P( A, B, C, D,  1,  4, 0xA4BEEA44 );
  P( D, A, B, C,  4, 11, 0x4BDECFA9 );
  P( C, D, A, B,  7, 16, 0xF6BB4B60 );
  P( B, C, D, A, 10, 23, 0xBEBFBC70 );
  P( A, B, C, D, 13,  4, 0x289B7EC6 );
  P( D, A, B, C,  0, 11, 0xEAA127FA );
  P( C, D, A, B,  3, 16, 0xD4EF3085 );
  P( B, C, D, A,  6, 23, 0x04881D05 );
  P( A, B, C, D,  9,  4, 0xD9D4D039 );
  P( D, A, B, C, 12, 11, 0xE6DB99E5 );
  P( C, D, A, B, 15, 16, 0x1FA27CF8 );
  P( B, C, D, A,  2, 23, 0xC4AC5665 );

#undef F

#define F(x,y,z) (y ^ (x | ~z))

  P( A, B, C, D,  0,  6, 0xF4292244 );
  P( D, A, B, C,  7, 10, 0x432AFF97 );
  P( C, D, A, B, 14, 15, 0xAB9423A7 );
  P( B, C, D, A,  5, 21, 0xFC93A039 );
  P( A, B, C, D, 12,  6, 0x655B59C3 );
  P( D, A, B, C,  3, 10, 0x8F0CCC92 );
  P( C, D, A, B, 10, 15, 0xFFEFF47D );
  P( B, C, D, A,  1, 21, 0x85845DD1 );
  P( A, B, C, D,  8,  6, 0x6FA87E4F );
  P( D, A, B, C, 15, 10, 0xFE2CE6E0 );
  P( C, D, A, B,  6, 15, 0xA3014314 );
  P( B, C, D, A, 13, 21, 0x4E0811A1 );
  P( A, B, C, D,  4,  6, 0xF7537E82 );
  P( D, A, B, C, 11, 10, 0xBD3AF235 );
  P( C, D, A, B,  2, 15, 0x2AD7D2BB );
  P( B, C, D, A,  9, 21, 0xEB86D391 );

#undef F

  ctx->state[0] += A;
  ctx->state[1] += B;
  ctx->state[2] += C;
  ctx->state[3] += D;
}

static void md5_update( MD5Context *ctx, const uint8 *input, uint32 length ) {
  if (!length) return;

  uint32 left = ( ctx->total[0] >> 3 ) & 0x3F;
  uint32 fill = 64 - left;

  ctx->total[0] += length <<  3;
  ctx->total[1] += length >> 29;

  ctx->total[0] &= 0xFFFFFFFF;
  ctx->total[1] += ctx->total[0] < ( length << 3 );

  if( left && length >= fill ) {
    memcpy( (void *) (ctx->buffer + left), (void *) input, fill );
    md5_process( ctx, ctx->buffer );
    length -= fill;
    input  += fill;
    left = 0;
  }

  while( length >= 64 ) {
    md5_process( ctx, input );
    length -= 64;
    input  += 64;
  }

  if( length ) {
    memcpy( (void *) (ctx->buffer + left), (void *) input, length );
  }
}

static constexpr uint8 md5_padding[64] = {
 0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

static void md5_finish(MD5Context *ctx, uint8 *digest) {
  uint8 msglen[8];

  PUT_UINT32( ctx->total[0], msglen, 0 );
  PUT_UINT32( ctx->total[1], msglen, 4 );

  uint32 last = ( ctx->total[0] >> 3 ) & 0x3F;
  uint32 padn = ( last < 56 ) ? ( 56 - last ) : ( 120 - last );

  md5_update( ctx, md5_padding, padn );
  md5_update( ctx, msglen, 8 );

  PUT_UINT32( ctx->state[0], digest,  0 );
  PUT_UINT32( ctx->state[1], digest,  4 );
  PUT_UINT32( ctx->state[2], digest,  8 );
  PUT_UINT32( ctx->state[3], digest, 12 );
}

#if 0
/*
 * Update context to reflect the concatenation of another buffer full
 * of bytes.
 */
// OLD DELETEME
static void MD5Update(struct MD5Context *ctx, const uint8 *buf,
                      unsigned len);
static void MD5Final(uint8 digest[16], struct MD5Context *ctx);
#endif

static string MakeResultString(struct MD5Context *ctx) {
  string ret(16, ' ');
  md5_finish(ctx, (uint8*)ret.data());
  return ret;
}

string MD5::Hash(const string &in) {
  MD5Context ctx;
  md5_starts(&ctx);
  md5_update(&ctx, (const uint8 *)in.data(), in.length());
  return MakeResultString(&ctx);
}

string MD5::Hashv(const vector<uint8> &v) {
  MD5Context ctx;
  md5_starts(&ctx);
  md5_update(&ctx, (const uint8*)v.data(), v.size());
  return MakeResultString(&ctx);
}

// XXX/PERF: Maybe use Util::readfile (generally much faster)
// and do this in memory; also would remove the stdio dependency?
string MD5::Hashf(FILE *f) {
  MD5Context ctx;
  md5_starts(&ctx);

  char buf[256];
  int x = 0;
  do {
    /* XXX doesn't distinguish error from EOF, but... */
    x = fread(buf, 1, 256, f);
    if (x) md5_update(&ctx, (const uint8 *)buf, x);
  } while (x == 256);

  return MakeResultString(&ctx);
}

string MD5::Ascii(const string &s) {
  static constexpr char hd[] = "0123456789abcdef";
  /* XX require specific length? */
  size_t sz = s.length();

  string out(sz * 2, '*');

  for (size_t i = 0; i < sz; i++) {
    out[i * 2]     = hd[(s[i] >> 4) & 0xF];
    out[i * 2 + 1] = hd[ s[i]       & 0xF];
  }

  return out;
}

/* XXX doesn't check each char is 0-9a-fA-f */
bool MD5::UnAscii(const string &s, string &out) {
  if (&s == &out) {
    // Doesn't work if s and out are the same object, so
    // make a copy and recurse.
    string cpy = s;
    return UnAscii(cpy, out);
  } else {
    size_t sz = s.length();

    if (sz != 32) return false;

    out = "0123456789abcdef";

    for (size_t i = 0; i < 16; i++) {
      out[i] = 
        (((s[i * 2] | 4400) % 55) << 4) |
        ((s[i * 2 + 1] | 4400) % 55);
    }

    return true;
  }
}
