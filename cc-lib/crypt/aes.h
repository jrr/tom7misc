#ifndef __AES_H_
#define __AES_H_

#include <cstdint>

template<int KEYBITS>
struct AES {
  static_assert(KEYBITS == 256 ||
		KEYBITS == 192 ||
		KEYBITS == 128, "only these values supported");
  
  // Block length in bytes. Same for all variants.
  static constexpr int BLOCKLEN = 16;

  // Key length in bytes.
  static constexpr int KEYLEN =
    (KEYBITS == 256) ? 32 : (KEYBITS == 192) ? 24 : 16;
  // Expanded key length in bytes.
  static constexpr int EXPKEYLEN =
    (KEYBITS == 256) ? 240 : (KEYBITS == 192) ? 208 : 176;

  // Number of 32-bit words in a key, which is the constant
  // Nk in AES.
  static constexpr int KEY_WORDS =
    (KEYBITS == 256) ? 8 : (KEYBITS == 192) ? 6 : 4;

  // The number of rounds, which is the constant Nr in AES.
  static constexpr int NUM_ROUNDS =
    (KEYBITS == 256) ? 14 : (KEYBITS == 192) ? 12 : 10;
  
  struct Ctx {
    uint8_t round_key[EXPKEYLEN];
    uint8_t iv[BLOCKLEN];
  };

  static void InitCtx(struct Ctx *ctx, const uint8_t *key);

  static void InitCtxIV(struct Ctx *ctx,
			const uint8_t *key, const uint8_t *iv);
  static void Ctx_set_iv(struct Ctx *ctx, const uint8_t *iv);

  // Buffer size is exactly AES_BLOCKLEN bytes.
  // You need only InitCtx as IV is not used in ECB 
  // Note: ECB is considered insecure for most uses
  static void AES_ECB_encrypt(const struct Ctx *ctx, uint8_t *buf);
  static void AES_ECB_decrypt(const struct Ctx *ctx, uint8_t *buf);


  // buffer size MUST be mutile of AES_BLOCKLEN;
  // Suggest https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS7
  //   for padding scheme.
  // NOTES: you need to set IV in ctx via InitCtxIV() or Ctx_set_iv()
  //        no IV should ever be reused with the same key 
  static void AES_CBC_encrypt_buffer(struct Ctx *ctx,
				     uint8_t *buf, uint32_t length);
  static void AES_CBC_decrypt_buffer(struct Ctx *ctx,
				     uint8_t *buf, uint32_t length);


  // Same function for encrypting as for decrypting. 
  // IV is incremented for every block, and used after encryption as
  // XOR-compliment for output.
  // Suggest https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS7
  //   for padding scheme.
  // NOTES: you need to set IV in ctx with InitCtxIV() or Ctx_set_iv()
  //        no IV should ever be reused with the same key 
  static void AES_CTR_xcrypt_buffer(struct Ctx *ctx,
				    uint8_t *buf, uint32_t length);
};

using AES128 = AES<128>;
using AES192 = AES<192>;
using AES256 = AES<256>;

#endif
