#ifndef __AES_H_
#define __AES_H_

#include <cstdint>


#define AES128 1
//#define AES192 1
//#define AES256 1


// template<int KEYBITS>
struct AES {
  // Block length in bytes. Same for all variants.
  static constexpr int BLOCKLEN = 16;

  // Key length in bytes.
#if defined(AES256) && (AES256 == 1)
  static constexpr int KEYLEN = 32;
  static constexpr int EXPKEYLEN = 240;
#elif defined(AES192) && (AES192 == 1)
  static constexpr int KEYLEN = 24;
  static constexpr int EXPKEYLEN = 208;
#else
  static constexpr int KEYLEN = 16;
  static constexpr int EXPKEYLEN = 176;
#endif
  
  
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

private:
#if defined(AES256) && (AES256 == 1)
  // The number of 32 bit words in a key.
  static constexpr int KEY_WORDS = 8;
  // The number of rounds in AES Cipher.
  static constexpr int NUM_ROUNDS = 14;
#elif defined(AES192) && (AES192 == 1)
  static constexpr int KEY_WORDS = 6;
  static constexpr int NUM_ROUNDS = 12;
#else
  static constexpr int KEY_WORDS = 4; 
  static constexpr int NUM_ROUNDS = 10;
#endif

};

// using AES128 = AES<128>;
// using AES192 = AES<192>;
// using AES256 = AES<256>;

#endif
