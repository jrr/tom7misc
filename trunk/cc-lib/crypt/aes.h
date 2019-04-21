#ifndef __AES_H_
#define __AES_H_

#include <cstdint>


  #define AES128 1
  //#define AES192 1
  //#define AES256 1

  // Block length in bytes AES is 128b block only
  #define AES_BLOCKLEN 16

  #if defined(AES256) && (AES256 == 1)
      #define AES_KEYLEN 32
      #define AES_keyExpSize 240
  #elif defined(AES192) && (AES192 == 1)
      #define AES_KEYLEN 24
      #define AES_keyExpSize 208
  #else
      #define AES_KEYLEN 16   // Key length in bytes
      #define AES_keyExpSize 176
  #endif


  struct AES_ctx
  {
    uint8_t RoundKey[AES_keyExpSize];
    uint8_t Iv[AES_BLOCKLEN];
  };

struct AES {

  static void AES_init_ctx(struct AES_ctx* ctx, const uint8_t* key);

  static void AES_init_ctx_iv(struct AES_ctx* ctx, const uint8_t* key, const uint8_t* iv);
  static void AES_ctx_set_iv(struct AES_ctx* ctx, const uint8_t* iv);

  // buffer size is exactly AES_BLOCKLEN bytes; 
  // you need only AES_init_ctx as IV is not used in ECB 
  // NB: ECB is considered insecure for most uses
  static void AES_ECB_encrypt(const struct AES_ctx* ctx, uint8_t* buf);
  static void AES_ECB_decrypt(const struct AES_ctx* ctx, uint8_t* buf);


  // buffer size MUST be mutile of AES_BLOCKLEN;
  // Suggest https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS7 for padding scheme
  // NOTES: you need to set IV in ctx via AES_init_ctx_iv() or AES_ctx_set_iv()
  //        no IV should ever be reused with the same key 
  static void AES_CBC_encrypt_buffer(struct AES_ctx* ctx, uint8_t* buf, uint32_t length);
  static void AES_CBC_decrypt_buffer(struct AES_ctx* ctx, uint8_t* buf, uint32_t length);


  // Same function for encrypting as for decrypting. 
  // IV is incremented for every block, and used after encryption as XOR-compliment for output
  // Suggesting https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS7 for padding scheme
  // NOTES: you need to set IV in ctx with AES_init_ctx_iv() or AES_ctx_set_iv()
  //        no IV should ever be reused with the same key 
  static void AES_CTR_xcrypt_buffer(struct AES_ctx* ctx, uint8_t* buf, uint32_t length);

};

#endif
