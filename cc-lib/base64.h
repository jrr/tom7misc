#ifndef __BASE64_H
#define __BASE64_H

#include <string>
#include <cstdint>
#include <vector>

struct Base64 {
  static std::string Encode(const std::string &s);
  static std::string EncodeV(const std::vector<uint8_t> &v);
  /* XXX good if it could do error checking */
  static std::string Decode(const std::string &s);
  static std::vector<uint8_t> DecodeV(const std::string &s);

  // All characters in "a-zA-Z0-9+/" as well as =, which is used for padding.
  static bool IsBase64Char(char c);
};

#endif
