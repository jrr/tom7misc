#ifndef __MD5_H
#define __MD5_H

#include <stdio.h>
#include <string>
#include <vector>
#include <cstdint>

/* hashes are returned as 16-byte
   binary data strings. */
struct MD5 {
  /* initialize MD5. This detects
     byte order and swaps if necessary. Note that
     this is not thread safe, ugh!
  */
  static void Init();

  static std::string Hash(const std::string &s);
  static std::string Hashv(const std::vector<uint8_t> &v);
  /* hashes the remainder of the file */
  static std::string Hashf(FILE *f);
  /* converts the input string into lowercase hex ascii */
  static std::string Ascii(const std::string &s);

  /* convert from mixed-case ascii to md5.
     true on success */
  static bool UnAscii(const std::string &s, std::string &out);
};

#endif
