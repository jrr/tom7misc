#ifndef __MD5_H
#define __MD5_H

#include <stdio.h>
#include <string>

/* hashes are returned as 16-byte
   binary data strings. */
struct MD5 {
  /* initialize MD5. This detects
     byte order and swaps if necessary. Note that
     this is not thread safe, ugh!
  */
  static void Init();

  static std::string Hash(const std::string &);
  /* hashes the remainder of the file */
  static std::string Hashf(FILE *);
  /* converts the input string into lowercase hex ascii */
  static std::string Ascii(const std::string &);

  /* convert from mixed-case ascii to md5.
     true on success */
  static bool UnAscii(const std::string &, std::string &out);
};

#endif
