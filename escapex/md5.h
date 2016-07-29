// XXX move to cc-lib

#ifndef __MD5_H
#define __MD5_H

#include <stdio.h>
#include <string>

/* hashes are returned as 16-byte
   binary data strings. */
struct md5 {
  /* initialize MD5. This detects
     byte order and swaps if necessary */
  static void init ();

  static std::string hash(const std::string &);
  /* hashes the remainder of the file */
  static std::string hashf(FILE *);
  /* converts the input string into lowercase hex ascii */
  static std::string ascii(const std::string &);

  /* convert from mixed-case ascii to md5.
     true on success */
  static bool unascii(const std::string &, std::string &out);
};

#endif
